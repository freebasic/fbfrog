''
'' Main module, command line interface
''
'' This is how fbfrog works:
'' 1. CLI
''   * command line arguments/options from fbfrog's command line or @files are
''     parsed into the tk buffer using lexLoadArgs()
''   * the tk buffer is then parsed by hParseArgs(): global options are handled,
''     script options are turned into an AST and their syntax is verified
''   * the script from default.fbfrog is implicitly prepended unless
''     -nodefaultscript was used
''   * frogEvaluateScript(): the script AST is evaluated (by following each code
''     path), producing the list of individual APIs we want to parse, and the
''     command line options for each one
'' 2. API parsing: For each API, ...
''   * we start with an empty tk buffer, CPP is initialized
''   * predefines for that API are inserted into the tk buffer using lexLoadC()
''   * CPP is told about -removedefine options etc.
''   * content of default.h is loaded into the tk buffer using lexLoadC()
''   * Artificial #include statements for the *.h input files (root files) for
''     that API are inserted into the tk buffer using lexLoadC()
''   * CPP runs and preprocesses the content of the tk buffer (cppMain()):
''     directive parsing, macro expansion, #if evaluation, #include handling.
''     #includes statements are expanded internally so that fbfrog gets to see
''     as much of the API as possible, which improves accuracy of things like
''     typedef detection and symbol renaming. The tokens from include files
''     affected by -filterout are marked for removal. Filenames of #include
''     statements from toplevel files are recorded, if they weren't found or
''     their code will be filtered out.
''   * C parser parses the preprocessed constructs in the tk buffer,
''     produces a self-contained AST
''   * Various steps of AST modifications to make the AST FB-friendly
''     (e.g. fixing identifier conflicts, or solving out redundant typedefs)
''   * Constucts marked due to -filterout are dropped
''   * Direct #include statements recorded by the CPP are reinserted at the top
''     of the bindings
'' 3. AST merging:
''   * Explaining VERBLOCKs: VERBLOCK nodes basically are like #if conditionals.
''     They are used to partition the nodes (API declarations) in an AST into
''     API-specific sections. The declarations in the VERBLOCK can belong to one
''     or more APIs. Whichever API(s) the VERBLOCK "covers" is determined by its
''     condition expression(s).
''   * The AST of each API is wrapped in a VERBLOCK node representing that API
''   * All the APIs' ASTs are merged into one final AST, by merging two at a
''     time (astMergeVerblocks()), by recursively finding the longest common
''     substring (LCS). So it basically is a diff'ing algorithm. The final AST
''     will be a sequence of VERBLOCKs (or only one if all APIs were equal).
''   * Finally, the VERBLOCKs are turned into real #if blocks, and their
''     condition expressions are optimized, eliminating common subexpressions
''     and redundant checks (astProcessVerblocks()).
'' 4. Emitting the final AST as FB code (emitFile())
''

#include once "fbfrog.bi"
#include once "file.bi"

namespace frog
	dim shared as integer verbose, windowsms, syntaxonly, fixunsizedarrays, nodefaultscript
	dim shared as string outname, defaultoutname

	dim shared as ASTNODE ptr script
	dim shared as ASTNODE ptr completeverors, fullveror
	dim shared as FROGAPI ptr apis
	dim shared as integer apicount

	dim shared as THASH renametypedefs, renametags, removeprocs, typedefhints

	dim shared renameopt(OPT_RENAMETYPEDEF to OPT_RENAMETAG) as THASH
	dim shared idopt(OPT_REMOVEDEFINE to OPT_NOEXPAND) as THASH

	dim shared as string prefix
end namespace

dim shared api as FROGAPI ptr

'' Find a *.fbfrog or *.h file in fbfrog's include/ dir, its "library" of
'' premade collections of pre-#defines etc. useful when creating bindings.
private function hFindResource( byref filename as string ) as string
	'' 1. Absolute path? Use as-is
	if( pathIsAbsolute( filename ) ) then return filename

	'' 2. Current directory?
	if( fileexists( filename ) ) then return filename

	'' 3. <exepath>/include/fbfrog/?
	const INCLUDEDIR = "include" + PATHDIV + "fbfrog" + PATHDIV
	var dir1 = hExepath( ) + INCLUDEDIR
	var found = dir1 + filename
	if( fileexists( found ) ) then return found

	'' 4. <exepath>/../include/fbfrog/?
	var dir2 = hExepath( ) + ".." + PATHDIV + INCLUDEDIR
	found = dir2 + filename
	if( fileexists( found ) ) then return found

	print "error: could not find '" + filename + "'"
	print "search dirs:"
	print "  <curdir> (" + curdir( ) + ")"
	print "  <exepath>/include/fbfrog (" + dir1 + ")"
	print "  <exepath>/../include/fbfrog (" + pathNormalize( dir2 ) + ")"
	end 1
end function

private sub hPrintHelpAndExit( )
	print "fbfrog 1.5 (" + __DATE_ISO__ + "), FreeBASIC *.bi binding generator"
	print "usage: fbfrog foo.h [options]"
	print "global options:"
	print "  @<file>          Read more command line arguments from a file"
	print "  -o <path/file>   Set output .bi file name, or just the output directory"
	print "  -v               Show verbose/debugging info"
	print "  -nodefaultscript Don't use default.fbfrog implicitly"
	print "  -windowsms       Use Extern ""Windows-MS"" instead of Extern ""Windows"""
	print "  -syntaxonly      Disable semantic checks, translate based on syntax only"
	print "  -fixunsizedarrays  Wrap [] arrays with a #define"
	print "  -renametypedef <oldid> <newid>  Rename a typedef"
	print "  -renametag <oldid> <newid>      Rename a struct/union/enum"
	print "  -removedefine <id>  Don't preserve a certain #define"
	print "  -removeproc <id>    Don't preserve a certain procedure"
	print "  -typedefhint <id>   Mark <id> as typedef, to help parsing of type casts"
	print "  -reservedid <id>    Rename symbols conflicting with this <id>"
	print "  -noexpand <id>      Disable expansion of certain #define"
	print "version-specific commands:"
	print "  -define <id> [<body>]    Add pre-#define"
	print "  -include <file>          Add pre-#include (will be filtered out)"
	print "  -fbfroginclude <file>    Add pre-#include from include/fbfrog/ (will be filtered out)"
	print "  -incdir <path>   Add #include search directory"
	print "  -filterout <filename-pattern>  Don't preserve code from matching #includes"
	print "  -filterin <filename-pattern>   Undo -filterout for matching #includes"
	print "  -inclib <name>           Add an #inclib ""<name>"" statement"
	print "version script logic:"
	print "  -declaredefines (<symbol>)+ [-unchecked]  Exclusive #defines"
	print "  -declareversions <symbol> (<number>)+     Version numbers"
	print "  -declarebool <symbol>                     Single on/off #define"
	print "  -select          (-case <symbol> ...)+ [-caseelse ...] -endselect"
	print "  -select <symbol> (-case <number> ...)+ [-caseelse ...] -endselect"
	print "  -ifdef <symbol> ... [-else ...] -endif"
	end 1
end sub

private function hTurnArgsIntoString( byval argc as integer, byval argv as zstring ptr ptr ) as string
	dim s as string

	'' Even including argv[0] so it's visible in error messages
	'' (specially parsed in hParseArgs())
	for i as integer = 0 to argc-1
		var arg = *argv[i]

		'' If the argument contains special chars (white-space, ", '),
		'' enclose it in quotes as needed for lexLoadArgs().

		'' Contains '?
		if( instr( arg, "'" ) > 0 ) then
			'' Must enclose in "..." and escape included " or \ chars properly.
			'' This also works if " or whitespace are included too.

			'' Insert \\ for \ before inserting \" for ", so \" won't accidentally
			'' be turned into \\".
			arg = strReplace( arg, $"\", $"\\" )
			arg = strReplace( arg, """", $"\""" )
			arg = """" + arg + """"
		'' Contains no ', but " or white-space?
		elseif( instr( arg, any !""" \t\f\r\n\v" ) > 0 ) then
			'' Enclose in '...', so no escaping is needed.
			arg = "'" + arg + "'"
		'' Empty? Represent it as ""
		elseif( len( arg ) = 0 ) then
			arg = """"""
		end if

		if( len( s ) > 0 ) then
			s += " "
		end if
		s += arg
	next

	function = s
end function

private sub hLoadArgsFile _
	( _
		byval x as integer, _
		byref filename as string, _
		byval location as TKLOCATION ptr _
	)

	const MAX_FILES = 1024  '' Arbitrary limit to detect recursion
	static filecount as integer

	if( filecount > MAX_FILES ) then
		tkOops( x, "suspiciously many @file expansions, recursion? (limit=" & MAX_FILES & ")" )
	end if

	filename = hFindResource( filename )

	'' Load the file content at the specified position
	lexLoadArgs( x, sourcebufferFromFile( filename, location ) )
	filecount += 1

end sub

'' Expand @file arguments in the tk buffer
private sub hExpandArgsFiles( )
	var x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_ARGSFILE
			var filename = *tkGetText( x )

			'' Complain if argument was only '@'
			if( len( filename ) = 0 ) then
				tkOopsExpected( x, "file name directly behind @ (no spaces in between)" )
			end if

			'' If the @file argument comes from an @file,
			'' open it relative to the parent @file's dir.
			var location = tkGetLocation( x )
			if( location->source->is_file ) then
				filename = pathAddDiv( pathOnly( *location->source->name ) ) + filename
			end if

			'' Load the file content behind the @file token
			hLoadArgsFile( x + 1, filename, location )

			'' Remove the @file token (now that its location is no
			'' longer referenced), so it doesn't get in the way of
			'' hParseArgs().
			tkRemove( x, x )

			'' Re-check this position in case a new @file token was inserted right here
			x -= 1
		end select

		x += 1
	loop
end sub

private sub hExpectId( byval x as integer )
	tkExpect( x, TK_ID, "(valid symbol name)" )
end sub

private function hIsStringOrId( byval x as integer ) as integer
	function = (tkGet( x ) = TK_STRING) or (tkGet( x ) = TK_ID)
end function

private sub hExpectPath( byval x as integer )
	if( hIsStringOrId( x ) = FALSE ) then
		tkOopsExpected( x, "<path> argument" )
	end if
end sub

private function hPathRelativeToArgsFile( byval x as integer ) as string
	var path = *tkGetText( x )

	'' If the file/dir argument isn't an absolute path, and it came from an
	'' @file, open it relative to the @file's dir.
	if( pathIsAbsolute( path ) = FALSE ) then
		var location = tkGetLocation( x )
		if( location->source->is_file ) then
			path = pathAddDiv( pathOnly( *location->source->name ) ) + path
		end if
	end if

	function = path
end function

declare sub hParseArgs( byref x as integer )

private sub hParseSelectCompound( byref x as integer )
	'' -select
	astAppend( frog.script, astNew( ASTCLASS_SELECT ) )
	var xblockbegin = x
	x += 1

	'' [<symbol>]
	dim as zstring ptr selectsymbol
	if( tkGet( x ) = TK_ID ) then
		selectsymbol = tkGetText( x )
		x += 1
	end if

	'' -case
	if( tkGet( x ) <> OPT_CASE ) then
		tkOopsExpected( x, "-case after the -select" )
	end if

	do
		hParseArgs( x )

		select case( tkGet( x ) )
		case TK_EOF
			tkOops( xblockbegin, "missing -endselect for this" )

		case OPT_CASE
			if( frog.script->tail->class = ASTCLASS_CASEELSE ) then
				tkOops( x, "-case behind -caseelse" )
			end if
			xblockbegin = x
			x += 1

			dim as ASTNODE ptr condition
			if( selectsymbol ) then
				'' <version number>
				if( hIsStringOrId( x ) = FALSE ) then
					tkOopsExpected( x, @"<version number> argument" )
				end if

				'' <symbol> = <versionnumber>
				condition = astNew( ASTCLASS_EQ, astNewTEXT( selectsymbol ), astNewTEXT( tkGetText( x ) ) )
			else
				'' <symbol>
				hExpectId( x )

				'' defined(<symbol>)
				condition = astNew( ASTCLASS_DEFINED, astNewTEXT( tkGetText( x ) ) )
			end if
			var n = astNew( ASTCLASS_CASE )
			n->expr = condition
			astAppend( frog.script, n )
			x += 1

		case OPT_CASEELSE
			if( frog.script->tail->class = ASTCLASS_CASEELSE ) then
				tkOops( x, "-caseelse behind -caseelse" )
			end if
			astAppend( frog.script, astNew( ASTCLASS_CASEELSE ) )
			xblockbegin = x
			x += 1

		case OPT_ENDSELECT
			astAppend( frog.script, astNew( ASTCLASS_ENDSELECT ) )
			x += 1
			exit do

		case else
			tkOopsExpected( x, "-case or -endselect" )
		end select
	loop
end sub

private sub hParseIfDefCompound( byref x as integer )
	'' -ifdef
	var xblockbegin = x
	x += 1

	'' <symbol>
	hExpectId( x )
	'' -ifdef <symbol>  =>  -select -case <symbol>
	astAppend( frog.script, astNew( ASTCLASS_SELECT ) )
	scope
		var n = astNew( ASTCLASS_CASE )
		n->expr = astNew( ASTCLASS_DEFINED, astNewTEXT( tkGetText( x ) ) )
		astAppend( frog.script, n )
	end scope
	x += 1

	do
		hParseArgs( x )

		select case( tkGet( x ) )
		case TK_EOF
			tkOops( xblockbegin, "missing -endif for this" )

		case OPT_ELSE
			if( frog.script->tail->class = ASTCLASS_CASEELSE ) then
				tkOops( x, "-else behind -else" )
			end if
			astAppend( frog.script, astNew( ASTCLASS_CASEELSE ) )
			xblockbegin = x
			x += 1

		case OPT_ENDIF
			astAppend( frog.script, astNew( ASTCLASS_ENDSELECT ) )
			x += 1
			exit do

		case else
			tkOopsExpected( x, iif( tkGet( xblockbegin ) = OPT_ELSE, _
					@"-endif", @"-else or -endif" ) )
		end select
	loop
end sub

private sub hParseOptionWithString _
	( _
		byref x as integer, _
		byval astclass as integer, _
		byval argdescription as zstring ptr _
	)

	x += 1

	if( hIsStringOrId( x ) = FALSE ) then
		tkOopsExpected( x, argdescription )
	end if
	astAppend( frog.script, astNew( astclass, tkGetText( x ) ) )
	x += 1

end sub

private sub hParseArgs( byref x as integer )
	static nestinglevel as integer

	nestinglevel += 1

	while( tkGet( x ) <> TK_EOF )
		var opt = tkGet( x )
		select case as const( opt )
		case OPT_NODEFAULTSCRIPT  : frog.nodefaultscript  = TRUE : x += 1
		case OPT_WINDOWSMS        : frog.windowsms        = TRUE : x += 1
		case OPT_SYNTAXONLY       : frog.syntaxonly       = TRUE : x += 1
		case OPT_FIXUNSIZEDARRAYS : frog.fixunsizedarrays = TRUE : x += 1
		case OPT_V                : frog.verbose          = TRUE : x += 1

		case OPT_O
			x += 1

			'' <path>
			hExpectPath( x )
			frog.outname = hPathRelativeToArgsFile( x )
			x += 1

		case OPT_RENAMETYPEDEF, OPT_RENAMETAG
			x += 1

			'' <oldid>
			hExpectId( x )
			var n = astNew( ASTCLASS_TEXT, tkSpellId( x ) )
			x += 1

			'' <newid>
			hExpectId( x )
			astRenameSymbol( n, tkSpellId( x ) )
			x += 1

			hashAddOverwrite( @frog.renameopt(opt), n->alias, n )

		case OPT_REMOVEDEFINE, OPT_REMOVEPROC, OPT_TYPEDEFHINT, OPT_RESERVEDID, OPT_NOEXPAND
			x += 1

			'' <id>
			hExpectId( x )
			hashAddOverwrite( @frog.idopt(opt), tkSpellId( x ), NULL )
			x += 1

		'' -declaredefines (<symbol>)+
		case OPT_DECLAREDEFINES
			x += 1

			'' (<symbol>)+
			var n = astNew( ASTCLASS_DECLAREDEFINES )
			hExpectId( x )
			do
				astAppend( n, astNewTEXT( tkGetText( x ) ) )
				x += 1
			loop while( tkGet( x ) = TK_ID )

			'' [-unchecked]
			if( tkGet( x ) = OPT_UNCHECKED ) then
				x += 1
				n->attrib or= ASTATTRIB_UNCHECKED
			end if

			astAppend( frog.script, n )

		'' -unchecked
		case OPT_UNCHECKED
			tkOops( x, "-unchecked without preceding -declaredefines" )

		'' -declareversions <symbol> (<string>)+
		case OPT_DECLAREVERSIONS
			x += 1

			'' <symbol>
			hExpectId( x )
			var n = astNew( ASTCLASS_DECLAREVERSIONS, tkGetText( x ) )
			x += 1

			'' (<string>)+
			if( tkGet( x ) <> TK_STRING ) then
				tkOopsExpected( x, "<version number> argument" )
			end if
			do
				astAppend( n, astNewTEXT( tkGetText( x ) ) )
				x += 1
			loop while( tkGet( x ) = TK_STRING )

			astAppend( frog.script, n )

		'' -declarebool <symbol>
		case OPT_DECLAREBOOL
			x += 1

			'' <symbol>
			hExpectId( x )
			astAppend( frog.script, astNew( ASTCLASS_DECLAREBOOL, tkGetText( x ) ) )
			x += 1

		case OPT_SELECT
			hParseSelectCompound( x )

		case OPT_IFDEF
			hParseIfDefCompound( x )

		case OPT_CASE, OPT_CASEELSE, OPT_ENDSELECT, OPT_ELSE, OPT_ENDIF
			if( nestinglevel <= 1 ) then
				select case( tkGet( x ) )
				case OPT_CASE      : tkOops( x, "-case without -select" )
				case OPT_CASEELSE  : tkOops( x, "-caseelse without -select" )
				case OPT_ENDSELECT : tkOops( x, "-endselect without -select" )
				case OPT_ELSE      : tkOops( x, "-else without -ifdef" )
				case else          : tkOops( x, "-endif without -ifdef" )
				end select
			end if
			exit while

		case OPT_INCDIR
			x += 1

			'' <path>
			hExpectPath( x )
			astAppend( frog.script, astNew( ASTCLASS_INCDIR, hPathRelativeToArgsFile( x ) ) )
			x += 1

		'' -filterout <filename-pattern>
		case OPT_FILTEROUT
			hParseOptionWithString( x, ASTCLASS_FILTEROUT, "<filename-pattern> argument" )

		'' -filterin <filename-pattern>
		case OPT_FILTERIN
			hParseOptionWithString( x, ASTCLASS_FILTERIN, "<filename-pattern> argument" )

		'' -inclib <name>
		case OPT_INCLIB
			hParseOptionWithString( x, ASTCLASS_INCLIB, "<name> argument" )

		'' -define <id> [<body>]
		case OPT_DEFINE
			x += 1

			'' <id>
			hExpectId( x )
			'' Produce an object-like #define
			astAppend( frog.script, astNewPPDEFINE( tkGetText( x ) ) )
			x += 1

			'' [<body>]
			if( hIsStringOrId( x ) ) then
				frog.script->tail->expr = astNewTEXT( tkGetText( x ) )
				x += 1
			end if

		'' -include <file>
		case OPT_INCLUDE
			hParseOptionWithString( x, ASTCLASS_PREINCLUDE, "<file> argument" )

		'' -fbfroginclude <file>
		case OPT_FBFROGINCLUDE
			hParseOptionWithString( x, ASTCLASS_FBFROGPREINCLUDE, "<file> argument" )

		case else
			'' *.fbfrog file given (without @)? Treat as @file too
			var filename = *tkGetText( x )
			if( pathExtOnly( filename ) = "fbfrog" ) then
				hLoadArgsFile( x + 1, filename, tkGetLocation( x ) )
				tkRemove( x, x )

				'' Must expand @files again in case the loaded file contained any
				hExpandArgsFiles( )
			else
				'' Input file
				var filename = hPathRelativeToArgsFile( x )
				astAppend( frog.script, astNewTEXT( filename ) )

				'' The first .h file name seen will be used for the final .bi
				if( len( (frog.defaultoutname) ) = 0 ) then
					frog.defaultoutname = pathStripExt( filename ) + ".bi"
				end if

				x += 1
			end if
		end select
	wend

	nestinglevel -= 1
end sub

private function hSkipToEndOfBlock( byval i as ASTNODE ptr ) as ASTNODE ptr
	var level = 0

	do
		select case( i->class )
		case ASTCLASS_SELECT
			level += 1

		case ASTCLASS_CASE, ASTCLASS_CASEELSE
			if( level = 0 ) then
				exit do
			end if

		case ASTCLASS_ENDSELECT
			if( level = 0 ) then
				exit do
			end if
			level -= 1
		end select

		i = i->next
	loop

	function = i
end function

private sub frogAddApi( byval verand as ASTNODE ptr, byval options as ASTNODE ptr )
	assert( astIsVERAND( verand ) )
	var i = frog.apicount
	frog.apicount += 1
	frog.apis = reallocate( frog.apis, frog.apicount * sizeof( *frog.apis ) )
	clear( frog.apis[i], 0, sizeof( (frog.apis[i]) ) )
	with( frog.apis[i] )
		.verand = verand
		.options = options
		hashInit( @.taghash, 4, FALSE )
	end with
end sub

sub apiAddTag( byval tag as ASTNODE ptr )
	if( hashContains( @api->taghash, tag->text, hashHash( tag->text ) ) ) then
		exit sub
	end if
	api->tagcount += 1
	api->tags = reallocate( api->tags, api->tagcount * sizeof( *api->tags ) )
	api->tags[api->tagcount-1] = tag
	hashAddOverwrite( @api->taghash, tag->text, NULL )
end sub

''
'' The script is a linear list of the command line options, for example:
'' (each line is a sibling AST node)
''    select __LIBFOO_VERSION
''    case 1
''    #define VERSION 1
''    case 2
''    #define VERSION 2
''    endselect
''    ifdef UNICODE
''    #define UNICODE
''    else
''    #define ANSI
''    endif
''    #define COMMON
'' We want to follow each possible code path to determine which versions fbfrog
'' should work with and what their options are. The possible code paths for this
'' example are:
''    <conditions>                                <options>
''    __LIBFOO_VERSION=1, defined(UNICODE)     => #define VERSION 1, #define UNICODE, #define COMMON
''    __LIBFOO_VERSION=2, defined(UNICODE)     => #define VERSION 2, #define UNICODE, #define COMMON
''    __LIBFOO_VERSION=1, not defined(UNICODE) => #define VERSION 1, #define ANSI, #define COMMON
''    __LIBFOO_VERSION=2, not defined(UNICODE) => #define VERSION 2, #define ANSI, #define COMMON
''
'' All the evaluation code assumes that the script is valid, especially that
'' if/else/endif and select/case/endselect nodes are properly used.
''
'' In order to evaluate multiple code paths, we start walking at the beginning,
'' and start a recursive call at every condition. The if path of an -ifdef is
'' evaluated by a recursive call, and we then go on to evaluate the else path.
'' Similar for -select's, except that there can be 1..n possible code paths
'' instead of always 2. Each -case code path except the last is evaluated by a
'' recursive call, and then we go on to evaluate the last -case code path.
''
'' Evaluating the first (couple) code path(s) first, and the last code path
'' last, means that they'll be evaluated in the order they appear in the script,
'' and the results will be in the pretty order expected by the user.
''
'' While evaluating, we keep track of the conditions and visited options of each
'' code path. Recursive calls are given the conditions/options so far seen by
'' the parent. This way, common conditions/options from before the last
'' conditional branch are passed into each code path resulting from the
'' conditional branch.
''
private sub frogEvaluateScript _
	( _
		byval start as ASTNODE ptr, _
		byval conditions as ASTNODE ptr, _
		byval options as ASTNODE ptr _
	)

	var i = start
	while( i )

		select case( i->class )
		case ASTCLASS_DECLAREDEFINES, ASTCLASS_DECLAREVERSIONS
			var decl = i
			i = i->next

			var completeveror = astNew( ASTCLASS_VEROR )

			'' Evaluate a separate code path for each #define/version
			var k = decl->head
			do
				dim as ASTNODE ptr condition
				if( decl->class = ASTCLASS_DECLAREDEFINES ) then
					'' defined(<symbol>)
					condition = astNew( ASTCLASS_DEFINED, astNewTEXT( k->text ) )
				else
					'' <symbol> = <versionnumber>
					condition = astNew( ASTCLASS_EQ, astNewTEXT( decl->text ), astClone( k ) )
				end if
				astAppend( completeveror, astClone( condition ) )

				k = k->next
				if( k = NULL ) then
					'' This is the last #define/version, so don't branch
					astAppend( conditions, condition )
					exit do
				end if

				'' Branch for this #define/version
				frogEvaluateScript( i, _
					astNewGROUP( astClone( conditions ), condition ), _
					astClone( options ) )
			loop

			astAppend( frog.completeverors, completeveror )

		case ASTCLASS_DECLAREBOOL
			var symbol = i->text
			i = i->next

			var completeveror = astNew( ASTCLASS_VEROR )

			'' Branch for the true code path
			'' defined(<symbol>)
			var condition = astNew( ASTCLASS_DEFINED, astNewTEXT( symbol ) )
			astAppend( completeveror, astClone( condition ) )
			frogEvaluateScript( i, _
				astNewGROUP( astClone( conditions ), astClone( condition ) ), _
				astClone( options ) )

			'' And follow the false code path here
			'' (not defined(<symbol>))
			condition = astNew( ASTCLASS_NOT, condition )
			astAppend( completeveror, astClone( condition ) )
			astAppend( conditions, condition )

			astAppend( frog.completeverors, completeveror )

		case ASTCLASS_SELECT
			var selectnode = i
			i = i->next

			do
				'' -case
				assert( i->class = ASTCLASS_CASE )
				var condition = i->expr
				i = i->next

				'' Evaluate the first -case whose condition is true
				if( astGroupContains( conditions, condition ) ) then
					exit do
				end if

				'' Condition was false, skip over the -case's body
				var eob = hSkipToEndOfBlock( i )
				select case( eob->class )
				case ASTCLASS_CASEELSE, ASTCLASS_ENDSELECT
					'' Reached -caseelse/-endselect
					i = eob->next
					exit do
				end select

				'' Go to next -case
				i = eob
			loop

		case ASTCLASS_CASE, ASTCLASS_CASEELSE
			'' When reaching a case/else block instead of the corresponding
			'' select, that means we're evaluating the code path of the
			'' previous case code path, and must now step over the
			'' block(s) of the alternate code path(s).
			i = hSkipToEndOfBlock( i->next )
			assert( (i->class = ASTCLASS_CASE) or _
				(i->class = ASTCLASS_CASEELSE) or _
				(i->class = ASTCLASS_ENDSELECT) )

		case ASTCLASS_ENDSELECT
			'' Ignore - nothing to do
			i = i->next

		case else
			astAppend( options, astClone( i ) )
			i = i->next
		end select
	wend

	assert( conditions->class = ASTCLASS_GROUP )
	conditions->class = ASTCLASS_VERAND
	frogAddApi( conditions, options )
end sub

''
'' Add a forward reference for the given tag, and update all references to the
'' tag to point to the new forward typedef.
''
'' We can do this fairly easily by turning the tag node itself into the forward
'' typedef. This way all references will stay valid and will automatically point
'' to the forward decl. It can then be moved to the top of the code.
''
'' In case the tag node represented a tag body in the code, we can insert a new
'' node in the tag's old place. Then the new node will represent the tag body.
'' (since the old tag node is busy being the forward typedef)
''
'' For completeness' sake, we need to create a new tag node anyways, to
'' represent the forward reference (the dtype/subtype of the forward decl),
'' if there is no tag body.
''
private function hAddFwdDecl( byval parent as ASTNODE ptr, byval oldtag as ASTNODE ptr ) as ASTNODE ptr
	var newtag = astCloneNode( oldtag )
	astMoveChildren( newtag, oldtag )

	'' New name for the forward reference
	astSetText( newtag, *newtag->text + "_" )

	'' If there's a body, insert it in front of the old node
	if( oldtag->attrib and ASTATTRIB_BODYDEFINED ) then
		astInsert( parent, newtag, oldtag )

		'' Unlink old node
		astRemove( parent, oldtag )
	end if

	'' Add the old node to the top
	astPrepend( parent, oldtag )

	'' Turn it into the wanted forward typedef
	oldtag->class = ASTCLASS_TYPEDEF
	oldtag->dtype = TYPE_UDT
	oldtag->subtype = newtag
	function = newtag
end function

private sub frogReadApi( )
	''
	'' 1. Preprocessing
	''

	tkInit( )

	cppInit( )

	scope
		'' Pre-#defines are simply inserted at the top of the token
		'' buffer, so that cppMain() parses them like any other #define.

		var i = api->options->head
		while( i )

			select case( i->class )
			case ASTCLASS_PPDEFINE
				dim as string prettyname, s

				prettyname = "pre-#define"
				s = "#define " + *i->text
				if( i->expr ) then
					assert( i->expr->class = ASTCLASS_TEXT )
					s += " " + *i->expr->text
				end if
				s += !"\n"

				var x = tkGetCount( )
				lexLoadC( x, sourcebufferFromZstring( prettyname, s, NULL ) )
				tkSetRemove( x, tkGetCount( ) - 1 )

			case ASTCLASS_INCDIR
				cppAddIncDir( astClone( i ) )

			case ASTCLASS_FILTEROUT, ASTCLASS_FILTERIN
				cppAddFilter( astClone( i ) )

			end select

			i = i->next
		wend
	end scope

	'' Insert the code from fbfrog pre-#includes (default.h etc.)
	'' * behind command line pre-#defines so that default.h can use them
	'' * marked for removal so the code won't be preserved
	scope
		var i = api->options->head
		while( i )
			if( i->class = ASTCLASS_FBFROGPREINCLUDE ) then
				var filename = hFindResource( *i->text )
				var x = tkGetCount( )
				lexLoadC( x, sourcebufferFromFile( filename, NULL ) )
				tkSetRemove( x, tkGetCount( ) - 1 )
			end if
			i = i->next
		wend
	end scope

	'' Add #includes for pre-#includes
	scope
		var i = api->options->head
		while( i )
			if( i->class = ASTCLASS_PREINCLUDE ) then
				var filename = *i->text
				cppAppendIncludeDirective( filename, TKFLAG_PREINCLUDE )
			end if
			i = i->next
		wend
	end scope

	''
	'' Add #include statements for the toplevel file(s) behind current
	'' tokens, but marked with TKFLAG_ROOTFILE to let the CPP know that no
	'' #include search should be done, and that they shouldn't be affected
	'' by -filterout etc.
	''
	'' This way we can re-use the #include handling code to load the
	'' toplevel files. (especially interesting for include guard optimization)
	''
	'' Note: pre-#defines should appear before tokens from root files, such
	'' that the order of -define vs *.h command line arguments doesn't
	'' matter.
	''
	scope
		var count = 0
		var i = api->options->head
		while( i )
			if( i->class = ASTCLASS_TEXT ) then
				cppAppendIncludeDirective( *i->text, TKFLAG_ROOTFILE )
				count += 1
			end if
			i = i->next
		wend
		if( count = 0 ) then
			oops( "no input files" )
		end if
	end scope

	cppMain( )

	'' Grab the direct #include file names collected by the CPP
	var directincludes = cppTakeDirectIncludes( )

	cppEnd( )

	'' Remove CPP directives and EOLs (tokens marked for removal by
	'' cppMain()). Doing this as a separate step allows
	'' * error reports during cppMain() to view the complete input
	'' * cppMain() to reference #define directives based on token position
	''   (to retrieve the bodies for macro expansion) as opposed to having
	''   to load them into AST
	tkApplyRemoves( )

	hMoveDefinesOutOfConstructs( )
	hOnlyFilterOutWholeConstructs( )

	tkTurnCPPTokensIntoCIds( )

	''
	'' C parsing
	''
	cInit( )
	cMain( )
	cEnd( )

	tkEnd( )

	''
	'' Finalize the AST for this API
	''

	'' Add a forward decl for any tags that were used before being defined
	for i as integer = 0 to api->tagcount - 1
		var tag = api->tags[i]
		if( tag->attrib and ASTATTRIB_USEBEFOREDEF ) then
			api->tags[i] = hAddFwdDecl( api->ast, tag )
		end if
	next

	if( api->need_externblock ) then
		'' Add an Extern block, ensuring to preserve the case of global
		'' vars and procedures, and covering the most-used calling convention.
		astWrapInExternBlock( api->ast, _
			iif( api->stdcalls > api->cdecls, _
				ASTATTRIB_STDCALL, ASTATTRIB_CDECL ) )
	end if

	if( frog.syntaxonly = FALSE ) then
		astFixIdsInit( )
		with( frog.idopt(OPT_RESERVEDID) )
			for i as integer = 0 to .room - 1
				var item = .items + i
				if( item->s ) then
					astFixIdsAddReservedId( item->s )
				end if
			next
		end with
		astFixIds( api->ast )
	end if

	astFilterOut( api->ast )

	'' Add "crt/long[double].bi" as direct #includes if the binding uses CLONG[DOUBLE]
	if( api->uses_clongdouble ) then
		astPrepend( directincludes, astNew( ASTCLASS_PPINCLUDE, "crt/longdouble.bi" ) )
	end if
	if( api->uses_clong ) then
		astPrepend( directincludes, astNew( ASTCLASS_PPINCLUDE, "crt/long.bi" ) )
	end if
	if( api->uses_wchar_t ) then
		var wcharbi = astNew( ASTCLASS_PPINCLUDE, "crt/wchar.bi" )
		if( astGroupContains( directincludes, wcharbi ) = FALSE ) then
			astPrepend( directincludes, wcharbi )
		end if
	end if

	'' Prepend the direct #includes (if any), outside the EXTERN block
	astPrependMaybeWithDivider( api->ast, directincludes )

	'' Prepend #inclibs
	scope
		var i = api->options->tail
		while( i )
			if( i->class = ASTCLASS_INCLIB ) then
				astPrependMaybeWithDivider( api->ast, astClone( i ) )
			end if
			i = i->prev
		wend
	end scope

	api->options = NULL
end sub

private function hMakeProgressString( byval position as integer, byval total as integer ) as string
	var sposition = str( position ), stotal = str( total )
	sposition = string( len( stotal ) - len( sposition ), " " ) + sposition
	function = "[" + sposition + "/" + stotal + "]"
end function

private function hMakeCountMessage( byval count as integer, byref noun as string ) as string
	if( count = 1 ) then
		function = "1 " + noun
	else
		function = count & " " + noun + "s"
	end if
end function

sub frogPrint( byref s as string )
	print frog.prefix + s
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	if( __FB_ARGC__ <= 1 ) then
		hPrintHelpAndExit( )
	end if

	sourcebuffersInit( )
	fbkeywordsInit( )
	fbcrtheadersInit( )
	extradatatypesInit( )
	lexInit( )

	for i as integer = lbound( frog.renameopt ) to ubound( frog.renameopt )
		hashInit( @frog.renameopt(i), 3, FALSE )
	next
	for i as integer = lbound( frog.idopt ) to ubound( frog.idopt )
		hashInit( @frog.idopt(i), 3, TRUE )
	next

	tkInit( )
	tkDontReportContext( )

	'' Load all command line arguments into the tk buffer
	lexLoadArgs( 0, sourcebufferFromZstring( "<command line>", _
			hTurnArgsIntoString( __FB_ARGC__, __FB_ARGV__ ), NULL ) )

	'' Load content of @files too
	hExpandArgsFiles( )

	'' Parse the command line arguments, skipping argv[0]. Global options
	'' are added to various frog.* fields, version-specific options are
	'' added to the frog.script list in their original order.
	frog.script = astNewGROUP( )
	hParseArgs( 1 )

	tkEnd( )

	'' Add the implicit default.h pre-#include
	astPrepend( frog.script, astNew( ASTCLASS_FBFROGPREINCLUDE, "default.h" ) )

	if( frog.nodefaultscript = FALSE ) then
		'' Parse default.fbfrog and prepend the options from it to the
		'' script from the command line.
		var userscript = frog.script
		frog.script = astNewGROUP( )
		tkInit( )
		hLoadArgsFile( 0, hFindResource( "default.fbfrog" ), NULL )
		hParseArgs( 0 )
		tkEnd( )
		astAppend( frog.script, userscript )
	end if

	'' Parse the version-specific options ("script"), following each
	'' possible code path, and determine how many and which versions there
	'' are.
	frog.completeverors = astNewGROUP( )
	frogEvaluateScript( frog.script->head, astNewGROUP( ), astNewGROUP( ) )
	assert( frog.apicount > 0 )

	frog.prefix = space( (len( str( frog.apicount ) ) * 2) + 4 )

	'' For each version, parse the input into an AST, using the options for
	'' that version, and then merge the AST with the previous one, so that
	'' finally we get a single AST representing all versions.
	''
	'' Doing the merging here step-by-step vs. collecting all ASTs and then
	'' merging them afterwards: Merging here immediately saves memory, and
	'' also means that the slow merging process for a version happens after
	'' parsing that version. Instead of one single big delay at the end,
	'' there is a small delay at each version.
	dim as ASTNODE ptr final
	for i as integer = 0 to frog.apicount - 1
		api = frog.apis + i

		print hMakeProgressString( i + 1, frog.apicount ) + " " + astDumpPrettyVersion( api->verand )
		frogReadApi( )

		api->ast = astWrapFileInVerblock( astNewVEROR( astClone( api->verand ) ), api->ast )
		if( final = NULL ) then
			final = astNewGROUP( api->ast )
		else
			final = astMergeVerblocks( final, api->ast )
		end if
		frog.fullveror = astNewVEROR( frog.fullveror, api->verand )
		api->verand = NULL
	next
	api = NULL

	'' Turn VERBLOCKs into #ifs etc.
	astProcessVerblocks( final )

	'' Prepend #pragma once
	'' It's always needed, except if the binding is empty: C headers
	'' typically have #include guards, but we don't preserve those.
	assert( final->class = ASTCLASS_GROUP )
	if( final->head ) then
		astPrependMaybeWithDivider( final, astNew( ASTCLASS_PRAGMAONCE ) )
	end if

	astAutoAddDividers( final )

	'' Write out the .bi file
	if( len( (frog.defaultoutname) ) = 0 ) then
		frog.defaultoutname = "unknown.bi"
	end if
	if( len( (frog.outname) ) = 0 ) then
		frog.outname = frog.defaultoutname
	elseif( pathIsDir( frog.outname ) ) then
		frog.outname = pathAddDiv( frog.outname ) + pathStrip( frog.defaultoutname )
	end if
	print "emitting: " + frog.outname;
	emitFile( frog.outname, final )
	print " (" + _
		hMakeCountMessage( emit.decls, "declaration" ) + ", " + _
		hMakeCountMessage( emit.todos, "TODO"        ) + ")"

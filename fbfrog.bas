'' Main module, command line interface

#include once "fbfrog.bi"

dim shared frog as FROGSTUFF

private sub hPrintHelpAndExit( )
	print "fbfrog 1.0 (" + __DATE_ISO__ + "), FreeBASIC *.bi binding generator"
	print "usage: fbfrog *.h [options]"
	print "*.fbfrog input files are special and treated similar to @<file>."
	print "global options:"
	print "  @<file>          Read more command line arguments from that file."
	print "                   (will be expanded in place before other parsing)"
	print "  -nomerge         Don't preserve code from #includes"
	print "  -whitespace      Try to preserve comments and empty lines"
	print "  -noautoextern    Don't add Extern blocks"
	print "  -windowsms       Use Extern ""Windows-MS"" instead of Extern ""Windows"""
	print "  -nonamefixup     Don't fix symbol identifier conflicts which happen"
	print "                   e.g. due to FB's keywords and/or case insensitivity"
	print "  -versiondefine <id>  Set identifier for version #define that may"
	print "                       be used by the generated binding."
	print "  -common          Extract common code into common header"
	print "  -incdir <path>   Add #include search directory"
	print "  -o <path>        Set output directory for generated .bi files"
	print "  -v               Show verbose/debugging info"
	print "commands specific to binding version/target:"
	print "  -version <string>        Begin of version-specific arguments"
	print "                           (ends at next -version or EOL)"
	print "  -target dos|linux|win32  Begin of target-specific arguments (ditto)"
	print "  -define <id> [<body>]    Add pre-#define"
	print "  -undef <id>              Add pre-#undef"
	print "  -include <file>          Add pre-#include"
	print "  -noexpand <id>           Disable expansion of certain #define"
	print "  -removedefine <id>       Don't preserve certain #defines/#undefs"
	print "  -appendbi <file>         Append arbitrary FB code from <file> to the binding"
	print "  -removematch ""<C token(s)>""    Drop constructs containing the given C token(s)."
	print "                               This should be used to work-around parsing errors."
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
		end if

		if( len( s ) > 0 ) then
			s += " "
		end if
		s += arg
	next

	function = s
end function

private sub hLoadResponseFile _
	( _
		byval x as integer, _
		byref filename as string, _
		byval location as TKLOCATION ptr _
	)

	const MAX_FILES = 1024  '' Arbitrary limit to detect recursion
	static filecount as integer

	if( filecount > MAX_FILES ) then
		tkOops( x, "suspiciously many @response file expansions, recursion? (limit=" & MAX_FILES & ")" )
	end if

	'' Load the file content at the specified position
	lexLoadArgs( x, filebufferFromFile( filename, location ) )
	filecount += 1

end sub

'' Expand @file arguments in the tk buffer
private sub hExpandResponseFileArguments( )
	var x = 0
	while( tkGet( x ) <> TK_EOF )

		if( tkGet( x ) = TK_RESPONSEFILE ) then
			var filename = *tkGetText( x )

			'' Complain if argument was only '@'
			if( len( filename ) = 0 ) then
				tkOopsExpected( x, "file name directly behind @ (no spaces in between)" )
			end if

			'' If the @file argument comes from an @file,
			'' open it relative to the parent @file's dir.
			var location = tkGetLocation( x )
			if( location->file->is_file ) then
				filename = pathAddDiv( pathOnly( *location->file->name ) ) + filename
			end if

			'' Load the file content behind the @file token
			hLoadResponseFile( x + 1, filename, location )

			'' Remove the @file token (now that its location is no
			'' longer referenced), so it doesn't get in the way of
			'' hParseArgs().
			tkRemove( x, x )

			'' Re-check this position in case a new @file token was inserted right here
			x -= 1
		end if

		x += 1
	wend
end sub

private sub hLoadBuiltinResponseFile _
	( _
		byval x as integer, _
		byref id as string, _
		byval location as TKLOCATION ptr _
	)

	'' <exepath>/builtin/<id>.fbfrog.
	var builtinfile = hExePath( ) + "builtin" + PATHDIV + id + ".fbfrog"
	hLoadResponseFile( x, builtinfile, location )

	'' Must expand @files again in case the loaded built-in file contained any
	hExpandResponseFileArguments( )

end sub

private sub hExpectId( byval x as integer )
	tkExpect( x, TK_ID, "valid symbol name" )
end sub

private function hIsStringOrId( byval x as integer ) as integer
	function = (tkGet( x ) = TK_STRING) or (tkGet( x ) = TK_ID)
end function

private sub hExpectPath( byval x as integer )
	if( hIsStringOrId( x ) = FALSE ) then
		tkOopsExpected( x, "<path> argument" )
	end if
end sub

private function hPathRelativeToResponseFile( byval x as integer ) as string
	var path = *tkGetText( x )

	'' If the file/dir argument isn't an absolute path, and it came from an
	'' @file, open it relative to the @file's dir.
	if( pathIsAbsolute( path ) = FALSE ) then
		var location = tkGetLocation( x )
		if( location->file->is_file ) then
			path = pathAddDiv( pathOnly( *location->file->name ) ) + path
		end if
	end if

	function = path
end function

private sub hReadFileArg( byval result as ASTNODE ptr, byval x as integer )
	var path = hPathRelativeToResponseFile( x )

	'' File or directory?
	var n = astNew( ASTCLASS_FILE, path )
	if( hReadableDirExists( path ) ) then
		n->class = ASTCLASS_DIR
	end if

	n->location = *tkGetLocation( x )
	astAppend( result, n )
end sub

enum
	BODY_TOPLEVEL = 0
	BODY_VERSION
	BODY_TARGET
end enum

private function hParseArgs( byref x as integer, byval body as integer ) as ASTNODE ptr
	var result = astNewGROUP( )

	if( body = BODY_TOPLEVEL ) then
		'' Skip argv[0]
		assert( tkGet( x ) <> TK_EOF )
		x += 1

		'' Load pre-#defines that are always used
		hLoadBuiltinResponseFile( x, "base", tkGetLocation( x - 1 ) )
	end if

	while( tkGet( x ) <> TK_EOF )
		var text = *tkGetText( x )

		select case( tkGet( x ) )
		case TK_OPTION
			select case( text )
			case "h", "?", "help", "-help", "-version"
				hPrintHelpAndExit( )

			case "nomerge"      : frog.nomerge      = TRUE
			case "whitespace"   : frog.whitespace   = TRUE
			case "noautoextern" : frog.noautoextern = TRUE
			case "windowsms"    : frog.windowsms    = TRUE
			case "nonamefixup"  : frog.nonamefixup  = TRUE
			case "common"       : frog.common = TRUE
			case "v", "verbose", "-verbose" : frog.verbose = TRUE

			case "versiondefine"
				x += 1

				'' <id>
				hExpectId( x )
				frog.versiondefine = *tkGetText( x )

			case "incdir"
				x += 1

				'' <path>
				hExpectPath( x )
				var n = astNewTEXT( hPathRelativeToResponseFile( x ) )
				n->location = *tkGetLocation( x )
				astAppend( frog.incdirs, n )

			case "o"
				x += 1

				'' <path>
				hExpectPath( x )
				frog.outdir = hPathRelativeToResponseFile( x )

			'' -version <version id> ...
			case "version"
				'' Another -version is coming - end any current -version/-target blocks
				if( body <> BODY_TOPLEVEL ) then
					exit while
				end if
				var location1 = tkGetLocation( x )
				x += 1

				'' <version id>
				if( hIsStringOrId( x ) = FALSE ) then
					tkOopsExpected( x, "<version id> argument" )
				end if
				'' astNewCONSTI( vallng( nextarg ), TYPE_LONGINT )
				var id = astNew( ASTCLASS_STRING, tkGetText( x ) )
				var location2 = tkGetLocation( x )
				id->location = *location2
				x += 1

				var location = tkGetLocation( x )
				var n = astNewVERBLOCK( id, NULL, hParseArgs( x, BODY_VERSION ) )
				n->location = *location1
				n->location.length = location2->column + location2->length - location1->column
				astAppend( result, n )
				x -= 1

			'' -target <target-id> ...
			case "target"
				'' Another -target is coming - end any current -target block
				if( body = BODY_TARGET ) then
					exit while
				end if
				var location = *tkGetLocation( x )
				x += 1

				'' <target-id>
				var attrib = 0
				dim as string targetid
				if( tkGet( x ) = TK_ID ) then
					targetid = *tkGetText( x )
					select case( targetid )
					case "dos"   : attrib = ASTATTRIB_DOS
					case "linux" : attrib = ASTATTRIB_LINUX
					case "win32" : attrib = ASTATTRIB_WIN32
					end select
				end if
				if( attrib = 0 ) then
					tkOopsExpected( x, "one of dos|linux|win32" )
				end if
				var location2 = tkGetLocation( x )
				location.length = location2->column + location2->length - location.column
				x += 1

				hLoadBuiltinResponseFile( x, targetid, @location )

				var n = astNew( ASTCLASS_TARGETBLOCK, hParseArgs( x, BODY_TARGET ) )
				n->attrib or= attrib
				n->location = location
				astAppend( result, n )
				x -= 1

			'' -define <id> [<body>]
			case "define"
				x += 1

				'' <id>
				hExpectId( x )
				var n = astNew( ASTCLASS_PPDEFINE, tkGetText( x ) )
				n->location = *tkGetLocation( x )

				'' [<body>]
				if( hIsStringOrId( x + 1 ) ) then
					x += 1
					n->expr = astNewTEXT( tkGetText( x ) )
					n->expr->location = *tkGetLocation( x )
				end if

				astAppend( result, n )

			case "undef"
				x += 1

				'' <id>
				hExpectId( x )
				var n = astNew( ASTCLASS_PPUNDEF, tkGetText( x ) )
				n->location = *tkGetLocation( x )
				astAppend( result, n )

			case "include"
				x += 1

				'' <file>
				hExpectPath( x )
				var n = astNew( ASTCLASS_PPINCLUDE, hPathRelativeToResponseFile( x ) )
				n->location = *tkGetLocation( x )
				astAppend( result, n )

			case "noexpand"
				x += 1

				'' <id>
				hExpectId( x )
				var n = astNew( ASTCLASS_NOEXPAND, tkGetText( x ) )
				n->location = *tkGetLocation( x )
				astAppend( result, n )

			case "removedefine"
				x += 1

				'' <id>
				hExpectId( x )
				var n = astNew( ASTCLASS_REMOVEDEFINE, tkGetText( x ) )
				n->location = *tkGetLocation( x )
				astAppend( result, n )

			case "removematch"
				x += 1

				'' <C tokens>
				if( (tkGet( x ) <> TK_ID) and (tkGet( x ) <> TK_STRING) ) then
					tkOopsExpected( x, "C tokens" )
				end if
				var n = astNew( ASTCLASS_REMOVEMATCH, tkGetText( x ) )
				n->location = *tkGetLocation( x )
				astAppend( result, n )

			case "appendbi"
				x += 1

				'' <file>
				hExpectPath( x )
				var n = astNew( ASTCLASS_APPENDBI, hPathRelativeToResponseFile( x ) )
				n->location = *tkGetLocation( x )
				astAppend( result, n )

			case else
				tkOops( x, "unknown command line option '" + text + "'" )
			end select

		case else
			select case( text )
			case "/?", "/h", "/help"
				hPrintHelpAndExit( )
			case else
				'' *.fbfrog file given (without @)? Treat as @file too
				if( pathExtOnly( text ) = "fbfrog" ) then
					hLoadResponseFile( x + 1, text, tkGetLocation( x ) )
					tkRemove( x, x )
					x -= 1

					'' Must expand @files again in case the loaded file contained any
					hExpandResponseFileArguments( )
				else
					hReadFileArg( result, x )
				end if
			end select
		end select

		x += 1
	wend

	function = result
end function

private sub hLoadPatternTokens( byval n as ASTNODE ptr )
	assert( n->class = ASTCLASS_REMOVEMATCH )

	tkInit( )

	'' Note: the FILEBUFFER id currently must be made unique, or else it
	'' could be reused by other -removematch options, even if the pattern
	'' text differs...
	var id = "<-removematch """ + *n->text + """>"

	lexLoadC( 0, filebufferFromZstring( id, n->text ), FALSE )

	for x as integer = 0 to tkGetCount( )-1
		astAppend( n, astNewTK( x ) )
	next

	tkEnd( )
end sub

private function hPatternMatchesHere _
	( _
		byval n as ASTNODE ptr, _
		byval x as integer, _
		byval last as integer _
	) as integer

	var tk = n->head
	while( (tk <> NULL) and (x <= last) )
		assert( tk->class = ASTCLASS_TK )

		if( tk->tk <> tkGet( x ) ) then exit function

		var text = tkGetText( x )
		if( (tk->text <> NULL) <> (text <> NULL) ) then exit function
		if( text ) then
			if( *tk->text <> *text ) then exit function
		end if

		tk = tk->next
		x += 1
	wend

	function = TRUE
end function

private function hConstructMatchesPattern _
	( _
		byval n as ASTNODE ptr, _
		byval first as integer, _
		byval last as integer _
	) as integer

	assert( n->class = ASTCLASS_REMOVEMATCH )

	'' Check whether the pattern exists in the construct:
	'' For each token in the construct, check whether the pattern starts
	'' there and if so whether it continues...
	for x as integer = first to last
		if( hPatternMatchesHere( n, x, last ) ) then
			return TRUE
		end if
	next

end function

private function hConstructMatchesAnyPattern _
	( _
		byval presetcode as ASTNODE ptr, _
		byval first as integer, _
		byval last as integer _
	) as integer

	var i = presetcode->head
	while( i )
		if( i->class = ASTCLASS_REMOVEMATCH ) then
			if( hConstructMatchesPattern( i, first, last ) ) then
				return TRUE
			end if
		end if
		i = i->next
	wend

end function

private function frogWorkRootFile _
	( _
		byval presetcode as ASTNODE ptr, _
		byval targetversion as ASTNODE ptr, _
		byval rootfile as ASTNODE ptr _
	) as ASTNODE ptr

	print space( frog.maxversionstrlen ) + "parsing: " + *rootfile->text

	'' Go through -removematch options for this version and run lexLoadC()
	'' on the C token string given behind -removematch on the command line.
	'' This will write into the tk buffer so it must be done here before
	'' the main parsing process starts...
	var have_removematch = FALSE
	if( presetcode ) then
		var i = presetcode->head
		while( i )
			if( i->class = ASTCLASS_REMOVEMATCH ) then
				have_removematch = TRUE
				hLoadPatternTokens( i )
			end if
			i = i->next
		wend
	end if

	tkInit( )

	cppInit( )

	if( presetcode ) then
		'' Pre-#defines/#undefs are simply inserted at the top of the
		'' token buffer, so that cppMain() parses them like any other
		'' #define/#undef.
		dim cppheader as string

		var i = presetcode->head
		while( i )

			select case( i->class )
			case ASTCLASS_NOEXPAND
				cppNoExpandSym( i->text )

			case ASTCLASS_REMOVEDEFINE
				cppRemoveSym( i->text )

			case ASTCLASS_PPDEFINE
				cppheader += "#define " + *i->text
				if( i->expr ) then
					assert( i->expr->class = ASTCLASS_TEXT )
					cppheader += " " + *i->expr->text
				end if
				cppheader += !"\n"

				cppRemoveSym( i->text )

			case ASTCLASS_PPUNDEF
				cppheader += "#undef " + *i->text + !"\n"
				cppRemoveSym( i->text )

			case ASTCLASS_PPINCLUDE
				cppheader += "#include """ + *i->text + """" + !"\n"

			end select

			i = i->next
		wend

		if( len( cppheader ) > 0 ) then
			'' Extra empty line between the header and regular input code,
			'' so that cppMain()'s whitespace handling won't be confused.
			'' (e.g. associating a comment at the top of regular code with
			'' the last statement from the header code)
			cppheader += !"\n"

			var id = "<CPP code from command line for " + astDumpPrettyVersion( targetversion ) + ">"
			lexLoadC( 0, filebufferFromZstring( id, cppheader ), FALSE )
		end if
	end if

	cppMain( rootfile, frog.whitespace, frog.nomerge )

	cppEnd( )

	tkRemoveEOLs( )
	tkTurnCPPTokensIntoCIds( )

	'' Apply -removematch options, if there are any
	if( have_removematch ) then
		var x = 0
		while( tkGet( x ) <> TK_EOF )
			var begin = x
			x = hFindConstructEnd( x )

			if( hConstructMatchesAnyPattern( presetcode, begin, x ) ) then
				tkRemove( begin, x )
				x = begin - 1
			end if

			x += 1
		wend
	end if

	'' Parse C constructs
	var ast = cFile( )

	tkEnd( )

	''
	'' Work on the AST
	''
	astMakeProcsDefaultToCdecl( ast )
	astCleanUpExpressions( ast )
	astFixArrayParams( ast )
	astFixAnonUDTs( ast )
	astRemoveRedundantTypedefs( ast )
	astMoveNestedDefinesToToplevel( ast )
	if( frog.nonamefixup = FALSE ) then
		astFixIds( ast )
	end if
	if( frog.noautoextern = FALSE ) then
		astAutoExtern( ast, frog.windowsms, frog.whitespace )
	end if

	if( presetcode ) then
		assert( ast->class = ASTCLASS_GROUP )
		var i = presetcode->head
		while( i )
			if( i->class = ASTCLASS_APPENDBI ) then
				astAppend( ast, astNew( ASTCLASS_DIVIDER ) )
				astAppend( ast, astClone( i ) )
			end if
			i = i->next
		wend
	end if

	astMergeDIVIDERs( ast )

	'' Put file's AST into a VERBLOCK, if a targetversion was given,
	'' in preparation for the astMergeFiles() call later
	ast = astWrapFileInVerblock( ast, targetversion )

	function = ast
end function

private function frogWorkVersion _
	( _
		byval targetversion as ASTNODE ptr, _
		byval presetcode as ASTNODE ptr _
	) as ASTNODE ptr

	print "[" + astDumpPrettyVersion( targetversion ) + "]"

	var rootfiles = astNewGROUP( )

	var i = presetcode->head
	while( i )
		select case( i->class )
		case ASTCLASS_FILE
			'' Input files
			astCloneAppend( rootfiles, i )
		case ASTCLASS_DIR
			'' Input files from directories
			var n = hScanDirectory( *i->text, "*.h" )
			astSetLocationAndAlsoOnChildren( n, @i->location )
			astAppend( rootfiles, n )
		end select
		i = i->next
	wend

	if( rootfiles->head = NULL ) then
		oops( "no input files" )
	end if

	i = rootfiles->head
	while( i )
		i->expr = frogWorkRootFile( presetcode, targetversion, i )
		i = i->next
	wend

	function = rootfiles
end function

private sub hRemoveEmptyFiles( byval files as ASTNODE ptr )
	var f = files->head
	while( f )
		if( f->expr = NULL ) then
			f = astRemove( files, f )
		else
			f = f->next
		end if
	wend
end sub

private function hMakeDeclCountMessage( byval declcount as integer ) as string
	if( declcount = 1 ) then
		function = "1 declaration"
	else
		function = declcount & " declarations"
	end if
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	frog.versiondefine = "__VERSION__"
	frog.incdirs = astNewGROUP( )
	filebufferInit( )
	fbkeywordsInit( )

	tkInit( )

	'' Load all command line arguments into the tk buffer
	lexLoadArgs( 0, filebufferFromZstring( "<command line>", _
			hTurnArgsIntoString( __FB_ARGC__, __FB_ARGV__ ) ) )

	'' Load content of @files too
	hExpandResponseFileArguments( )

	'' Parse the command line arguments
	frog.code = hParseArgs( 0, BODY_TOPLEVEL )

	tkEnd( )

	var versions = astCollectVersions( frog.code )
	var targets = astCollectTargets( frog.code )

	'' If no targets given, assume all
	if( targets = 0 ) then
		targets = ASTATTRIB__ALLTARGET
	end if

	'' If no versions given, use a dummy, to hold the targets
	if( versions->head = NULL ) then
		astAppend( versions, astNew( ASTCLASS_DUMMYVERSION ) )
	end if

	var targetversions = astCombineVersionsAndTargets( versions, targets )

	'' There will always be at least one combined version; even if there
	'' were only targets and no versions, one dummy version per target will
	'' be used.
	assert( targetversions->head )

	if( frog.verbose ) then
		print "versions/targets:"
		var i = targetversions->head
		while( i )
			print "  " + astDumpPrettyVersion( i )
			i = i->next
		wend
	end if

	'' Find longest version string, for pretty output
	scope
		var i = targetversions->head
		do
			var s = astDumpPrettyVersion( i )
			if( frog.maxversionstrlen < len( s ) ) then
				frog.maxversionstrlen = len( s )
			end if
			i = i->next
		loop while( i )
		frog.maxversionstrlen += 3
	end scope

	var targetversion = targetversions->head

	'' For each version...
	dim as ASTNODE ptr files
	do
		'' Determine preset code for that version
		var presetcode = astGet1VersionAndTargetOnly( frog.code, targetversion )

		'' Parse files for this version and combine them with files
		'' from previous versions if possible
		files = astMergeFiles( files, frogWorkVersion( targetversion, presetcode ) )

		astDelete( presetcode )

		targetversion = targetversion->next
	loop while( targetversion )

	'' Remove files that don't have an AST left, i.e. were combined into
	'' others by astMergeFiles() and shouldn't be emitted individually anymore.
	hRemoveEmptyFiles( files )

	if( frog.common ) then
		astExtractCommonCodeFromFiles( files, versions )
	end if

	astProcessVerblocksAndTargetblocksOnFiles( files, versions, targets, frog.versiondefine )

	'' There should always be at least 1 file left to emit
	assert( files->head )

	'' Normally, each .bi file should be generated just next to the
	'' corresponding .h file (or one of them in case of merging...).
	'' In that case no directories need to be created.
	''
	'' If an output directory was given, then all .bi files should be
	'' emitted there. This might require creating some sub-directories to
	'' preserve the original directory structure.
	''     commonparent/a.bi
	''     commonparent/foo/a.bi
	''     commonparent/foo/b.bi
	'' shouldn't just become:
	''     outdir/a.bi
	''     outdir/a.bi
	''     outdir/b.bi
	'' because a.bi would be overwritten and the foo/ sub-directory would
	'' be lost. Instead, it should be:
	''     outdir/a.bi
	''     outdir/foo/a.bi
	''     outdir/foo/b.bi
	if( len( frog.outdir ) > 0 ) then
		var f = files->head
		var commonparent = pathOnly( *f->text )
		f = f->next
		while( f )
			commonparent = pathFindCommonBase( commonparent, *f->text )
			f = f->next
		wend
		if( frog.verbose ) then
			print "common parent: " + commonparent
		end if

		f = files->head
		do
			'' Remove the common parent prefix
			var normed = *f->text
			assert( strStartsWith( normed, commonparent ) )
			normed = right( normed, len( normed ) - len( commonparent ) )

			'' And prefix the output dir instead
			normed = pathAddDiv( frog.outdir ) + normed

			astSetText( f, normed )
			astSetComment( f, normed )

			f = f->next
		loop while( f )
	end if

	var f = files->head
	do
		'' Replace the file extension, .h -> .bi
		astSetText( f, pathStripExt( *f->text ) + ".bi" )

		'' Do auto-formatting if not preserving whitespace
		if( frog.whitespace = FALSE ) then
			astAutoAddDividers( f->expr )
		end if

		print "emitting: " + *f->text + " (" + _
			hMakeDeclCountMessage( astCountDecls( f->expr ) ) + ")"
		'astDump( f->expr )
		emitFile( *f->text, f->expr )

		f = f->next
	loop while( f )

	astDelete( files )
	astDelete( versions )
	astDelete( targetversions )

	if( frog.verbose ) then
		astPrintStats( )
	end if

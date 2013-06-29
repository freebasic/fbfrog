#include once "fbfrog.bi"

dim shared as FROGSTUFF frog

sub oops( byref message as string )
	print "oops, " & message
	end 1
end sub

function strReplace _
	( _
		byref text as string, _
		byref a as string, _
		byref b as string _
	) as string

	dim as string keep, result
	dim as integer alen = any, blen = any, i = any

	result = text

	alen = len( a )
	blen = len( b )

	i = 0
	do
		'' Does result contain an occurence of a?
		i = instr( i + 1, result, a )
		if( i = 0 ) then
			exit do
		end if

		'' Cut out a and insert b in its place
		'' result  =  front  +  b  +  back
		keep = right( result, len( result ) - ((i - 1) + alen) )
		result = left( result, i - 1 )
		result += b
		result += keep

		i += blen - 1
	loop

	function = result
end function

function strStartsWith( byref s as string, byref lookfor as string ) as integer
	function = (left( s, len( lookfor ) ) = lookfor)
end function

function strMatches _
	( _
		byref origpattern as string, _
		byref s as string _
	) as integer

	dim as string pattern = origpattern
	dim as integer wildcard = instr( pattern, "*" )
	if( instr( wildcard + 1, pattern, "*" ) > 0 ) then
		oops( __FUNCTION__ & "(): pattern with more than one wildcard" )
		end 1
	end if

	if( wildcard > 0 ) then
		dim as integer lhs = wildcard - 1
		dim as integer rhs = len( pattern ) - wildcard
		function = (( left( s, lhs ) =  left( pattern, lhs )) and _
		            (right( s, rhs ) = right( pattern, rhs )))
	else
		function = (pattern = s)
	end if
end function

private sub hPrintHelp( )
	print "fbfrog 0.1 from " + __DATE_ISO__
	print "usage: fbfrog *.h"
	print "The given *.h file will be translated into a *.bi file. It needs reviewing"
	print "and editing afterwards, so watch out for TODOs and C/FB differences like"
	print "procedure calling conventions."
	print "options:"
	print "  -merge       Combine all files into one"
	print "  -verbose     Show debugging info"
	print "  -help, -version      Help and version output"
	end 0
end sub

private sub hAddFromDir( byref d as string )
	dim as TLIST list = any
	dim as string ptr s = any

	listInit( @list, sizeof( string ) )

	hScanDirectoryForH( d, @list )

	s = listGetHead( @list )
	while( s )
		depAdd( *s )
		*s = ""
		s = listGetNext( s )
	wend

	listEnd( @list )
end sub

private sub hParseArgs( byval argc as integer, byval argv as zstring ptr ptr )
	dim as string arg

	for i as integer = 1 to argc-1
		arg = *argv[i]

		'' option?
		if( left( arg, 1 ) = "-" ) then
			'' Strip all preceding '-'s
			do
				arg = right( arg, len( arg ) - 1 )
			loop while( left( arg, 1 ) = "-" )

			select case( arg )
			case "help", "version"
				hPrintHelp( )
			case "merge"
				frog.merge = TRUE
			case "verbose"
				frog.verbose = TRUE
			case "dep"
				frog.dep = TRUE
			case else
				'' "-l<name>" or "-l <name>"
				if( left( arg, 1 ) = "l" ) then
					'' Cut off the l
					arg = right( arg, len( arg ) - 1 )

					'' Now empty? then it was just "-l"
					if( len( arg ) = 0 ) then
						'' Use the following arg, if any, as <name>
						i += 1
						if( i < argc ) then
							arg = *argv[i]
						else
							oops( "missing argument for -l option, please use -l<name> or -l <name>" )
						end if
					end if

					frog.preset = arg
				elseif( len( arg ) > 0 ) then
					oops( "unknown option: '" + arg + "', try --help" )
				end if
			end select
		else
			select case( pathExtOnly( arg ) )
			case "h", "hh", "hxx", "hpp", "c", "cc", "cxx", "cpp"
				'' File from command line, search in current directory
				depAdd( arg )
			case ""
				'' No extension? Treat as directory...
				hAddFromDir( arg )
			case else
				oops( "'" + arg + "' is not a *.h file" )
			end select
		end if
	next
end sub

private sub hSetPPIndentAttrib _
	( _
		byval n as ASTNODE ptr, _
		byval enable as integer _
	)

	var child = n->head
	while( child )
		hSetPPIndentAttrib( child, enable )
		child = child->next
	wend

	if( enable ) then
		select case( n->class )
		case ASTCLASS_PPIF
			n->attrib or= ASTATTRIB_PPINDENTBEGIN
		case ASTCLASS_PPELSEIF, ASTCLASS_PPELSE
			n->attrib or= ASTATTRIB_PPINDENTBEGIN or ASTATTRIB_PPINDENTEND
		case ASTCLASS_PPENDIF
			n->attrib or= ASTATTRIB_PPINDENTEND
		end select
	else
		select case( n->class )
		case ASTCLASS_PPIF, ASTCLASS_PPELSEIF, ASTCLASS_PPELSE, _
		     ASTCLASS_PPENDIF
			n->attrib and= not (ASTATTRIB_PPINDENTBEGIN or ASTATTRIB_PPINDENTEND)
		end select
	end if

end sub

private function hAstMatches _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr _
	) as integer

	if( a->class <> b->class ) then
		exit function
	end if

	'' Compare common children
	var achild = a->head
	var bchild = b->head
	while( (achild <> NULL) and (bchild <> NULL) )
		if( hAstMatches( achild, bchild ) = FALSE ) then
			exit function
		end if
		achild = achild->next
		bchild = bchild->next
	wend

	function = TRUE
end function

private function hFindFirstMatch _
	( _
		byval group as ASTNODE ptr, _
		byval lookfor as ASTNODE ptr _
	) as ASTNODE ptr

	assert( group->class = ASTCLASS_GROUP )

	var child = group->head
	while( child )
		if( hAstMatches( child, lookfor ) ) then
			exit while
		end if
		child = child->next
	wend

	astDelete( lookfor )
	function = child
end function

private function hFindLastMatch _
	( _
		byval group as ASTNODE ptr, _
		byval lookfor as ASTNODE ptr _
	) as ASTNODE ptr

	assert( group->class = ASTCLASS_GROUP )

	var child = group->tail
	while( child )
		if( hAstMatches( child, lookfor ) ) then
			exit while
		end if
		child = child->prev
	wend

	astDelete( lookfor )
	function = child
end function

private function hFindIncludeGuard _
	( _
		byval n as ASTNODE ptr, _
		byref firstifndef as ASTNODE ptr, _
		byref def as ASTNODE ptr, _
		byref lastendif as ASTNODE ptr _
	) as integer

	if( n->class <> ASTCLASS_GROUP ) then exit function

	'' Find first #ifndef and last #endif, if any
	firstifndef = hFindFirstMatch( n, _
		astNew( ASTCLASS_PPIF, _
			astNew( ASTCLASS_LOGNOT, _
				astNew( ASTCLASS_DEFINED ) ) ) )

	lastendif = hFindLastMatch( n, astNew( ASTCLASS_PPENDIF ) )

	if( (firstifndef = NULL) or (lastendif = NULL) ) then exit function

	'' Is the #ifndef followed by a #define?
	def = firstifndef->next
	if( def = NULL ) then exit function

	'' Compare the #ifndef ID against the #define ID, for an include guard
	'' it's supposed to be the same
	var ifndefid = firstifndef->head->head->head
	if( ifndefid = NULL ) then exit function
	if( ifndefid->class <> ASTCLASS_ID ) then exit function
	function = (*def->text = *ifndefid->text)
end function

private sub hRemovePPIndentFromIncludeGuard( byval n as ASTNODE ptr )
	dim as ASTNODE ptr firstifndef, def, lastendif
	if( hFindIncludeGuard( n, firstifndef, def, lastendif ) ) then
		hSetPPIndentAttrib( firstifndef, FALSE )
		hSetPPIndentAttrib( lastendif, FALSE )
	end if
end sub

private sub hRemoveIncludeGuard( byval n as ASTNODE ptr )
	dim as ASTNODE ptr firstifndef, def, lastendif
	if( hFindIncludeGuard( n, firstifndef, def, lastendif ) ) then
		astRemoveChild( n, firstifndef )
		astRemoveChild( n, def )
		astRemoveChild( n, lastendif )
	end if
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	depInit( )
	fsInit( )

	hParseArgs( __FB_ARGC__, __FB_ARGV__ )

	if( fsGetHead( ) = NULL ) then
		oops( "no input files" )
	end if

	if( frog.dep ) then
		depScan( )
		depPrintFlat( )
		end 0
	end if

	''
	'' By default, all input files are translated 1:1. With -merge however,
	'' files with refcount = 1 are merged into their parent (in place of
	'' the #include statement), and files with refcount = 0 are appended.
	''
	'' Concat: Load & parse all files with refcount = 0 into the token buffer,
	'' one after another, so #includes (when merging) can be handled
	'' in the context of their parent, the current file.
	''
	'' Regular translation, 1:1, possibly with merges
	'' Pass 1: Translate everything that doesn't look like it'll be merged
	'' Pass 2: Translate files that looked like they should be merged,
	''         but weren't. (recursive #includes, all refcount > 0)
	''

	dim as FSFILE ptr f = any
	dim as ASTNODE ptr ast = any

	f = fsGetHead( )
	while( f )

		fsPush( f )
		tkInit( )
		lexLoadFile( 0, f->normed )
		print "translating: ";f->normed

		ppComments( )
		ppDividers( )
		ppDirectives1( )

		select case( frog.preset )
		case "zip"
			tkRemoveAllOf( TK_ID, "ZIP_EXTERN" )
		end select

		ppDirectives2( )

		if( (strMatches( "tests/*", f->pretty ) = FALSE) or _
		    strMatches( "tests/pp/eval-*", f->pretty ) ) then
			ppEvalInit( )
			ppEvalExpressions( )
			ppSplitElseIfs( )
			ppEvalIfs( )
			ppMergeElseIfs( )
			ppEvalEnd( )
		end if

		ast = cToplevel( )

		hSetPPIndentAttrib( ast, TRUE )
		'hRemovePPIndentFromIncludeGuard( ast )
		hRemoveIncludeGuard( ast )
		if( strMatches( "tests/pp/expr-*", f->pretty ) ) then
			hSetPPIndentAttrib( ast, FALSE )
		end if

		'astDump( ast )
		emitFile( pathStripExt( f->normed ) + ".bi", ast )
		astDelete( ast )

		tkEnd( )
		fsPop( )

		f = listGetNext( f )
	wend

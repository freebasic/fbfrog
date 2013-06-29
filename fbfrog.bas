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

private sub frogInit( )
	listInit( @frog.files, sizeof( FROGFILE ) )
	hashInit( @frog.filehash, 6 )
end sub

private function frogAddFile _
	( _
		byval context as FROGFILE ptr, _
		byref pretty as string _
	) as FROGFILE ptr

	dim as string normed

	if( context ) then
		'' Search for #included files in one of the parent directories
		'' of the current file. Usually the #include will refer to a
		'' file in the same directory or in a sub-directory at the same
		'' level or some levels up.

		var parent = pathOnly( context->normed )
		do
			'' File not found anywhere, ignore it.
			if( len( parent ) = 0 ) then
				normed = ""
				exit do
			end if

			normed = parent + pretty
			if( hFileExists( normed ) ) then
				if( frog.verbose ) then
					print "    found: " + normed
				end if
				exit do
			end if

			if( frog.verbose ) then
				print "    not found: " + normed
			end if

			parent = pathStripLastComponent( parent )
		loop
	else
		normed = pathMakeAbsolute( pretty )
	end if

	var missing = FALSE
	if( len( normed ) > 0 ) then
		normed = pathNormalize( normed )
	else
		'' File missing/not found; still add it to the hash
		normed = pretty
		missing = TRUE
	end if

	var hash = hashHash( normed )
	var item = hashLookup( @frog.filehash, normed, hash )
	if( item->s ) then
		'' Already exists
		if( frog.verbose ) then
			print "    old news: " + pretty + ": " + normed
		end if
		return item->data
	end if

	if( frog.verbose ) then
		if( missing ) then
			print "    registered: " + pretty + " (missing)"
		else
			print "    registered: " + pretty + ": " + normed
		end if
	end if

	'' Add file
	dim as FROGFILE ptr f = listAppend( @frog.files )
	f->pretty = pretty
	f->normed = normed
	f->missing = missing

	'' Add to hash table
	hashAdd( @frog.filehash, item, hash, f->normed, f )

	function = f
end function

private sub hPrintHelp( byref message as string )
	if( len( message ) > 0 ) then
		print message
	end if
	print "fbfrog 0.1 from " + __DATE_ISO__
	print "usage: fbfrog *.h"
	print "The given *.h file will be translated into a *.bi file. It needs reviewing"
	print "and editing afterwards, so watch out for TODOs and C/FB differences like"
	print "procedure calling conventions."
	print "options:"
	print "  -l <name>    Select preset"
	print "  -verbose     Show debugging info"
	end (iif( len( message ) > 0, 1, 0 ))
end sub

private sub hAddFromDir( byref d as string )
	dim as TLIST list
	listInit( @list, sizeof( string ) )

	hScanDirectoryForH( d, @list )

	dim as string ptr s = listGetHead( @list )
	while( s )
		frogAddFile( NULL, *s )
		*s = ""
		s = listGetNext( s )
	wend

	listEnd( @list )
end sub

private sub hParseArgs1( byval argc as integer, byval argv as zstring ptr ptr )
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
				hPrintHelp( "" )
			case "merge"
				frog.merge = TRUE
			case "verbose"
				frog.verbose = TRUE
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
							hPrintHelp( "missing argument for -l option" )
						end if
					end if

					frog.preset = arg
				elseif( len( arg ) > 0 ) then
					hPrintHelp( "unknown option: " + *argv[i] )
				end if
			end select
		end if
	next
end sub

private sub hParseArgs2( byval argc as integer, byval argv as zstring ptr ptr )
	dim as string arg
	for i as integer = 1 to argc-1
		arg = *argv[i]
		if( left( arg, 1 ) <> "-" ) then
			select case( pathExtOnly( arg ) )
			case "h", "hh", "hxx", "hpp", "c", "cc", "cxx", "cpp"
				'' File from command line, search in current directory
				frogAddFile( NULL, arg )
			case ""
				'' No extension? Treat as directory...
				hAddFromDir( arg )
			case else
				hPrintHelp( "'" + arg + "' is not a *.h file" )
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

private sub frogLoadFile( byval f as FROGFILE ptr )
	print "parsing: ";f->pretty

	tkInit( )
	lexLoadFile( 0, f->normed )

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

		select case( frog.preset )
		case "zip"
			ppAddSymbol( "ZIP_EXTERN", TRUE )
			ppAddSymbol( "__cplusplus", FALSE )
			ppAddSymbol( "ZIP_DISABLE_DEPRECATED", FALSE )
		end select

		ppEvalExpressions( )
		ppSplitElseIfs( )
		ppEvalIfs( )
		ppMergeElseIfs( )
		ppEvalEnd( )
	end if

	f->ast = cToplevel( )

	tkEnd( )

	hSetPPIndentAttrib( f->ast, TRUE )
	'hRemovePPIndentFromIncludeGuard( ast )
	hRemoveIncludeGuard( f->ast )
	if( strMatches( "tests/pp/expr-*", f->pretty ) ) then
		hSetPPIndentAttrib( f->ast, FALSE )
	end if
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	frogInit( )

	hParseArgs1( __FB_ARGC__, __FB_ARGV__ )
	hParseArgs2( __FB_ARGC__, __FB_ARGV__ )

	if( listGetHead( @frog.files ) = NULL ) then
		oops( "no input files" )
	end if

	'' Load files given on command line.
	'' Load #includes too if available and not yet done.
	'' Files newly registered by the inner loop will eventually be worked
	'' off by the outer loop, as they're appended to the files list.
	dim as FROGFILE ptr f = listGetHead( @frog.files )
	while( f )
		if( f->missing = FALSE ) then
			frogLoadFile( f )

			'' For each statement...
			var child = f->ast->head
			while( child )
				'' #include?
				if( child->class = ASTCLASS_PPINCLUDE ) then
					print "  #include: " & *child->text;
					if( frog.verbose ) then
						print
					end if
					child->includefile = frogAddFile( f, *child->text )
					if( frog.verbose = FALSE ) then
						if( child->includefile->missing ) then
							print " (not found)";
						end if
						print
					end if
				end if
				child = child->next
			wend
		end if
		f = listGetNext( f )
	wend

	'emitFile( pathStripExt( f->normed ) + ".bi", ast )

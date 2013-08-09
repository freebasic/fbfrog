'' Main module, command line interface

#include once "fbfrog.bi"

dim shared as FROGSTUFF frog

private sub frogInit( )
	listInit( @frog.files, sizeof( FROGFILE ) )
	hashInit( @frog.filehash, 6 )
end sub

private function hFindCommonParent( ) as string
	dim as string s
	dim as FROGFILE ptr f = listGetHead( @frog.files )
	while( f )
		if( f->missing = FALSE ) then
			if( len( s ) > 0 ) then
				s = pathFindCommonBase( s, f->normed )
			else
				s = pathOnly( f->normed )
			end if
		end if
		f = listGetNext( f )
	wend
	function = s
end function

private function frogAddFile _
	( _
		byval context as FROGFILE ptr, _
		byref pretty as string _
	) as FROGFILE ptr

	dim as string normed, report

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
					if( len( report ) ) then print report
					report = "    found: " + normed
				end if
				exit do
			end if

			if( frog.verbose ) then
				if( len( report ) ) then print report
				report = "    not found: " + normed
			end if

			'' Stop searching parent directories after trying
			'' the common base
			if( parent = frog.commonparent ) then
				normed = ""
				exit do
			end if

			parent = pathStripLastComponent( parent )
		loop
	else
		normed = pathMakeAbsolute( pretty )
		if( frog.verbose ) then
			report = "    root: " + pretty
		end if
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
			print report + " (old news)"
		end if
		return item->data
	end if

	if( frog.verbose ) then
		if( missing ) then
			print report + " (missing)"
		else
			print report + " (new)"
		end if
	end if

	'' Add file
	dim as FROGFILE ptr f = listAppend( @frog.files )
	f->pretty = pretty
	f->normed = normed
	f->missing = missing

	'' Add to hash table
	hashAdd( @frog.filehash, item, hash, f->normed, f )

	'' Update/recalculate common base path
	frog.commonparent = hFindCommonParent( )

	function = f
end function

private sub hPrintHelp( byref message as string )
	if( len( message ) > 0 ) then
		print message
	end if
	print "fbfrog 0.1 from " + __DATE_ISO__ + ", usage: fbfrog [options] *.h"
	print "options:"
	print "  -l <name>    Select preset"
	print "  -m           Merge multiple headers into one"
	print "  -v           Show debugging info"
	print "By default, fbfrog will generate a *.bi file for each given *.h file."
	print "*.bi files need to be reviewed and tested! Watch out for calling conventions!"
	end (iif( len( message ) > 0, 1, 0 ))
end sub

private sub hHandleOption _
	( _
		byval argc as integer, _
		byval argv as zstring ptr ptr, _
		byref i as integer, _
		byref arg as string _
	)

	select case( arg )
	case "h", "?", "help", "version"
		hPrintHelp( "" )
	case "m"
		frog.merge = TRUE
		exit sub
	case "v"
		frog.verbose = TRUE
		exit sub
	end select

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
		exit sub
	end if

	hPrintHelp( "unknown option: " + *argv[i] )
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

			hHandleOption( argc, argv, i, arg )
		end if
	next
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

private sub hRemoveNode _
	( _
		byval n as ASTNODE ptr, _
		byval astclass as integer, _
		byref id as string _
	)

	if( (n->class = astclass) and (*n->text = id) ) then
		n->class = ASTCLASS_NOP
		exit sub
	end if

	var child = n->head
	while( child )
		hRemoveNode( child, astclass, id )
		child = child->next
	wend

end sub

private sub hFixArrayParams( byval n as ASTNODE ptr )
	if( n->class = ASTCLASS_PARAM ) then
		'' C array parameters are really just pointers (i.e. the array
		'' is passed byref), and FB doesn't support array parameters
		'' like that, so turn them into pointers:
		''    int a[5]  ->  byval a as long ptr
		if( n->array ) then
			astDelete( n->array )
			n->array = NULL
			n->dtype = typeAddrOf( n->dtype )
		end if
	end if

	var child = n->head
	while( child )
		hFixArrayParams( child )
		child = child->next
	wend
end sub

'' Removes typedefs where the typedef identifier is the same as the struct tag,
'' e.g. "typedef struct T T;" since FB doesn't have separate struct/type
'' namespaces and such typedefs aren't needed.
private sub hRemoveRedundantTypedefs( byval n as ASTNODE ptr )
	var child = n->head
	while( child )
		hRemoveRedundantTypedefs( child )
		child = child->next
	wend

	if( (n->class = ASTCLASS_TYPEDEF) and _
	    (typeGetDtAndPtr( n->dtype ) = TYPE_UDT) ) then
		assert( n->subtype->class = ASTCLASS_ID )
		if( ucase( *n->text, 1 ) = ucase( *n->subtype->text ) ) then
			n->class = ASTCLASS_NOP
		end if
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
			ppAddSym( "ZIP_EXTERN", TRUE )
			ppAddSym( "__cplusplus", FALSE )
			ppAddSym( "ZIP_DISABLE_DEPRECATED", FALSE )
			ppAddSym( "_HAD_ZIP_H", FALSE )
			ppAddSym( "_HAD_ZIPCONF_H", FALSE )
		end select

		ppEvalExpressions( )
		ppSplitElseIfs( )
		ppEvalIfs( )
		ppMergeElseIfs( )
		ppEvalEnd( )
	end if

	f->ast = cToplevel( )

	tkEnd( )

	select case( frog.preset )
	case "zip"
		hRemoveNode( f->ast, ASTCLASS_PPDEFINE, "_HAD_ZIP_H" )
		hRemoveNode( f->ast, ASTCLASS_PPDEFINE, "_HAD_ZIPCONF_H" )
	end select

	hFixArrayParams( f->ast )
	hRemoveRedundantTypedefs( f->ast )

	hSetPPIndentAttrib( f->ast, TRUE )
	hRemovePPIndentFromIncludeGuard( f->ast )
	if( strMatches( "tests/pp/expr-*", f->pretty ) ) then
		hSetPPIndentAttrib( f->ast, FALSE )
	end if
end sub

private function frogCalcRefcount( byval lookfor as FROGFILE ptr ) as integer
	var refcount = 0

	dim as FROGFILE ptr f = listGetHead( @frog.files )
	while( f )

		'' File loaded? (missing files weren't loaded)
		if( f->ast ) then
			'' For each statement...
			var child = f->ast->head
			while( child )
				'' #include?
				if( child->class = ASTCLASS_PPINCLUDE ) then
					if( child->includefile = lookfor ) then
						refcount += 1
					end if
				end if
				child = child->next
			wend
		end if

		f = listGetNext( f )
	wend

	function = refcount
end function

private sub hTryMergeIncludes( byval n as ASTNODE ptr )
	var child = n->head
	while( child )
		'' #include?
		if( child->class = ASTCLASS_PPINCLUDE ) then
			'' If this is the only reference, merge
			var incf = child->includefile
			if( incf->refcount = 1 ) then
				if( incf->ast ) then
					print "  #include: " & incf->pretty
					child = astReplaceChild( n, child, incf->ast )
					incf->ast = NULL
				end if
			end if
		end if

		hTryMergeIncludes( child )

		child = child->next
	wend
end sub

private sub hMergeGROUPs( byval n as ASTNODE ptr )
	var child = n->head
	while( child )
		var nxt = child->next

		if( child->class = ASTCLASS_GROUP ) then
			var groupchild = child->head
			var first = groupchild
			while( groupchild )
				astAddChildBefore( n, astClone( groupchild ), child )
				groupchild = groupchild->next
			wend

			astRemoveChild( n, child )

			'' Work on the GROUP's children too (if any),
			'' since it may contain nested GROUPs
			if( first ) then
				nxt = first
			end if
		end if

		child = nxt
	wend
end sub

private sub hRemoveNOPs( byval n as ASTNODE ptr )
	var child = n->head
	while( child )
		var nxt = child->next

		if( child->class = ASTCLASS_NOP ) then
			astRemoveChild( n, child )
		end if

		child = nxt
	wend
end sub

private sub hMergeDIVIDERs( byval n as ASTNODE ptr )
	var child = n->head
	while( child )
		var nxt = child->next

		if( nxt ) then
			if( (child->class = ASTCLASS_DIVIDER) and _
			    (  nxt->class = ASTCLASS_DIVIDER) ) then
				astRemoveChild( n, child )
			end if
		end if

		child = nxt
	wend
end sub

private sub hRemoveOuterDIVIDERs( byval n as ASTNODE ptr )
	if( n->head = NULL ) then
		exit sub
	end if

	if( n->head->class = ASTCLASS_DIVIDER ) then
		astRemoveChild( n, n->head )
	end if

	if( n->tail->class = ASTCLASS_DIVIDER ) then
		astRemoveChild( n, n->tail )
	end if
end sub

private sub frogEmitFile( byval f as FROGFILE ptr )
	var binormed = pathStripExt( f->normed ) + ".bi"
	var bipretty = pathStripExt( f->pretty ) + ".bi"
	print "emitting: " + bipretty
	hMergeGROUPs( f->ast )
	hRemoveNOPs( f->ast )
	hMergeDIVIDERs( f->ast )
	hRemoveOuterDIVIDERs( f->ast )
	'astDump( f->ast )

	if( len( frog.preset ) > 0 ) then
		var astdumpfile = pathStripExt( f->normed ) + ".ast.bi"
		emitFile( astdumpfile, f->ast, TRUE )
	end if

	emitFile( binormed, f->ast )
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
			print "parsing: ";f->pretty
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
							print " (not found)"
						else
							print ''" -> " + child->includefile->normed
						end if
					end if
				end if
				child = child->next
			wend
		end if

		f = listGetNext( f )
	wend

	if( frog.merge ) then
		'' Calculate #include refcounts
		f = listGetHead( @frog.files )
		while( f )
			f->refcount = frogCalcRefcount( f )
			f = listGetNext( f )
		wend

		'' Merge every #include that can be merged
		f = listGetHead( @frog.files )
		while( f )
			if( f->ast ) then
				print "merging: " + f->pretty
				hTryMergeIncludes( f->ast )
			end if
			f = listGetNext( f )
		wend

		'' Concatenate files with refcount=0
		dim as FROGFILE ptr first
		f = listGetHead( @frog.files )
		while( f )
			if( f->refcount = 0 ) then
				if( first ) then
					'' Already have a first; append to it
					if( f->ast ) then
						astAddChild( first->ast, f->ast )
						f->ast = NULL
					end if
				else
					'' This is the first
					first = f
				end if
			end if
			f = listGetNext( f )
		wend
	end if

	'' Emit all files that have an AST left (i.e. weren't merged into or
	'' appended to anything)
	f = listGetHead( @frog.files )
	while( f )
		if( f->ast ) then
			frogEmitFile( f )
		end if
		f = listGetNext( f )
	wend

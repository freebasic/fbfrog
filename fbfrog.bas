#include once "fbfrog.bi"

dim shared as FROGSTUFF frog

sub oops( byref message as string )
	print "oops, " & message
	end 1
end sub

private sub frogInit( )
	frog.concat = FALSE
	frog.follow = FALSE
	frog.merge = FALSE
	frog.verbose = FALSE
	frog.f = NULL

	hashInit( @frog.definehash, 6 )
	listInit( @frog.files, sizeof( FROGFILE ) )
	hashInit( @frog.filehash, 6 )
end sub

function frogAddDefine _
	( _
		byval id as zstring ptr, _
		byval flags as uinteger _
	) as uinteger

	dim as integer length = any, dat = any
	dim as uinteger hash = any
	dim as HashItem ptr item = any

	length = len( *id )
	assert( length > 0 )

	hash = hashHash( id, length )
	item = hashLookup( @frog.definehash, id, length, hash )

	if( item->s ) then
		'' Already exists, add the flags. For lookups only, flags
		'' should be 0, so nothing changes.
		item->data = cast( any ptr, cuint( item->data ) or flags )
		return cuint( item->data )
	end if

	'' If no information, don't bother adding, allowing this function
	'' to be used for lookups only.
	if( flags ) then
		'' Add new
		dat = -1
		hashAdd( @frog.definehash, item, hash, _
		         storageStore( id, length, @dat ), length, cast( any ptr, flags ) )
	end if

	function = flags
end function

function frogAddFile _
	( _
		byref origname as string, _
		byval is_preparse as integer, _
		byval search_paths as integer _
	) as FROGFILE ptr

	dim as string hardname, parent
	dim as uinteger hash = any
	dim as HASHITEM ptr item = any
	dim as FROGFILE ptr f = any

	if( search_paths ) then
		'' File collected from an #include directive. Try to find it
		'' in one of the parent directories of the current file.
		''
		'' (Usually the #include will refer to a file in the same
		'' directory or in a sub-directory at the same level or some
		'' levels up)

		parent = pathOnly( *frog.f->hardname )
		do
			'' File not found anywhere, ignore it.
			if( len( parent ) = 0 ) then
				if( frog.verbose ) then
					print "  ignoring: " + origname
				end if
				return NULL
			end if

			hardname = parent + origname
			if( hFileExists( hardname ) ) then
				if( frog.verbose ) then
					print "  found: " + origname + ": " + hardname
				end if
				exit do
			end if

			if( frog.verbose ) then
				print "  not found: " + origname + ": " + hardname
			end if

			parent = pathStripLastComponent( parent )
		loop
	else
		'' File from command line, search in current directory
		hardname = pathMakeAbsolute( origname )
	end if

	hardname = pathNormalize( hardname )

	assert( len( origname ) > 0 )
	assert( len( hardname ) > 0 )

	hash = hashHash( strptr( hardname ), len( hardname ) )
	item = hashLookup( @frog.filehash, strptr( hardname ), len( hardname ), hash )

	if( item->s ) then
		'' Already exists
		if( frog.verbose ) then
			print "  old news: " + origname + ": " + hardname
		end if
		return item->data
	end if

	if( frog.verbose ) then
		print "  new file: " + origname + ": " + hardname
	end if

	'' Add file
	f = listAppend( @frog.files )
	f->softname = strDuplicate( strptr( origname ), len( origname ) )
	f->hardname = strDuplicate( strptr( hardname ), len( hardname ) )
	f->refcount = 0
	f->flags = iif( is_preparse, FILE_EXTRA, 0 )

	'' Add to hash table
	hashAdd( @frog.filehash, item, hash, f->hardname, len( hardname ), f )

	function = f
end function

private sub frogAddFromDir( byref d as string )
	dim as LINKEDLIST list = any
	dim as string ptr s = any

	listInit( @list, sizeof( string ) )

	hScanDirectoryForH( d, @list )

	s = listGetHead( @list )
	while( s )
		frogAddFile( *s, FALSE, FALSE )
		*s = ""
		s = listGetNext( s )
	wend

	listEnd( @list )
end sub

sub frogSetVisited( byval f as FROGFILE ptr )
	f->flags or= FILE_VISITED
end sub

private function frogCanVisit( byval f as FROGFILE ptr ) as integer
	function = ((f->flags and FILE_VISITED) = 0)
end function

private function frogCanFollow( byval f as FROGFILE ptr ) as integer
	'' Only work on files found during the #include preparse if
	'' --follow was given
	function = (frog.follow or ((f->flags and FILE_EXTRA) = 0))
end function

function frogCanMerge( byval f as FROGFILE ptr ) as integer
	function = (frog.merge and (f->refcount = 1) and frogCanFollow( f ))
end function

private function frogCanWorkOn( byval f as FROGFILE ptr ) as integer
	function = (frogCanVisit( f ) and frogCanFollow( f ))
end function

private sub hPrintHelp( )
	print "fbfrog 0.1 from " + __DATE_ISO__
	print "usage: fbfrog *.h"
	print "The given *.h file will be translated into a *.bi file. It needs reviewing"
	print "and editing afterwards, so watch out for TODOs and C/FB differences like"
	print "procedure calling conventions."
	print "options:"
	print "  -concat      Concatenate headers that don't #include each other"
	print "  -follow      Also translate all #includes that can be found"
	print "  -merge       Insert #included files into their parent"
	print "  -verbose     Show debugging info"
	print "  -help, -version      Help and version output"
	end 0
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	dim as string arg
	dim as FROGFILE ptr first = any
	dim as integer x = any

	frogInit( )
	storageInit( )

	for i as integer = 1 to __FB_ARGC__-1
		arg = *__FB_ARGV__[i]

		if( len( arg ) = 0 ) then
			continue for
		end if

		if( arg[0] <> asc( "-" ) ) then
			'' file names will be handled later
			continue for
		end if

		do
			arg = right( arg, len( arg ) - 1 )
		loop while( left( arg, 1 ) = "-" )

		select case( arg )
		case "concat"
			frog.concat = TRUE
		case "follow"
			frog.follow = TRUE
		case "help", "version"
			hPrintHelp( )
		case "merge"
			frog.merge = TRUE
		case "verbose"
			frog.verbose = TRUE
		case else
			if( len( arg ) > 0 ) then
				oops( "unknown option: '" + arg + "', try --help" )
			end if
		end select
	next

	'' Now that all options are known -- start adding the files
	for i as integer = 1 to __FB_ARGC__-1
		arg = *__FB_ARGV__[i]

		if( len( arg ) = 0 ) then
			continue for
		end if

		if( arg[0] = asc( "-" ) ) then
			continue for
		end if

		select case( pathExtOnly( arg ) )
		case "h", "hh", "hxx", "hpp", "c", "cc", "cxx", "cpp"
			frogAddFile( arg, FALSE, FALSE )
		case ""
			'' No extension? Treat as directory...
			frogAddFromDir( arg )
		case else
			oops( "not a .h file: '" + arg + "'" )
		end select
	next

	if( listGetHead( @frog.files ) = NULL ) then
		oops( "no input files" )
	end if

	''
	'' Preparse everything to collect a list of all involved headers.
	'' (no translation yet)
	''
	'' Data collected by the preparse:
	'' - All modes except the default want to know more input files
	''   from #includes, including /their/ #includes, and so on...
	'' - --concat/--merge also need to know who includes what and how often
	'' - A list (actually a hash table) of #defines that represent
	''   calling conventions or dllexport declspecs or are empty,
	''   this information is used by the procdecl parser/translator
	''   to allow/disallow such a #define and, in case of callconvs,
	''   preserve and move it where it belongs in FB syntax.
	''

	'' Go through all input files, and new ones as they are
	'' appended to the list...
	frog.f = listGetHead( @frog.files )
	while( frog.f )

		if( frogCanFollow( frog.f ) ) then
			print "preparsing: " + *frog.f->softname
			tkInit( )
			lexInsertFile( 0, *frog.f->hardname )
			preparseToplevel( )
			tkEnd( )
		end if

		frog.f = listGetNext( frog.f )
	wend

	''
	'' By default, all input files are translated 1:1.
	''
	'' --follow only enables the preparse, to find more input files.
	''
	'' --merge however causes the parser to merge in #includes that only
	'' have one reference/parent. So with --merge those will-be-merged
	'' files need to be skipped here, instead of being translated 1:1.
	''
	'' --concat on the other hand should concatenate all files into a
	'' single file, provided they don't already #include each other.
	'' This means translating & concatenating everything with refcount = 0,
	'' while working off the rest normally.
	''

	''
	'' --concat pass
	''
	'' Load & parse all files with refcount = 0 into the token buffer,
	'' one after another, so #includes (when merging) can be handled
	'' in the context of their parent, the current file.
	''
	if( frog.concat ) then
		tkInit( )

		first = NULL
		frog.f = listGetHead( @frog.files )
		while( frog.f )

			'' Only concatenate if not #included anywhere
			if( frogCanVisit( frog.f ) and (frog.f->refcount = 0) ) then
				frogSetVisited( frog.f )

				if( first = NULL ) then
					first = frog.f
					print "first: " + *frog.f->softname
				else
					print "appending: " + *frog.f->softname
				end if

				frog.f->flags or= FILE_VISITED

				x = tkCount( )

				'' Insert the file content behind some EOLs, to ensure it's separated
				'' (not all files have EOL at EOF), unless it's the first
				lexInsertFile( x, *frog.f->hardname )
				if( x > 0 ) then
					tkInsert( x, TK_EOL, NULL )
					tkInsert( x, TK_EOL, NULL )
				end if

				'' Now parse the appended tokens, preparing for translation...
				'' and to possibly merge #includes (if --merge is on).
				parseToplevel( x )
			end if

			frog.f = listGetNext( frog.f )
		wend

		if( first ) then
			print "concatenating as: " + pathStripExt( *first->softname ) + ".bi"
			translateToplevel( )
			emitWriteFile( pathStripExt( *first->hardname ) + ".bi" )
		end if

		tkEnd( )
	end if

	''
	'' Regular translation, 1:1, possibly with merges
	'' Pass 1: Translate everything that doesn't look like it'll be merged
	'' Pass 2: Translate files that looked like they should be merged,
	''         but weren't. (recursive #includes, all refcount > 0)
	''
	for i as integer = 0 to 1
		frog.f = listGetHead( @frog.files )
		while( frog.f )

			if( frogCanWorkOn( frog.f ) and _
			    ((frogCanMerge( frog.f ) = FALSE) or (i = 1)) ) then
				frogSetVisited( frog.f )
				print "translating: " + *frog.f->softname

				tkInit( )
				lexInsertFile( 0, *frog.f->hardname )

				parseToplevel( 0 )
				translateToplevel( )

				emitWriteFile( pathStripExt( *frog.f->hardname ) + ".bi" )
				tkEnd( )
			end if

			frog.f = listGetNext( frog.f )
		wend
	next

	print "done: ";
	emitStats( )
	if( frog.verbose ) then
		hashStats( @frog.filehash, "filename" )
		hashStats( @frog.definehash, "define" )
		storageStats( )
	end if
	end 0

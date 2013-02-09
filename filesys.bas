'' Include file searching (and caching of the found file names)

#include once "fbfrog.bi"

type FSSTUFF
	files		as TLIST '' FROGFILE
	filehash	as THASH

	contextstack	as TLIST '' FSFILE ptr, stack of parent contexts
	context		as FSFILE ptr  '' current context
end type

dim shared as FSSTUFF filesys

sub fsInit( )
	listInit( @filesys.files, sizeof( FSFILE ) )
	hashInit( @filesys.filehash, 6 )
	listInit( @filesys.contextstack, sizeof( FSFILE ptr ) )
end sub

sub fsPush( byval context as FSFILE ptr )
	dim as FSFILE ptr ptr node = any

	'' Already have a context? Back up on stack
	if( filesys.context ) then
		node = listAppend( @filesys.contextstack )
		*node = filesys.context
	end if

	filesys.context = context
end sub

sub fsPop( )
	dim as FSFILE ptr ptr node = any

	node = listGetTail( @filesys.contextstack )
	if( node ) then
		filesys.context = *node
		listDelete( @filesys.contextstack, node )
	else
		filesys.context = NULL
	end if
end sub

function fsAdd( byref pretty as string ) as FSFILE ptr
	dim as string normed, real, parent
	dim as uinteger hash = any
	dim as THASHITEM ptr item = any
	dim as FSFILE ptr f = any

	if( filesys.context ) then
		'' Search for #included files in one of the parent directories
		'' of the current file. Usually the #include will refer to a
		'' file in the same directory or in a sub-directory at the same
		'' level or some levels up.

		parent = pathOnly( filesys.context->normed )
		do
			'' File not found anywhere, ignore it.
			if( len( parent ) = 0 ) then
				if( frog.verbose ) then
					print "  missing: " + pretty
				end if
				real = ""
				exit do
			end if

			real = parent + pretty
			if( hFileExists( real ) ) then
				if( frog.verbose ) then
					print "  found: " + pretty + ": " + real
				end if
				exit do
			end if

			if( frog.verbose ) then
				print "  not found: " + pretty + ": " + real
			end if

			parent = pathStripLastComponent( parent )
		loop
	else
		real = pathMakeAbsolute( pretty )
	end if

	normed = pathNormalize( real )

	if( len( normed ) > 0 ) then
		hash = hashHash( normed )
		item = hashLookup( @filesys.filehash, normed, hash )

		if( item->s ) then
			'' Already exists
			if( frog.verbose ) then
				print "  old news: " + pretty + ": " + normed
			end if
			return item->data
		end if
	else
		hash = hashHash( pretty )
		item = hashLookup( @filesys.filehash, pretty, hash )
	end if

	if( frog.verbose ) then
		print "  new file: " + pretty + ": " + normed
	end if

	'' Add file
	f = listAppend( @filesys.files )
	f->pretty = pretty
	f->normed = normed

	'' Add to hash table
	if( len( f->normed ) > 0 ) then
		hashAdd( @filesys.filehash, item, hash, f->normed, f )
	else
		hashAdd( @filesys.filehash, item, hash, f->pretty, f )
	end if

	function = f
end function

function fsGetHead( ) as FSFILE ptr
	function = listGetHead( @filesys.files )
end function

sub fsStats( )
	hashStats( @filesys.filehash, "filename" )
end sub

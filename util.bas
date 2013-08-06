'' Generic linked list
''
'' Generic hash table (open addressing/closed hashing),
'' based on GCC's libcpp's hash table.
''
'' Path/file name handling functions, directory tree search

#include once "fbfrog.bi"
#include once "crt.bi"
#include once "dir.bi"

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

	var result = text

	var alen = len( a )
	var blen = len( b )

	var i = 0
	do
		'' Does result contain an occurence of a?
		i = instr( i + 1, result, a )
		if( i = 0 ) then
			exit do
		end if

		'' Cut out a and insert b in its place
		'' result  =  front  +  b  +  back
		var keep = right( result, len( result ) - ((i - 1) + alen) )
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

function strEndsWith( byref s as string, byref lookfor as string ) as integer
	function = (right( s, len( lookfor ) ) = lookfor)
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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#define listGetPtr( node ) cptr( any ptr, cptr( ubyte ptr, node ) + sizeof( LISTNODE ) )
#define listGetNode( p ) cptr( LISTNODE ptr, cptr( ubyte ptr, p ) - sizeof( LISTNODE ) )

function listGetHead( byval l as TLIST ptr) as any ptr
	if( l->head ) then
		function = listGetPtr( l->head )
	end if
end function

function listGetTail( byval l as TLIST ptr) as any ptr
	if( l->tail ) then
		function = listGetPtr( l->tail )
	end if
end function

function listGetNext( byval p as any ptr ) as any ptr
	dim as LISTNODE ptr nxt = any
	nxt = listGetNode( p )->next
	if( nxt ) then
		function = listGetPtr( nxt )
	end if
end function

function listGetPrev( byval p as any ptr ) as any ptr
	dim as LISTNODE ptr prv = any
	prv = listGetNode( p )->prev
	if( prv ) then
		function = listGetPtr( prv )
	end if
end function

function listAppend( byval l as TLIST ptr ) as any ptr
	dim as LISTNODE ptr node = any

	node = callocate( l->nodesize )
	node->next = NULL
	node->prev = l->tail
	if( l->tail ) then
		l->tail->next = node
	else
		l->head = node
	end if
	l->tail = node

	function = listGetPtr( node )
end function

sub listDelete( byval l as TLIST ptr, byval p as any ptr )
	dim as LISTNODE ptr node = any, nxt = any, prv = any

	if( p = NULL ) then exit sub
	node = listGetNode( p )

	nxt = node->next
	prv = node->prev
	if( prv ) then
		prv->next = nxt
	else
		l->head = nxt
	end if
	if( nxt ) then
		nxt->prev = prv
	else
		l->tail = prv
	end if

	deallocate( node )
end sub

function listCount( byval l as TLIST ptr ) as integer
	dim as integer count = any
	dim as LISTNODE ptr node = any

	count = 0
	node = l->head
	while( node )
		count += 1
		node = node->next
	wend

	function = count
end function

sub listInit( byval l as TLIST ptr, byval unit as integer )
	l->head = NULL
	l->tail = NULL
	l->nodesize = sizeof( LISTNODE ) + unit
end sub

sub listEnd( byval l as TLIST ptr )
	dim as LISTNODE ptr node = any, nxt = any
	node = l->head
	while( node )
		nxt = node->next
		deallocate( node )
		node = nxt
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function hashHash( byval s as zstring ptr ) as uinteger
	dim as uinteger hash = any

	hash = 5381
	while( (*s)[0] )
		hash = (*s)[0] + (hash shl 5) - hash
		s += 1
	wend

	function = hash
end function

private function hashHash2( byval s as zstring ptr ) as uinteger
	dim as uinteger hash = any

	hash = 0
	while( (*s)[0] )
		hash = (*s)[0] + (hash shl 6) + (hash shl 16) - hash
		s += 1
	wend

	function = hash
end function

private sub hAllocTable( byval h as THASH ptr )
	'' They must be zeroed, because NULL instead of a string indicates
	'' unused items
	h->items = callocate( h->room * sizeof( THASHITEM ) )
end sub

private sub hGrowTable( byval h as THASH ptr )
	dim as THASHITEM ptr old = any
	dim as integer oldroom = any

	old = h->items
	oldroom = h->room

	h->resizes += 1
	h->room shl= 1
	hAllocTable( h )

	'' Insert all used items from the old table into the new one.
	'' This will redistribute everything using the new h->room.
	for item as THASHITEM ptr = old to (old + (oldroom - 1))
		if( item->s ) then
			'' Yep, this is recursive, but since the table is
			'' larger by now, we won't recurse in here again.
			*hashLookup( h, item->s, item->hash ) = *item
		end if
	next

	deallocate( old )
end sub

function hashLookup _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval hash as uinteger _
	) as THASHITEM ptr

	dim as uinteger roommask = any, i = any, stepsize = any
	dim as THASHITEM ptr item = any

	'' Enlarge the hash map when >= 75% is used up, for better lookup
	'' performance (it's easier to find free items if there are many; i.e.
	'' less collisions), and besides there always must be free slots,
	'' otherwise a lookup could end up in an infinite loop.
	if( (h->count * 4) >= (h->room * 3) ) then
		hGrowTable( h )
	end if

	h->lookups += 1

	roommask = h->room - 1

	'' First probe
	i = hash and roommask
	item = h->items + i

	'' Found unused item with first probe?
	if( item->s = NULL ) then
		h->perfects += 1
		return item
	end if

	'' Item is used. Is it the correct string?
	if( item->hash = hash ) then
		if( *item->s = *s ) then
			return item
		end if
	end if

	'' The first probe reached an item containing the wrong string.
	'' The collision is resolved by stepping through items until a
	'' free item or the look-for string is found.
	''
	'' The step size is calculated based on a 2nd hash value. It is or'ed
	'' with 1 to make sure it's odd, so all items will eventually be
	'' reached, because h->room always is a power of 2.
	stepsize = (hashHash2( s ) and roommask) or 1

	do
#if 0
		'' Collisions happen when both hashes are equal mod table size
		print "** COLLISION at " + hex( i ) + ": " + *s + _
			" with existing " + *item->s
#endif
		h->collisions += 1

		i = (i + stepsize) and roommask
		item = h->items + i

		'' Found unused item?
		'' The string is not in the hash, or it would have been found before.
		if( item->s = NULL ) then
			exit do
		end if

		'' Item is used. Is it the correct string?
		if( item->hash = hash ) then
			if( *item->s = *s ) then
				exit do
			end if
		end if
	loop

	function = item
end function

sub hashAdd _
	( _
		byval h as THASH ptr, _
		byval item as THASHITEM ptr, _
		byval hash as uinteger, _
		byval s as zstring ptr, _
		byval dat as any ptr _
	)

	item->s = s
	item->hash = hash
	item->data = dat
	h->count += 1

end sub

sub hashAddOverwrite _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval dat as any ptr _
	)

	dim as uinteger hash = any
	dim as THASHITEM ptr item = any

	hash = hashHash( s )
	item = hashLookup( h, s, hash )

	'' Update count if entry doesn't exist yet
	if( item->s = NULL ) then
		h->count += 1
	end if

	'' Overwrite existing entry (if any)
	item->s = s
	item->hash = hash
	item->data = dat

end sub

sub hashInit( byval h as THASH ptr, byval exponent as integer )
	h->count = 0
	h->room = 1 shl exponent
	h->resizes = 0
	h->lookups = 0
	h->perfects = 0
	h->collisions = 0
	hAllocTable( h )
end sub

sub hashEnd( byval h as THASH ptr )
	deallocate( h->items )
end sub

sub hashStats( byval h as THASH ptr, byref prefix as string )
	print using "  " + prefix + " hash: " + _
		"&/& hits (&%), &/& used (&%), & resizes"; _
		h->perfects; h->lookups; _
		cint( (100 / h->lookups) * h->perfects ); _
		h->count; h->room; _
		cint( (100 / h->room) * h->count ); _
		h->resizes
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Searches backwards for the last '.' while still behind '/' or '\'.
private function hFindExtBegin( byref path as string ) as integer
	for i as integer = len( path )-1 to 0 step -1
		select case( path[i] )
		case asc( "." )
			return i
		case asc( "/" ), asc( "\" )
			exit for
		end select
	next
	function = len( path )
end function

function pathStripExt( byref path as string ) as string
	function = left( path, hFindExtBegin( path ) )
end function

function pathExtOnly( byref path as string ) as string
	function = right( path, len( path ) - hFindExtBegin( path ) - 1 )
end function

private function hFindFileName( byref path as string ) as integer
	for i as integer = len( path )-1 to 0 step -1
		select case( path[i] )
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
		case asc( "\" ), asc( "/" )
#else
		case asc( "/" )
#endif
			return i + 1
		end select
	next
end function

function pathOnly( byref path as string ) as string
	function = left( path, hFindFileName( path ) )
end function

function pathAddDiv( byref path as string ) as string
	dim as string s
	dim as integer length = any

	s = path
	length = len( s )

	if( length > 0 ) then
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
		select case( s[length-1] )
		case asc( "\" ), asc( "/" )

		case else
			s += "\"
		end select
#else
		if( s[length-1] <> asc( "/" ) ) then
			s += "/"
		end if
#endif
	end if

	function = s
end function

private function pathGetRootLength( byref s as string ) as integer
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
	if( len( s ) >= 3 ) then
		'' x:\...
		if( s[1] = asc( ":" ) ) then
			select case( s[2] )
			case asc( "/" ), asc( "\" )
				return 3
			end select
		end if
	end if
#ifdef __FB_WIN32__
	'' UNC paths
	if( len( s ) >= 2 ) then
		'' \\...
		if( s[0] = asc( "\" ) ) then
			if( s[1] = asc( "\" ) ) then
				return 2
			end if
		end if
	end if
#endif
#else
	if( len( s ) >= 1 ) then
		'' /...
		if( s[0] = asc( "/" ) ) then
			return 1
		end if
	end if
#endif
end function

function pathStripLastComponent( byref path as string ) as string
	function = pathOnly( left( path, len( path ) - 1 ) )
end function

function pathFindCommonBase _
	( _
		byref aorig as string, _
		byref borig as string _
	) as string

	dim as string a, b
	dim as integer aroot = any, broot = any, alen = any, blen = any

	a = aorig
	b = borig
	aroot = pathGetRootLength( a )
	broot = pathGetRootLength( b )

	do
		alen = len( a )
		blen = len( b )

		if( (alen < aroot) or (blen < broot) ) then
			exit do
		end if

		if( alen = blen ) then
			if( a = b ) then
				return a
			end if
		end if

		if( alen > blen ) then
			a = pathStripLastComponent( a )
		else
			b = pathStripLastComponent( b )
		end if
	loop

end function

function pathStripCommonBase _
	( _
		byref a as string, _
		byref b as string _
	) as string
	function = right( a, len( a ) - len( pathFindCommonBase( a, b ) ) )
end function

'' Component stack for the path solver
type PATHSOLVER
	p	as integer ptr
	room	as integer
	top	as integer
end type

dim shared as PATHSOLVER solver

private sub solverInit( )
	solver.p = NULL
	solver.room = 0
	solver.top = -1
end sub

private sub solverEnd( )
	deallocate( solver.p )
end sub

private sub solverPush( byval w as integer )
	solver.top += 1
	if( solver.top >= solver.room ) then
		solver.room += 128
		solver.p = reallocate( solver.p, sizeof( integer ) * solver.room )
	end if
	solver.p[solver.top] = w
end sub

private function solverPop( ) as integer
	if( solver.top > 0 ) then
		solver.top -= 1
	end if
	function = solver.p[solver.top]
end function

'' Turns a relative path into an absolute path
function pathMakeAbsolute( byref path as string ) as string
	if( pathGetRootLength( path ) = 0 ) then
		function = pathAddDiv( curdir( ) ) + path
	else
		function = path
	end if
end function

'' Resolves .'s and ..'s in the path,
'' normalizes path separators to the host standard.
function pathNormalize( byref path as string ) as string
	dim as integer rootlen = any, dots = any, chars = any, r = any, w = any
	dim as string s

	rootlen = pathGetRootLength( path )
	if( rootlen = 0 ) then
		return path
	end if

	'' r: read position, w: write position
	'' r reads ahead, while w slowly writes out the result.
	'' First r and w stay together, but as soon as r finds a .., w is set
	'' back a bit, right in front of the path component it wrote last, so
	'' that the component is dropped (it will be overwritten by following
	'' components).
	'' The stack is needed to be able to skip back over multiple components
	'' in succession, for example in 'aa/bb/../..'.

	'' r and w start out behind the root path (/ or C:\ etc.) so that it's
	'' not touched. The begin of the first component after the root path
	'' must be on the stack to be able to skip back to it (although the
	'' begin of the root path itself, 0, is not on the stack, so it can't
	'' be removed with a '..').
	solverInit( )
	solverPush( rootlen )

	s = path
	dots = 0 '' Number of .'s in the current component
	chars = 0 '' Number of chars in the current component
	w = rootlen

	for r = rootlen to len( s )-1
		select case( s[r] )
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
		case asc( "\" ), asc( "/" )
#else
		case asc( "/" )
#endif
			'' Component closed: check whether it was /./ or /../
			select case( dots )
			case 1    '' /./
				'' Ignore: don't write this /, and remove the .
				w -= 1

			case 2    '' /../
				'' Go back to the begin of the component this
				'' '..' refers to
				w = solverPop( )

			case else
				if( chars = 0 ) then
					'' // (Ignore: don't write this /)
				else
					'' Write this /
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
					s[w] = asc( "\" ) '' This also normalizes / to \
#else
					s[w] = asc( "/" )
#endif
					'' New component starts behind this /
					w += 1
					'' Remember this begin position so
					'' w can be reset to it during '..'.
					solverPush( w )
				end if

			end select

			dots = 0
			chars = 0

		case asc( "." )
			dots += 1
			chars += 1
			s[w] = s[r]
			w += 1

		case else
			dots = 0
			chars += 1
			s[w] = s[r]
			w += 1

		end select
	next

	solverEnd( )
	function = left( s, w )
end function

function hFileExists( byref file as string ) as integer
	dim as integer f = any
	f = freefile( )
	if( open( file, for binary, access read, as #f ) = 0 ) then
		close #f
		function = TRUE
	else
		function = FALSE
	end if
end function

type DIRNODE
	next		as DIRNODE ptr
	path		as string
end type

type DIRQUEUE
	head		as DIRNODE ptr
	tail		as DIRNODE ptr
end type

dim shared as DIRQUEUE dirs

private sub dirsAppend( byref path as string )
	dim as DIRNODE ptr node = any

	node = callocate( sizeof( DIRNODE ) )
	node->path = pathAddDiv( path )

	if( dirs.tail ) then
		dirs.tail->next = node
	end if
	dirs.tail = node
	if( dirs.head = NULL ) then
		dirs.head = node
	end if
end sub

private sub dirsDropHead( )
	dim as DIRNODE ptr node = any
	if( dirs.head ) then
		node = dirs.head
		dirs.head = node->next
		if( dirs.head = NULL ) then
			dirs.tail = NULL
		end if
		node->path = ""
		deallocate( node )
	end if
end sub

private sub hScanParent _
	( _
		byref parent as string, _
		byval resultlist as TLIST ptr _
	)

	dim as string found

	'' Scan for *.h files
	found = dir( parent + "*.h", fbNormal )
	while( len( found ) > 0 )
		'' Add the file name to the result list
		*cptr( string ptr, listAppend( resultlist ) ) = parent + found

		found = dir( )
	wend

	'' Scan for subdirectories
	found = dir( parent + "*", fbDirectory or fbReadOnly )
	while( len( found ) > 0 )
		select case( found )
		case ".", ".."
			'' Ignore these special subdirectories

		case else
			'' Remember the subdirectory for further scanning
			dirsAppend( parent + found )
		end select

		found = dir( )
	wend

end sub

sub hScanDirectoryForH _
	( _
		byref rootdir as string, _
		byval resultlist as TLIST ptr _
	)

	dirsAppend( rootdir )

	print "scanning tree for *.h files: '" + dirs.head->path + "'"

	'' Work off the queue -- each subdir scan can append new subdirs
	while( dirs.head )
		hScanParent( dirs.head->path, resultlist )
		dirsDropHead( )
	wend

end sub

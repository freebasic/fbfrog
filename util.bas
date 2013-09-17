#include once "fbfrog.bi"
#include once "crt.bi"
#include once "dir.bi"

sub oops( byval message as zstring ptr )
	print "oops, " + *message
	end 1
end sub

private sub hCalcErrorLine _
	( _
		byval file as ASTNODE ptr, _
		byval linenum as integer, _
		byval column as integer, _
		byval limit as integer, _
		byref s as string, _
		byref offset as integer _
	)

	s = lexPeekLine( file, linenum )

	'' Line too long to fit in console?
	if( len( s ) > limit ) then
		var shift = 0

		if( column < ((limit * 3) / 4) ) then
			'' Offset is still well visible, so align to left.
			s = left( s, limit )
		else
			'' Must scroll the line to the left (and the offset too),
			'' to make the location visible.
			'' a) center it, if the string is long enough for this,
			'' b) or align to the right.

			'' Enough chars behind the offset to fill up a half?
			var half = limit / 2
			if( (len( s ) - column) >= half ) then
				'' Center: shift left to align offset to visible boundary,
				shift = column - limit
				'' and shift further to reach the middle.
				shift += half
				s = mid( s, shift+1, limit )
			else
				'' Right align:
				shift = len( s ) - limit
				s = right( s, limit )
			end if
		end if

		offset = column - shift
	else
		offset = column
	end if

end sub

'' Prints out a message like this:
'' filename.bas(123): duplicate definition of 'i'
''          dim i as integer
''              ^
sub hReportLocation _
	( _
		byval location as TKLOCATION ptr, _
		byval message as zstring ptr, _
		byval more_context as integer _
	)

	print *location->file->comment + "(" & (location->linenum + 1) & "): " + *message

	'' Determine how many chars can be printed for the error line:
	'' Normally we can fill a line in the console, so get the console width.
	dim as integer limit = loword( width( ) ) - 1
	if( limit < 0 ) then
		limit = 0
	end if

	var linecount = lexCountLines( location->file )

	'' Show the error line and maybe some extra lines above and below it,
	'' for more context, with line numbers prefixed to them:
	''    9:        do
	''   10:            dim i as integer
	''                      ^
	''   11:            i = 123
	''   12:            print i
	dim as string linenums(-2 to 2)

	dim as integer min, max

	if( more_context ) then
		min = location->linenum + lbound( linenums )
		if( min < 0 ) then min = 0
		min -= location->linenum

		max = location->linenum + ubound( linenums )
		if( max >= linecount ) then max = linecount - 1
		max -= location->linenum
	else
		min = 0
		max = 0
	end if

	for i as integer = min to max
		linenums(i) = str( location->linenum + i + 1 )
		if( len( linenums(i) ) < len( str( location->linenum + 1 ) ) ) then
			linenums(i) = " " + linenums(i)
		end if
		linenums(i) += ": "
	next

	'' Indentation and line numbers reduce the available width for the error line
	limit -= len( linenums(0) )
	limit -= 4

	for i as integer = min to max
		dim s as string
		dim offset as integer
		hCalcErrorLine( location->file, location->linenum + i, location->column, limit, s, offset )

		print "    " + linenums(i) + s

		if( i = 0 ) then
			print space( 4 + len( linenums(i) ) + offset ) + "^" + string( location->length - 1, "~" )
		end if
	next

end sub

function strDuplicate( byval s as zstring ptr ) as zstring ptr
	dim as zstring ptr p = any
	if( s ) then
		p = callocate( len( *s ) + 1 )
		*p = *s
		function = p
	else
		function = NULL
	end if
end function

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

function strMakePrintable( byref a as string ) as string
	dim b as string

	for i as integer = 0 to len( a )-1
		select case( a[i] )
		case CH_LF  : b += "\n"
		case CH_CR  : b += "\r"
		case CH_TAB : b += "\t"
		case is < 32, 127 : b += "?"
		case else   : b += chr( a[i] )
		end select
	next

	function = b
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

function strContainsNonHexDigits( byval s as zstring ptr ) as integer
	dim as ubyte ptr p = s

	if( p ) then
		do
			select case( p[0] )
			case 0
				exit do
			case CH_0 to CH_9, CH_A to CH_F, CH_L_A to CH_L_F

			case else
				return TRUE
			end select

			p += 1
		loop
	end if

	function = FALSE
end function

function strContainsNonOctDigits( byval s as zstring ptr ) as integer
	dim as ubyte ptr p = s

	if( p ) then
		do
			select case( p[0] )
			case 0
				exit do
			case CH_0 to CH_7

			case else
				return TRUE
			end select

			p += 1
		loop
	end if

	function = FALSE
end function

function hMakePrettyByteSize( byval size as uinteger ) as string
	'' Determine the max power of 1024 in the number, and the remainder
	'' corresponding to that part
	'' (1024 is the unit of bytes/KiB/MiB/GiB)
	''         0 = power of 1024 = 0 (bytes), size = 0, remainder = 0
	''        10 = power of 1024 = 0 (bytes), size = 10
	''      1024 = power of 1024 = 1 (KiB), size = 1, remainder = 0
	''      2048 = power of 1024 = 1 (KiB), size = 2, remainder = 0
	'' 1024*7+13 = power of 1024 = 1 (KiB), size = 7, remainder = 13
	'' 3146753 = 1024^2 * 3 + 1025 = MiB, size = 0, remainder = 1
	'' 1024^a * b + c  =  power of 1024 = a, size = b, remainder = c mod 1024^a
	dim as uinteger r
	var i = 0
	while( size >= 1024 )
		r = size and 1023
		size shr= 10
		i += 1
	wend

	'' This will use 4 digits at most, because size will be < 1024 by now
	var s = str( size )

	'' If there are less than 3 digits, add a '.' dot and the remainder,
	'' so that e.g. 1536 shows up as 1.50 KiB, and (1024 * 10 + 512) shows
	'' up as 10.5 KiB.
	if( size < 10 ) then
		'' x.xx
		r = (r * 100) shr 10
		if( r > 0 ) then
			'' .0x or .xx
			s += "."
			if( r < 10 ) then s += "0"
			s &= r
		end if
	elseif( size < 100 ) then
		'' xx.x
		r = (r * 10) shr 10
		if( r > 0 ) then
			'' .x
			s += "." & r
		end if
	end if

	static as zstring ptr units(0 to 3) = _
	{ _
		@"byte", @"KiB", @"MiB", @"GiB" _
	}

	'' Unit string
	s += " " + *units(i)

	'' byte -> bytes, unless the number is 1
	if( (i = 0) and (size <> 1) ) then
		s += "s"
	end if

	function = s
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' Generic linked list

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
'' Generic hash table (open addressing/closed hashing), based on GCC's libcpp's
'' hash table.
''
'' Note: No deletions possible due to the collision resolution adding duplicates
'' to other entries in the table, instead of adding to the same bucket.
'' Each duplicate can be reached by following through the chain of steps
'' indicated by hashHash2(), the first free entry reached indicates the end of
'' the chain -- that's where duplicates are inserted. Removing an entry from
'' this chain would cause the following entries to become unreachable/lost,
'' as the free item in the middle would appear as the end of the chain now.

function hashHash( byval s as zstring ptr ) as uinteger
	dim as uinteger hash = 5381
	while( (*s)[0] )
		hash = (*s)[0] + (hash shl 5) - hash
		s += 1
	wend
	function = hash
end function

private function hashHash2( byval s as zstring ptr ) as uinteger
	dim as uinteger hash = 0
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
	var old = h->items
	var oldroom = h->room

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

	'' Enlarge the hash map when >= 75% is used up, for better lookup
	'' performance (it's easier to find free items if there are many; i.e.
	'' less collisions), and besides there always must be free slots,
	'' otherwise a lookup could end up in an infinite loop.
	if( (h->count * 4) >= (h->room * 3) ) then
		hGrowTable( h )
	end if

	h->lookups += 1

	dim as uinteger roommask = h->room - 1

	'' First probe
	var i = hash and roommask
	var item = h->items + i

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
	var stepsize = (hashHash2( s ) and roommask) or 1

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

	if( h->duplicate_strings ) then
		s = strDuplicate( s )
	end if

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

	var hash = hashHash( s )
	var item = hashLookup( h, s, hash )

	'' Update count if entry doesn't exist yet
	if( item->s = NULL ) then
		h->count += 1
	end if

	if( h->duplicate_strings ) then
		'' Free existing string if overwriting
		if( item->s ) then
			deallocate( item->s )
		end if

		s = strDuplicate( s )
	end if

	'' Overwrite existing entry (if any)
	item->s = s
	item->hash = hash
	item->data = dat

end sub

sub hashInit _
	( _
		byval h as THASH ptr, _
		byval exponent as integer, _
		byval duplicate_strings as integer _
	)

	h->count = 0
	h->room = 1 shl exponent
	h->resizes = 0
	h->lookups = 0
	h->perfects = 0
	h->collisions = 0
	h->duplicate_strings = duplicate_strings
	hAllocTable( h )

end sub

sub hashEnd( byval h as THASH ptr )
	'' Free each item's string if they were duplicated
	if( h->duplicate_strings ) then
		var i = h->items
		var limit = i + h->room
		while( i < limit )
			deallocate( i->s )
			i += 1
		wend
	end if

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
'' Path/file name handling functions

'' Searches backwards for the last '.' while still behind '/' or '\'.
private function hFindExtBegin( byref path as string ) as integer
	for i as integer = len( path )-1 to 0 step -1
		select case( path[i] )
		case asc( "." )
			return i
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
		case asc( "\" ), asc( "/" )
#else
		case asc( "/" )
#endif
			exit for
		end select
	next
	function = len( path )
end function

function pathStripExt( byref path as string ) as string
	function = left( path, hFindExtBegin( path ) )
end function

function pathExtOnly( byref path as string ) as string
	'' -1 to strip the '.' in front of the file extension
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

function pathStrip( byref path as string ) as string
	function = right( path, len( path ) - hFindFileName( path ) )
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

function pathIsAbsolute( byref s as string ) as integer
	function = (pathGetRootLength( s ) > 0)
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

'' Turns a relative path into an absolute path
function pathMakeAbsolute( byref path as string ) as string
	if( pathIsAbsolute( path ) ) then
		function = path
	else
		function = pathAddDiv( curdir( ) ) + path
	end if
end function

function hExepath( ) as string
	function = pathAddDiv( exepath( ) )
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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' Directory tree search

function hDirExists( byref path as string ) as integer
	var fixed = path
	if( right( fixed, len( PATHDIV ) ) = PATHDIV ) then
		fixed = left( fixed, len( fixed ) - len( PATHDIV ) )
	end if
	function = (dir( fixed, fbDirectory ) <> "")
end function

function hFileExists( byref path as string ) as integer
	function = (dir( path, fbNormal ) <> "")
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
		byref filepattern as string, _
		byval resultlist as TLIST ptr _
	)

	dim as string found

	'' Scan for *.h files
	found = dir( parent + filepattern, fbNormal )
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

sub hScanDirectory _
	( _
		byref rootdir as string, _
		byref filepattern as string, _
		byval resultlist as TLIST ptr _
	)

	dirsAppend( rootdir )

	print "scanning tree for " + filepattern + " files: '" + dirs.head->path + "'"

	'' Work off the queue -- each subdir scan can append new subdirs
	while( dirs.head )
		hScanParent( dirs.head->path, filepattern, resultlist )
		dirsDropHead( )
	wend

end sub

function hShell( byref ln as string ) as integer
	print "$ " + ln
	var result = shell( ln )
	if( result = 0 ) then
		function = TRUE
	elseif( result = -1 ) then
		print "command not found: '" + ln + "'"
	else
		print "'" + ln + "' terminated with exit code " + str( result )
	end if
end function

sub hMkdir( byref path as string )
	if( mkdir( path ) ) then
	end if
end sub

sub hMkdirP( byref path as string )
	'' Given a path like this:
	''    foo/bar/baz
	'' Do these mkdir()'s:
	''    foo
	''    foo/bar
	''    foo/bar/baz

	for i as integer = 0 to len( path )-1
		select case( path[i] )
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
		case asc( "\" ), asc( "/" )
#else
		case asc( "/" )
#endif
			hMkdir( left( path, i ) )
		end select
	next

	hMkdir( path )
end sub

sub hKill( byref path as string )
	if( kill( path ) ) then
	end if
end sub

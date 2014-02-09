#include once "fbfrog.bi"
#include once "crt.bi"
#include once "dir.bi"

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

namespace filebuffers
	dim shared hash as THASH
	dim shared nodes as TLIST '' FILEBUFFER
end namespace

sub filebufferInit( )
	hashInit( @filebuffers.hash, 8, FALSE )
	listInit( @filebuffers.nodes, sizeof( FILEBUFFER ) )
end sub

function hDumpFileBuffer( byval file as FILEBUFFER ptr ) as string
	var s = *file->name + "("

	if( file->is_file ) then
		s += "is a file, "
	end if

	if( file->buffer ) then
		var realsize = file->size - 1  '' minus the extra null terminator
		s += realsize & " byte"
		if( realsize <> 1 ) then s += "s"
		s += " loaded, "
	end if

	s += file->lines & " line"
	if( file->lines <> 1 ) then s += "s"
	s += ")"

	function = s
end function

function filebufferAdd( byval filename as zstring ptr ) as FILEBUFFER ptr
	var hash = hashHash( filename )
	var item = hashLookup( @filebuffers.hash, filename, hash )

	'' Doesn't exist yet?
	if( item->s = NULL ) then
		dim as FILEBUFFER ptr file = listAppend( @filebuffers.nodes )
		file->name = strDuplicate( filename )
		file->is_file = FALSE
		file->buffer = NULL
		file->size = 0
		file->lines = 0
		hashAdd( @filebuffers.hash, item, hash, file->name, file )
	end if

	function = item->data
end function

private function hLoadFile _
	( _
		byval filename as zstring ptr, _
		byval srcloc as TKLOCATION ptr, _
		byref size as integer _
	) as ubyte ptr

	'' Read in the whole file content
	var f = freefile( )
	if( open( *filename, for binary, access read, as #f ) ) then
		oopsLocation( srcloc, "could not open file: '" + *filename + "'" )
	end if

	dim as ulongint filesize = lof( f )
	if( filesize > &h40000000 ) then
		oopsLocation( srcloc, "a header file bigger than 1 GiB? no way..." )
	end if

	'' An extra 0 byte at the end of the buffer so we can look ahead
	'' without bound checks, and don't need to give special treatment
	'' to empty files.
	dim as integer sizetoload = filesize
	size = sizetoload + 1
	dim as ubyte ptr buffer = callocate( size )

	if( sizetoload > 0 ) then
		var sizeloaded = 0
		var result = get( #f, , *buffer, sizetoload, sizeloaded )
		if( result or (sizeloaded <> sizetoload) ) then
			oopsLocation( srcloc, "file I/O failed" )
		end if
	end if

	close #f

	'' Currently tokens store text as null-terminated strings, so they
	'' can't allow embedded nulls, and null also indicates EOF to the lexer.
	for i as integer = 0 to sizetoload-1
		if( buffer[i] = 0 ) then
			oopsLocation( srcloc, "file '" + *filename + "' has embedded nulls, please fix that first!" )
		end if
	next

	function = buffer
end function

function filebufferFromFile( byval filename as zstring ptr, byval srcloc as TKLOCATION ptr ) as FILEBUFFER ptr
	var file = filebufferAdd( filename )

	'' Load if not cached yet
	if( file->buffer = NULL ) then
		assert( file->buffer = NULL )
		file->is_file = TRUE
		file->buffer = hLoadFile( file->name, srcloc, file->size )
	end if

	function = file
end function

function filebufferFromZstring( byval filename as zstring ptr, byval s as zstring ptr ) as FILEBUFFER ptr
	var file = filebufferAdd( filename )

	'' Load if not cached yet
	if( file->buffer = NULL ) then
		assert( file->buffer = NULL )
		file->buffer = strDuplicate( s )
		file->size = len( *s ) + 1
	end if

	function = file
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub oops( byval message as zstring ptr )
	print "oops, " + *message
	end 1
end sub

function hDumpLocation( byval location as TKLOCATION ptr ) as string
	if( location->file ) then
		var s = "location("
		s += hDumpFileBuffer( location->file ) & ", "
		s += "line " & location->linenum + 1 & ", "
		s += "column " & location->column + 1 & ", "
		s += "length " & location->length
		s += ")"
		function = s
	else
		function = "(no location info)"
	end if
end function

private sub hCalcErrorLine _
	( _
		byval column as integer, _
		byval limit as integer, _
		byref s as string, _
		byref offset as integer _
	)

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

function hConsoleWidth( ) as integer
	dim as integer w = loword( width( ) )
	if( w < 0 ) then
		w = 0
	end if
	function = w
end function

'' Prints an error message like this to the console:
'' filename.bas(123): duplicate definition of 'foobar'
''          dim foobar as integer
''              ^~~~~~
sub hReport _
	( _
		byval location as TKLOCATION ptr, _
		byval message as zstring ptr, _
		byval more_context as integer _
	)

	assert( location )
	assert( location->file )
	assert( location->file->name )

	print *location->file->name + "(" & (location->linenum + 1) & "): " + *message

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
		if( max >= location->file->lines ) then max = location->file->lines - 1
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

	var maxwidth = hConsoleWidth( )

	'' Indentation and line numbers reduce the width available for the error line
	maxwidth -= len( linenums(0) )
	maxwidth -= 4

	for i as integer = min to max
		dim offset as integer
		var sourceline = lexPeekLine( location->file, location->linenum + i )
		hCalcErrorLine( location->column, maxwidth, sourceline, offset )
		print "    " + linenums(i) + sourceline
		if( i = 0 ) then
			'' ^~~~ error marker
			print space( 4 + len( linenums(i) ) + offset ) + _
			      "^" + string( location->length - 1, "~" )
		end if
	next
end sub

sub oopsLocation( byval location as TKLOCATION ptr, byval message as zstring ptr )
	hReport( location, message, TRUE )
	end 1
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

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

sub strSplit _
	( _
		byref origs as string, _
		byref delimiter as string, _
		byref l as string, _
		byref r as string _
	)

	var s = origs
	var leftlen = instr( s, delimiter ) - 1

	if( leftlen >= 0 ) then
		l = left( s, leftlen )
		r = right( s, len( s ) - leftlen - len( delimiter ) )
	else
		l = s
		r = ""
	end if

end sub

function strTrim( byref s as string ) as string
	function = trim( s, any !" \t" )
end function

'' Checks whether a string starts with and ends in [double-]quotes.
private function strIsQuoted( byref s as string ) as integer
	var last = len( s ) - 1
	if( last >= 1 ) then
		function = ((s[0] = CH_DQUOTE) and (s[last] = CH_DQUOTE)) or _
		           ((s[0] = CH_QUOTE ) and (s[last] = CH_QUOTE ))
	end if
end function

function strUnquote( byref s as string ) as string
	if( strIsQuoted( s ) ) then
		function = mid( s, 2, len( s ) - 2 )
	else
		function = s
	end if
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

function strIsValidSymbolId( byval s as zstring ptr ) as integer
	var i = 0
	do
		select case as const( (*s)[0] )
		case 0
			exit do

		case CH_A to CH_Z, CH_L_A to CH_L_Z, CH_UNDERSCORE
			'' A-Z, a-z, _ are allowed

		case CH_0 to CH_9
			'' Numbers are allowed but not at the front
			if( i = 0 ) then
				exit function
			end if

		case else
			exit function
		end select

		s += 1
		i += 1
	loop

	function = TRUE
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

function hashContains _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval hash as uinteger _
	) as integer
	function = (hashLookup( h, s, hash )->s <> NULL)
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

'' Add entry, overwriting previous user data stored in that slot
function hashAddOverwrite _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval dat as any ptr _
	) as THASHITEM ptr

	var hash = hashHash( s )
	var item = hashLookup( h, s, hash )

	if( item->s ) then
		'' Already exists
		assert( *item->s = *s )
		assert( item->hash = hash )
	else
		'' New entry
		if( h->duplicate_strings ) then
			item->s = strDuplicate( s )
		else
			item->s = s
		end if
		item->hash = hash
		h->count += 1
	end if

	item->data = dat

	function = item
end function

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

sub hashDump( byval h as THASH ptr )
	print "hash: " & h->count & "/" & h->room & " slots used"
	for i as integer = 0 to h->room-1
		with( h->items[i] )
			print "    " & i & ": ";
			if( .s ) then
				print "hash=" + hex( .hash ) + ", s=""" + *.s + """, data=" + hex( .data )
			else
				print "(free)"
			end if
		end with
	next
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

dim shared as zstring ptr fbkeywords(0 to ...) => _
{ _
	@"ABS", _
	@"ABSTRACT", _
	@"ACCESS", _
	@"ACOS", _
	@"ALIAS", _
	@"ALLOCATE", _
	@"AND", _
	@"ANDALSO", _
	@"ANY", _
	@"APPEND", _
	@"AS", _
	@"ASC", _
	@"ASIN", _
	@"ASM", _
	@"ASSERT", _
	@"ASSERTWARN", _
	@"ATAN2", _
	@"ATN", _
	@"BASE", _
	@"BEEP", _
	@"BIN", _
	@"BINARY", _
	@"BIT", _
	@"BITRESET", _
	@"BITSET", _
	@"BLOAD", _
	@"BSAVE", _
	@"BYREF", _
	@"BYTE", _
	@"BYVAL", _
	@"CALL", _
	@"CALLOCATE", _
	@"CASE", _
	@"CAST", _
	@"CBYTE", _
	@"CDBL", _
	@"CDECL", _
	@"CHAIN", _
	@"CHDIR", _
	@"CHR", _
	@"CINT", _
	@"CIRCLE", _
	@"CLASS", _
	@"CLEAR", _
	@"CLNG", _
	@"CLNGINT", _
	@"CLOSE", _
	@"CLS", _
	@"COLOR", _
	@"COMMAND", _
	@"COMMON", _
	@"CONDBROADCAST", _
	@"CONDCREATE", _
	@"CONDDESTROY", _
	@"CONDSIGNAL", _
	@"CONDWAIT", _
	@"CONST", _
	@"CONSTRUCTOR", _
	@"CONTINUE", _
	@"COS", _
	@"CPTR", _
	@"CSHORT", _
	@"CSIGN", _
	@"CSNG", _
	@"CSRLIN", _
	@"CUBYTE", _
	@"CUINT", _
	@"CULNG", _
	@"CULNGINT", _
	@"CUNSG", _
	@"CURDIR", _
	@"CUSHORT", _
	@"CVD", _
	@"CVI", _
	@"CVL", _
	@"CVLONGINT", _
	@"CVS", _
	@"CVSHORT", _
	@"DATA", _
	@"DATE", _
	@"DEALLOCATE", _
	@"DECLARE", _
	@"DEFBYTE", _
	@"DEFDBL", _
	@"DEFINE", _
	@"DEFINED", _
	@"DEFINT", _
	@"DEFLNG", _
	@"DEFLONGINT", _
	@"DEFSHORT", _
	@"DEFSNG", _
	@"DEFSTR", _
	@"DEFUBYTE", _
	@"DEFUINT", _
	@"DEFULNG", _
	@"DEFULONGINT", _
	@"DEFUSHORT", _
	@"DELETE", _
	@"DESTRUCTOR", _
	@"DIM", _
	@"DIR", _
	@"DO", _
	@"DOUBLE", _
	@"DRAW", _
	@"DYLIBFREE", _
	@"DYLIBLOAD", _
	@"DYLIBSYMBOL", _
	@"DYNAMIC", _
	@"ELSE", _
	@"ELSEIF", _
	@"ENCODING", _
	@"END", _
	@"ENDIF", _
	@"ENDMACRO", _
	@"ENUM", _
	@"ENVIRON", _
	@"EOF", _
	@"EQV", _
	@"ERASE", _
	@"ERFN", _
	@"ERL", _
	@"ERMN", _
	@"ERR", _
	@"ERROR", _
	@"EXEC", _
	@"EXEPATH", _
	@"EXIT", _
	@"EXP", _
	@"EXPLICIT", _
	@"EXPORT", _
	@"EXTENDS", _
	@"EXTERN", _
	@"FIELD", _
	@"FIX", _
	@"FLIP", _
	@"FOR", _
	@"FRAC", _
	@"FRE", _
	@"FREEFILE", _
	@"FUNCTION", _
	@"GET", _
	@"GETJOYSTICK", _
	@"GETKEY", _
	@"GETMOUSE", _
	@"GOSUB", _
	@"GOTO", _
	@"HEX", _
	@"HIBYTE", _
	@"HIWORD", _
	@"IF", _
	@"IFDEF", _
	@"IFNDEF", _
	@"IIF", _
	@"IMAGECONVERTROW", _
	@"IMAGECREATE", _
	@"IMAGEDESTROY", _
	@"IMAGEINFO", _
	@"IMP", _
	@"IMPLEMENTS", _
	@"IMPORT", _
	@"INCLIB", _
	@"INCLUDE", _
	@"INKEY", _
	@"INP", _
	@"INPUT", _
	@"INSTR", _
	@"INSTRREV", _
	@"INT", _
	@"INTEGER", _
	@"IS", _
	@"KILL", _
	@"LANG", _
	@"LBOUND", _
	@"LCASE", _
	@"LEFT", _
	@"LEN", _
	@"LET", _
	@"LIB", _
	@"LIBPATH", _
	@"LINE", _
	@"LOBYTE", _
	@"LOC", _
	@"LOCAL", _
	@"LOCATE", _
	@"LOCK", _
	@"LOF", _
	@"LOG", _
	@"LONG", _
	@"LONGINT", _
	@"LOOP", _
	@"LOWORD", _
	@"LPOS", _
	@"LPRINT", _
	@"LSET", _
	@"LTRIM", _
	@"MACRO", _
	@"MID", _
	@"MKD", _
	@"MKDIR", _
	@"MKI", _
	@"MKL", _
	@"MKLONGINT", _
	@"MKS", _
	@"MKSHORT", _
	@"MOD", _
	@"MULTIKEY", _
	@"MUTEXCREATE", _
	@"MUTEXDESTROY", _
	@"MUTEXLOCK", _
	@"MUTEXUNLOCK", _
	@"NAME", _
	@"NAMESPACE", _
	@"NEW", _
	@"NEXT", _
	@"NOT", _
	@"OBJECT", _
	@"OCT", _
	@"OFFSETOF", _
	@"ON", _
	@"OPEN", _
	@"OPERATOR", _
	@"OPTION", _
	@"OR", _
	@"ORELSE", _
	@"OUT", _
	@"OUTPUT", _
	@"OVERLOAD", _
	@"PAINT", _
	@"PALETTE", _
	@"PASCAL", _
	@"PCOPY", _
	@"PEEK", _
	@"PMAP", _
	@"POINT", _
	@"POINTCOORD", _
	@"POINTER", _
	@"POKE", _
	@"POS", _
	@"PRAGMA", _
	@"PRESERVE", _
	@"PRESET", _
	@"PRINT", _
	@"PRIVATE", _
	@"PROCPTR", _
	@"PROPERTY", _
	@"PROTECTED", _
	@"PSET", _
	@"PTR", _
	@"PUBLIC", _
	@"PUT", _
	@"RANDOM", _
	@"RANDOMIZE", _
	@"READ", _
	@"REALLOCATE", _
	@"REDIM", _
	@"REM", _
	@"RESET", _
	@"RESTORE", _
	@"RESUME", _
	@"RETURN", _
	@"RGB", _
	@"RGBA", _
	@"RIGHT", _
	@"RMDIR", _
	@"RND", _
	@"RSET", _
	@"RTRIM", _
	@"RUN", _
	@"SADD", _
	@"SCOPE", _
	@"SCREEN", _
	@"SCREENCONTROL", _
	@"SCREENCOPY", _
	@"SCREENEVENT", _
	@"SCREENGLPROC", _
	@"SCREENINFO", _
	@"SCREENLIST", _
	@"SCREENLOCK", _
	@"SCREENPTR", _
	@"SCREENRES", _
	@"SCREENSET", _
	@"SCREENSYNC", _
	@"SCREENUNLOCK", _
	@"SEEK", _
	@"SELECT", _
	@"SETDATE", _
	@"SETENVIRON", _
	@"SETMOUSE", _
	@"SETTIME", _
	@"SGN", _
	@"SHARED", _
	@"SHELL", _
	@"SHL", _
	@"SHORT", _
	@"SHR", _
	@"SIN", _
	@"SINGLE", _
	@"SIZEOF", _
	@"SLEEP", _
	@"SPACE", _
	@"SPC", _
	@"SQR", _
	@"STATIC", _
	@"STDCALL", _
	@"STEP", _
	@"STOP", _
	@"STR", _
	@"STRING", _
	@"STRPTR", _
	@"SUB", _
	@"SWAP", _
	@"SYSTEM", _
	@"TAB", _
	@"TAN", _
	@"THEN", _
	@"THREADCALL", _
	@"THREADCREATE", _
	@"THREADWAIT", _
	@"TIME", _
	@"TIMER", _
	@"TO", _
	@"TRIM", _
	@"TYPE", _
	@"TYPEOF", _
	@"UBOUND", _
	@"UBYTE", _
	@"UCASE", _
	@"UINTEGER", _
	@"ULONG", _
	@"ULONGINT", _
	@"UNDEF", _
	@"UNION", _
	@"UNLOCK", _
	@"UNSIGNED", _
	@"UNTIL", _
	@"USHORT", _
	@"USING", _
	@"VAL", _
	@"VALINT", _
	@"VALLNG", _
	@"VALUINT", _
	@"VALULNG", _
	@"VAR", _
	@"VARPTR", _
	@"VA_ARG", _
	@"VA_FIRST", _
	@"VA_NEXT", _
	@"VIEW", _
	@"VIRTUAL", _
	@"WAIT", _
	@"WBIN", _
	@"WCHR", _
	@"WEND", _
	@"WHEX", _
	@"WHILE", _
	@"WIDTH", _
	@"WINDOW", _
	@"WINDOWTITLE", _
	@"WINPUT", _
	@"WITH", _
	@"WOCT", _
	@"WRITE", _
	@"WSPACE", _
	@"WSTR", _
	@"WSTRING", _
	@"XOR", _
	@"ZSTRING", _
	@"__DATE_ISO__", _
	@"__DATE__", _
	@"__FB_BACKEND__", _
	@"__FB_BUILD_DATE__", _
	@"__FB_DEBUG__", _
	@"__FB_ERR__", _
	@"__FB_FPMODE__", _
	@"__FB_FPU__", _
	@"__FB_GCC__", _
	@"__FB_LANG__", _
	@"__FB_LINUX__", _
	@"__FB_MAIN__", _
	@"__FB_MIN_VERSION__", _
	@"__FB_MT__", _
	@"__FB_OPTION_BYVAL__", _
	@"__FB_OPTION_DYNAMIC__", _
	@"__FB_OPTION_ESCAPE__", _
	@"__FB_OPTION_EXPLICIT__", _
	@"__FB_OPTION_GOSUB__", _
	@"__FB_OPTION_PRIVATE__", _
	@"__FB_OUT_DLL__", _
	@"__FB_OUT_EXE__", _
	@"__FB_OUT_LIB__", _
	@"__FB_OUT_OBJ__", _
	@"__FB_SIGNATURE__", _
	@"__FB_UNIX__", _
	@"__FB_VECTORIZE__", _
	@"__FB_VERSION__", _
	@"__FB_VER_MAJOR__", _
	@"__FB_VER_MINOR__", _
	@"__FB_VER_PATCH__", _
	@"__FILE_NQ__", _
	@"__FILE__", _
	@"__FUNCTION_NQ__", _
	@"__FUNCTION__", _
	@"__LINE__", _
	@"__PATH__", _
	@"__TIME__" _
}

dim shared fbkeywordhash as THASH

sub fbkeywordsInit( )
	hashInit( @fbkeywordhash, 16, TRUE )
	for i as integer = lbound( fbkeywords ) to ubound( fbkeywords )
		hashAddOverwrite( @fbkeywordhash, fbkeywords(i), NULL )
	next
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' Directory tree search

function hReadableDirExists( byref path as string ) as integer
	var fixed = path
	if( right( fixed, len( PATHDIV ) ) = PATHDIV ) then
		fixed = left( fixed, len( fixed ) - len( PATHDIV ) )
	end if
	function = (dir( fixed, fbDirectory or fbReadOnly or fbHidden ) <> "")
end function

function hFileExists( byref path as string ) as integer
	function = (dir( path, fbNormal ) <> "")
end function

dim shared as ASTNODE ptr dirs

private sub dirsAppend( byref path as string )
	astAppend( dirs, astNewTEXT( pathAddDiv( path ) ) )
end sub

private function hScanParent _
	( _
		byref parent as string, _
		byref filepattern as string _
	) as ASTNODE ptr

	var files = astNewGROUP( )

	'' Scan for *.h files
	var found = dir( parent + filepattern, fbNormal )
	while( len( found ) > 0 )

		'' Add the file name to the result list
		astAppend( files, astNewTEXT( parent + found ) )

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

	function = files
end function

function hScanDirectory _
	( _
		byref rootdir as string, _
		byref filepattern as string _
	) as ASTNODE ptr

	var files = astNewGROUP( )
	dirs = astNewGROUP( )

	dirsAppend( rootdir )

	if( frog.verbose ) then
		print "scanning tree for " + filepattern + " files: '" + *dirs->head->text + "'"
	end if

	'' Work off the queue -- each subdir scan can append new subdirs
	while( dirs->head )
		astAppend( files, hScanParent( *dirs->head->text, filepattern ) )
		astRemove( dirs, dirs->head )
	wend

	astDelete( dirs )
	dirs = NULL

	function = files
end function

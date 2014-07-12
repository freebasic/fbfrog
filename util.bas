#include once "fbfrog.bi"
#include once "crt.bi"
#include once "dir.bi"

function min( byval a as integer, byval b as integer ) as integer
	if( b < a ) then a = b
	function = a
end function

function max( byval a as integer, byval b as integer ) as integer
	if( b > a ) then a = b
	function = a
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

namespace sourcebuffers
	dim shared hash as THASH
end namespace

sub sourcebuffersInit( )
	hashInit( @sourcebuffers.hash, 8, FALSE )
end sub

function hDumpSourceBuffer( byval file as SOURCEBUFFER ptr ) as string
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

function sourcebufferNew _
	( _
		byval name_ as zstring ptr, _
		byval location as TKLOCATION ptr _
	) as SOURCEBUFFER ptr

	dim as SOURCEBUFFER ptr source = callocate( sizeof( SOURCEBUFFER ) )

	source->name = strDuplicate( name_ )
	if( location ) then
		source->location = *location
	end if

	function = source
end function

private sub hLoadFile( byval source as SOURCEBUFFER ptr )
	'' Read in the whole file content
	var f = freefile( )
	if( open( *source->name, for binary, access read, as #f ) ) then
		oopsLocation( @source->location, "could not open file: '" + *source->name + "'" )
	end if

	dim as ulongint filesize = lof( f )
	if( filesize > &h40000000 ) then
		oopsLocation( @source->location, "a header file bigger than 1 GiB? no way..." )
	end if

	'' An extra 0 byte at the end of the buffer so we can look ahead
	'' without bound checks, and don't need to give special treatment
	'' to empty files.
	dim as integer sizetoload = filesize
	source->size = sizetoload + 1
	source->buffer = callocate( source->size )

	if( sizetoload > 0 ) then
		var sizeloaded = 0
		var result = get( #f, , *source->buffer, sizetoload, sizeloaded )
		if( result or (sizeloaded <> sizetoload) ) then
			oopsLocation( @source->location, "file I/O failed" )
		end if
	end if

	close #f

	'' Currently tokens store text as null-terminated strings, so they
	'' can't allow embedded nulls, and null also indicates EOF to the lexer.
	for i as integer = 0 to sizetoload-1
		if( source->buffer[i] = 0 ) then
			oopsLocation( @source->location, "file '" + *source->name + "' has embedded nulls, please fix that first!" )
		end if
	next
end sub

function sourcebufferFromFile _
	( _
		byval filename as zstring ptr, _
		byval location as TKLOCATION ptr _
	) as SOURCEBUFFER ptr

	'' Caching files based on the file name
	var hash = hashHash( filename )
	var item = hashLookup( @sourcebuffers.hash, filename, hash )

	'' Doesn't exist yet?
	if( item->s = NULL ) then
		var source = sourcebufferNew( filename, location )
		source->is_file = TRUE
		hLoadFile( source )

		hashAdd( @sourcebuffers.hash, item, hash, source->name, source )
	end if

	function = item->data
end function

function sourcebufferFromZstring _
	( _
		byval prettyname as zstring ptr, _
		byval s as zstring ptr, _
		byval location as TKLOCATION ptr _
	) as SOURCEBUFFER ptr

	'' Note: caching zstring source buffers
	'' - They don't have a globally unique identifier
	'' - The string data itself isn't unique either, so comparing that alone
	''   could give false positives
	'' - However, if 2 options have the same pattern then they'll have the
	''   same lexing errors etc. and only the 1st one ever matters. So it
	''   doesn't make a difference if we alias the 2nd one to the 1st.
	'' - But that's only true as long as they're the same option. Otherwise
	''   we may accidentially alias "-a x" and "-b x". The "prettyname"
	''   could be used to verify this here, but then we'd have to check both
	''   prettyname and string data.

	var source = sourcebufferNew( prettyname, location )
	source->buffer = strDuplicate( s )
	source->size = len( *s ) + 1

	function = source
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub oops( byval message as zstring ptr )
	print "oops, " + *message
	end 1
end sub

function hDumpLocation( byval location as TKLOCATION ptr ) as string
	if( location->source ) then
		var s = "location("
		s += hDumpSourceBuffer( location->source ) & ", "
		s += "line " & location->linenum + 1 & ", "
		s += "column " & location->column + 1 & ", "
		s += "length " & location->length
		s += ")"
		function = s
	else
		function = "(no location info)"
	end if
end function

sub hCalcErrorLine _
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

function hErrorMarker( byval indent as integer, byval length as integer ) as string
	function = space( indent ) + "^" + string( length - 1, "~" )
end function

private function hTrimmedSourceLine _
	( _
		byval location as TKLOCATION ptr, _
		byval linenum as integer, _
		byval maxwidth as integer, _
		byref adjustedcolumn as integer _
	) as string

	var ln = lexPeekLine( location->source, linenum )
	hCalcErrorLine( location->column, maxwidth, ln, adjustedcolumn )

	function = ln
end function

''
'' Builds an error message string like this:
''
''    filename.bas(10): duplicate definition of 'foo'
''        dim foo as integer
''            ^~~
''
function hReport( byval location as TKLOCATION ptr, byval message as zstring ptr ) as string
	if( location->source = NULL ) then
		return *message
	end if

	'' filename(1-based linenumber): message
	var s = *location->source->name + "(" & (location->linenum + 1) & "): " + *message

	'' A line of source code, trimmed to fit into the width limit
	const INDENT = 4
	const MAXWIDTH = 80 - INDENT
	dim markeroffset as integer
	s += !"\n" + space( INDENT )
	s += hTrimmedSourceLine( location, location->linenum, MAXWIDTH, markeroffset )

	'' Error marker below the erroneous line
	s += !"\n" + hErrorMarker( INDENT + markeroffset, location->length )

	if( (not location->source->is_file) and (location->source->location.source <> NULL) ) then
		s += !"\n" + hReport( @location->source->location, "from here:" )
	end if

	function = s
end function

sub oopsLocation( byval location as TKLOCATION ptr, byval message as zstring ptr )
	print hReport( location, message )
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

function hashHash( byval s as zstring ptr ) as ulong
	dim as long hash = 5381
	while( (*s)[0] )
		hash = (*s)[0] + (hash shl 5) - hash
		s += 1
	wend
	function = hash
end function

private function hashHash2( byval s as zstring ptr ) as ulong
	dim as ulong hash = 0
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
		byval hash as ulong _
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
		byval hash as ulong _
	) as integer
	function = (hashLookup( h, s, hash )->s <> NULL)
end function

sub hashAdd _
	( _
		byval h as THASH ptr, _
		byval item as THASHITEM ptr, _
		byval hash as ulong, _
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

#if __FB_DEBUG__
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
#endif

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

function hExepath( ) as string
	function = pathAddDiv( exepath( ) )
end function

private function pathEndsWithDiv( byref s as string ) as integer
	var length = len( s )
	if( length > 0 ) then
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
		select case( s[length-1] )
		case asc( "\" ), asc( "/" )
			function = TRUE
		end select
#else
		function = (s[length-1] = asc( "/" ))
#endif
	end if
end function

function pathIsDir( byref s as string ) as integer
	function = hReadableDirExists( s ) or pathEndsWithDiv( s )
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
	@"_", _  '' FB's line continuation char
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
	hashInit( @fbkeywordhash, 10, TRUE )
	for i as integer = lbound( fbkeywords ) to ubound( fbkeywords )
		hashAddOverwrite( @fbkeywordhash, fbkeywords(i), NULL )
	next
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function hReadableDirExists( byref path as string ) as integer
	var fixed = path
	if( right( fixed, len( PATHDIV ) ) = PATHDIV ) then
		fixed = left( fixed, len( fixed ) - len( PATHDIV ) )
	end if
	function = (len( dir( fixed, fbDirectory or fbReadOnly or fbHidden ) ) > 0)
end function

function hFileExists( byref path as string ) as integer
	function = (dir( path, fbNormal ) <> "")
end function

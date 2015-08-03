#include once "fbfrog.bi"
#include once "crt.bi"
#include once "dir.bi"

function min(byval a as integer, byval b as integer) as integer
	if b < a then a = b
	function = a
end function

function max(byval a as integer, byval b as integer) as integer
	if b > a then a = b
	function = a
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function sourceinfoForZstring(byval prettyname as zstring ptr) byref as SourceInfo
	function = *new SourceInfo(strDuplicate(prettyname), FALSE)
end function

sub FileBuffer.load(byval location as TkLocation)
	'' Read in the whole file content
	var f = freefile()
	if open(*source.name, for binary, access read, as #f) then
		oopsLocation(location, "could not open file: '" + *source.name + "'")
	end if

	dim as ulongint filesize = lof(f)
	if filesize > &h40000000 then
		oopsLocation(location, "a header file bigger than 1 GiB? no way...")
	end if

	'' An extra 0 byte at the end of the buffer so we can look ahead
	'' without bound checks, and don't need to give special treatment
	'' to empty files.
	dim as integer sizetoload = filesize
	buffer = callocate(sizetoload + 1)

	if sizetoload > 0 then
		var sizeloaded = 0
		var result = get(#f, , *cptr(ubyte ptr, buffer), sizetoload, sizeloaded)
		if result or (sizeloaded <> sizetoload) then
			oopsLocation(location, "file I/O failed")
		end if
	end if

	close #f

	'' Currently tokens store text as null-terminated strings, so they
	'' can't allow embedded nulls, and null also indicates EOF to the lexer.
	for i as integer = 0 to sizetoload-1
		if buffer[i] = 0 then
			oopsLocation(location, "file '" + *source.name + "' has embedded nulls, please fix that first!")
		end if
	next
end sub

namespace filebuffers
	dim shared hashtb as THASH
end namespace

sub filebuffersInit()
	hashInit(@filebuffers.hashtb, 8, FALSE)
end sub

function filebuffersAdd(byval filename as zstring ptr, byval location as TkLocation) as FileBuffer ptr
	'' Cache file buffers based on the file name
	var hash = hashHash(filename)
	var item = hashLookup(@filebuffers.hashtb, filename, hash)

	'' Not yet loaded?
	if item->s = NULL then
		var file = new FileBuffer
		file->source.name = strDuplicate(filename)
		file->source.is_file = TRUE
		file->load(location)
		hashAdd(@filebuffers.hashtb, item, hash, file->source.name, file)
	end if

	function = item->data
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub oops(byval message as zstring ptr)
	print "oops, " + *message
	end 1
end sub

function hDumpLocation(byval location as TkLocation) as string
	if location.source then
		function = *location.source->name + "(" & location.linenum & ")"
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
	if len(s) > limit then
		var shift = 0

		if column < ((limit * 3) / 4) then
			'' Offset is still well visible, so align to left.
			s = left(s, limit)
		else
			'' Must scroll the line to the left (and the offset too),
			'' to make the location visible.
			'' a) center it, if the string is long enough for this,
			'' b) or align to the right.

			'' Enough chars behind the offset to fill up a half?
			var half = limit / 2
			if (len(s) - column) >= half then
				'' Center: shift left to align offset to visible boundary,
				shift = column - limit
				'' and shift further to reach the middle.
				shift += half
				s = mid(s, shift+1, limit)
			else
				'' Right align:
				shift = len(s) - limit
				s = right(s, limit)
			end if
		end if

		offset = column - shift
	else
		offset = column
	end if

end sub

function hErrorMarker(byval indent as integer, byval length as integer) as string
	function = space(indent) + "^" + string(length - 1, "~")
end function

'' Builds an error message string like this:
''    filename.bas(10): duplicate definition of 'foo'
'' The filename is shown relative to curdir() if possible, that's usually nicer
'' for the user.
function hReport(byval location as TkLocation, byval message as zstring ptr) as string
	if location.source then
		function = pathStripCurdir(*location.source->name) + "(" & location.linenum & "): " + *message
	else
		function = *message
	end if
end function

sub oopsLocation(byval location as TkLocation, byval message as zstring ptr)
	print hReport(location, message)
	end 1
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function hTrim(byref s as string) as string
	function = trim(s, any !" \t")
end function

function hLTrim(byref s as string) as string
	function = ltrim(s, any !" \t")
end function

function strStartsWith(byref s as string, byref lookfor as string) as integer
	function = left(s, len(lookfor)) = lookfor
end function

function strDuplicate(byval s as zstring ptr) as zstring ptr
	dim as zstring ptr p = any
	if s then
		p = callocate(len(*s) + 1)
		*p = *s
		function = p
	else
		function = NULL
	end if
end function

sub strSplit(byref s as string, byref delimiter as string, byref l as string, byref r as string)
	var leftlen = instr(s, delimiter) - 1
	if leftlen > 0 then
		l = left(s, leftlen)
		r = right(s, len(s) - leftlen - len(delimiter))
	else
		l = s
		r = ""
	end if
end sub

function strReplace _
	( _
		byref text as string, _
		byref a as string, _
		byref b as string _
	) as string

	var result = text

	var alen = len(a)
	var blen = len(b)

	var i = 0
	do
		'' Does result contain an occurence of a?
		i = instr(i + 1, result, a)
		if i = 0 then
			exit do
		end if

		'' Cut out a and insert b in its place
		'' result  =  front  +  b  +  back
		var keep = right(result, len(result) - ((i - 1) + alen))
		result = left(result, i - 1)
		result += b
		result += keep

		i += blen - 1
	loop

	function = result
end function

function strReplaceNonIdChars(byref orig as string, byval replacement as integer) as string
	var s = orig

	for i as integer = 0 to len(s) - 1
		dim as integer ch = s[i]

		select case as const ch
		case CH_A to CH_Z, CH_L_A to CH_L_Z, CH_UNDERSCORE, CH_0 to CH_9

		case else
			ch = replacement
		end select

		s[i] = ch
	next

	function = s
end function

function strMakePrintable(byref a as string) as string
	dim b as string

	for i as integer = 0 to len(a)-1
		select case a[i]
		case CH_LF  : b += "\n"
		case CH_CR  : b += "\r"
		case CH_TAB : b += "\t"
		case is < 32, 127 : b += "?"
		case else   : b += chr(a[i])
		end select
	next

	function = b
end function

function strIsValidSymbolId(byval s as zstring ptr) as integer
	var i = 0
	do
		select case as const (*s)[0]
		case 0
			exit do

		case CH_A to CH_Z, CH_L_A to CH_L_Z, CH_UNDERSCORE
			'' A-Z, a-z, _ are allowed

		case CH_0 to CH_9
			'' Numbers are allowed but not at the front
			if i = 0 then
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

private function strIsNumber(byref s as string) as integer
	for i as integer = 0 to len(s) - 1
		if (s[i] < CH_0) or (s[i] > CH_9) then
			exit function
		end if
	next
	function = TRUE
end function

'' Does an identifier start with __ (double underscore) or _U (single underscore
'' and upper-case letter)?
function strIsReservedIdInC(byval id as zstring ptr) as integer
	if (*id)[0] = CH_UNDERSCORE then
		var ch2 = (*id)[1]
		if (ch2 = CH_UNDERSCORE) or ((ch2 >= CH_A) and (ch2 <= CH_Z)) then
			return TRUE
		end if
	end if
end function

'' Recursive string matching, with ? and * wildcards, seems to work ok
'' (based on post from stackoverflow)
function strMatch(byref s as string, byref pattern as string) as integer
	'' Always match?
	if pattern = "*" then return TRUE

	'' Same? (safe even if the string contains wildcard chars itself)
	if s = pattern then return TRUE

	'' String <> pattern. String empty?
	if len(s) = 0 then return FALSE

	select case left(pattern, 1)
	case "*"
		'' Either the rest of the pattern must match right here,
		'' or the pattern must match somewhere later in the string.
		return strMatch(s, right(pattern, len(pattern) - 1)) orelse _
		       strMatch(right(s, len(s) - 1), pattern)

	case "?", left(s, 1)
		'' Current char matches; now check the rest.
		return strMatch(right(s, len(s) - 1), right(pattern, len(pattern) - 1))
	end select

	function = FALSE
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
''
'' If the hash table owns the strings then hashAdd() will duplicate them and
'' then never change them again until hashEnd() which frees them.
''

function hashHash(byval s as zstring ptr) as ulong
	dim as long hash = 5381
	while (*s)[0]
		hash = (*s)[0] + (hash shl 5) - hash
		s += 1
	wend
	function = hash
end function

private function hashHash2(byval s as zstring ptr) as ulong
	dim as ulong hash = 0
	while (*s)[0]
		hash = (*s)[0] + (hash shl 6) + (hash shl 16) - hash
		s += 1
	wend
	function = hash
end function

private sub hAllocTable(byval h as THASH ptr)
	'' They must be zeroed, because NULL instead of a string indicates
	'' unused items
	h->items = callocate(h->room * sizeof(THASHITEM))
end sub

private sub hGrowTable(byval h as THASH ptr)
	var old = h->items
	var oldroom = h->room

	h->room shl= 1
	hAllocTable(h)

	'' Insert all used items from the old table into the new one.
	'' This will redistribute everything using the new h->room.
	for item as THASHITEM ptr = old to (old + (oldroom - 1))
		if item->s then
			'' Yep, this is recursive, but since the table is
			'' larger by now, we won't recurse in here again.
			*hashLookup(h, item->s, item->hash) = *item
		end if
	next

	deallocate(old)
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
	if (h->count * 4) >= (h->room * 3) then
		hGrowTable(h)
	end if

	dim as uinteger roommask = h->room - 1

	'' First probe
	var i = hash and roommask
	var item = h->items + i

	'' Found unused item with first probe?
	if item->s = NULL then
		return item
	end if

	'' Item is used. Is it the correct string?
	if item->hash = hash then
		if *item->s = *s then
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
	var stepsize = (hashHash2(s) and roommask) or 1

	do
		i = (i + stepsize) and roommask
		item = h->items + i

		'' Found unused item?
		'' The string is not in the hash, or it would have been found before.
		if item->s = NULL then
			exit do
		end if

		'' Item is used. Is it the correct string?
		if item->hash = hash then
			if *item->s = *s then
				exit do
			end if
		end if
	loop

	function = item
end function

function hashLookupDataOrNull(byval h as THASH ptr, byval id as zstring ptr) as any ptr
	var item = hashLookup(h, id, hashHash(id))
	if item->s then
		function = item->data
	end if
end function

function hashContains _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval hash as ulong _
	) as integer
	function = (hashLookup(h, s, hash)->s <> NULL)
end function

sub hashAdd _
	( _
		byval h as THASH ptr, _
		byval item as THASHITEM ptr, _
		byval hash as ulong, _
		byval s as zstring ptr, _
		byval dat as any ptr _
	)

	if h->duplicate_strings then
		s = strDuplicate(s)
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

	var hash = hashHash(s)
	var item = hashLookup(h, s, hash)

	if item->s then
		'' Already exists
		assert(*item->s = *s)
		assert(item->hash = hash)
	else
		'' New entry
		if h->duplicate_strings then
			item->s = strDuplicate(s)
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
	h->duplicate_strings = duplicate_strings
	hAllocTable(h)

end sub

sub hashEnd(byval h as THASH ptr)
	'' Free each item's string if they were duplicated
	if h->duplicate_strings then
		var i = h->items
		var limit = i + h->room
		while i < limit
			deallocate(i->s)
			i += 1
		wend
	end if

	deallocate(h->items)
end sub

#if __FB_DEBUG__
sub hashDump(byval h as THASH ptr)
	print "hash: " & h->count & "/" & h->room & " slots used"
	for i as integer = 0 to h->room-1
		with h->items[i]
			print "    " & i & ": ";
			if .s then
				print "hash=" + hex(.hash) + ", s=""" + *.s + """, data=" + hex(.data)
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
private function hFindExtBegin(byref path as string) as integer
	for i as integer = len(path)-1 to 0 step -1
		select case path[i]
		case asc(".")
			return i
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
		case asc($"\"), asc("/")
#else
		case asc("/")
#endif
			exit for
		end select
	next
	function = len(path)
end function

function pathStripExt(byref path as string) as string
	function = left(path, hFindExtBegin(path))
end function

function pathExtOnly(byref path as string) as string
	'' -1 to strip the '.' in front of the file extension
	function = right(path, len(path) - hFindExtBegin(path) - 1)
end function

private function hFindFileName(byref path as string) as integer
	for i as integer = len(path)-1 to 0 step -1
		select case path[i]
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
		case asc($"\"), asc("/")
#else
		case asc("/")
#endif
			return i + 1
		end select
	next
end function

function pathOnly(byref path as string) as string
	function = left(path, hFindFileName(path))
end function

function pathStrip(byref path as string) as string
	function = right(path, len(path) - hFindFileName(path))
end function

function pathAddDiv(byref path as string) as string
	dim as string s
	dim as integer length = any

	s = path
	length = len(s)

	if length > 0 then
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
		select case s[length-1]
		case asc($"\"), asc("/")

		case else
			s += $"\"
		end select
#else
		if s[length-1] <> asc("/") then
			s += "/"
		end if
#endif
	end if

	function = s
end function

private function pathGetRootLength(byref s as string) as integer
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
	if len(s) >= 3 then
		'' x:\...
		if s[1] = asc(":") then
			select case s[2]
			case asc("/"), asc($"\")
				return 3
			end select
		end if
	end if
#ifdef __FB_WIN32__
	'' UNC paths
	if len(s) >= 2 then
		'' \\...
		if s[0] = asc($"\") then
			if s[1] = asc($"\") then
				return 2
			end if
		end if
	end if
#endif
#else
	if len(s) >= 1 then
		'' /...
		if s[0] = asc("/") then
			return 1
		end if
	end if
#endif
end function

function pathIsAbsolute(byref s as string) as integer
	function = (pathGetRootLength(s) > 0)
end function

'' Turns a relative path into an absolute path
function pathMakeAbsolute(byref path as string) as string
	if pathIsAbsolute(path) then
		function = path
	else
		function = hCurdir() + path
	end if
end function

function hExepath() as string
	function = pathAddDiv(exepath())
end function

function hCurdir() as string
	function = pathAddDiv(curdir())
end function

function pathStripCurdir(byref path as string) as string
	var pwd = hCurdir()
	if left(path, len(pwd)) = pwd then
		function = right(path, len(path) - len(pwd))
	else
		function = path
	end if
end function

private function pathEndsWithDiv(byref s as string) as integer
	var length = len(s)
	if length > 0 then
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
		select case s[length-1]
		case asc($"\"), asc("/")
			function = TRUE
		end select
#else
		function = (s[length-1] = asc("/"))
#endif
	end if
end function

function pathIsDir(byref s as string) as integer
	function = hReadableDirExists(s) or pathEndsWithDiv(s)
end function

'' Component stack for the path solver
const MAXSOLVERSTACK = 128
namespace solver
	dim shared stack(0 to MAXSOLVERSTACK-1) as integer
	dim shared as integer top
end namespace

private sub solverInit()
	solver.top = -1
end sub

private sub solverPush(byval w as integer)
	solver.top += 1
	if solver.top >= MAXSOLVERSTACK then
		oops("path solver stack too small, MAXSOLVERSTACK=" & MAXSOLVERSTACK)
	end if
	solver.stack(solver.top) = w
end sub

private function solverPop() as integer
	if solver.top > 0 then
		solver.top -= 1
	end if
	function = solver.stack(solver.top)
end function

'' Resolves .'s and ..'s in the path,
'' normalizes path separators to the host standard.
function pathNormalize(byref path as string) as string
	var rootlen = pathGetRootLength(path)
	if rootlen = 0 then
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
	solverInit()
	solverPush(rootlen)

	var s = path
	var dots = 0 '' Number of .'s in the current component
	var chars = 0 '' Number of chars in the current component
	var w = rootlen

	for r as integer = rootlen to len(s) - 1
		select case s[r]
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
		case asc($"\"), asc("/")
#else
		case asc("/")
#endif
			'' Component closed: check whether it was /./ or /../
			select case dots
			case 1    '' /./
				'' Ignore: don't write this /, and remove the .
				w -= 1

			case 2    '' /../
				'' Go back to the begin of the component this
				'' '..' refers to
				w = solverPop()

			case else
				if chars = 0 then
					'' // (Ignore: don't write this /)
				else
					'' Write this /. For Win32/DOS this also normalizes / to \.
					s[w] = asc(PATHDIV)
					'' New component starts behind this /
					w += 1
					'' Remember this begin position so
					'' w can be reset to it during '..'.
					solverPush(w)
				end if

			end select

			dots = 0
			chars = 0

		case asc(".")
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

	function = left(s, w)
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' *.h CRT/POSIX headers for which FB has corresponding crt/*.bi versions
dim shared fbcrtheaders(0 to ...) as zstring ptr = _
{ _
	@"assert", @"ctype", @"errno", @"float", @"limits", @"locale", _
	@"math", @"setjmp", @"signal", @"stdarg", @"stddef", @"stdint", _
	@"stdio", @"stdlib", @"string", @"time", _
	@"sys/types", @"sys/socket", @"wchar" _
}

dim shared fbcrtheaderhash as THASH

sub fbcrtheadersInit()
	hashInit(@fbcrtheaderhash, 5, TRUE)
	for i as integer = lbound(fbcrtheaders) to ubound(fbcrtheaders)
		hashAddOverwrite(@fbcrtheaderhash, fbcrtheaders(i), NULL)
	next
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type DATATYPEINFO
	id as zstring ptr
	dtype as integer
end type

dim shared extradatatypes(0 to ...) as DATATYPEINFO => _
{ _
	(@"__int8"   , TYPE_BYTE    ), _
	(@"__int16"  , TYPE_SHORT   ), _
	(@"__int32"  , TYPE_LONG    ), _
	(@"__int64"  , TYPE_LONGINT ), _
	(@"int8_t"   , TYPE_BYTE    ), _
	(@"int16_t"  , TYPE_SHORT   ), _
	(@"int32_t"  , TYPE_LONG    ), _
	(@"int64_t"  , TYPE_LONGINT ), _
	(@"uint8_t"  , TYPE_UBYTE   ), _
	(@"uint16_t" , TYPE_USHORT  ), _
	(@"uint32_t" , TYPE_ULONG   ), _
	(@"uint64_t" , TYPE_ULONGINT), _
	(@"intptr_t" , TYPE_INTEGER ), _
	(@"uintptr_t", TYPE_UINTEGER), _
	(@"ptrdiff_t", TYPE_INTEGER ), _
	(@"size_t"   , TYPE_UINTEGER), _
	(@"ssize_t"  , TYPE_INTEGER ), _
	(@"wchar_t"  , TYPE_WSTRING )  _
}

dim shared extradatatypehash as THASH

sub extradatatypesInit()
	hashInit(@extradatatypehash, 6, FALSE)
	for i as integer = 0 to ubound(extradatatypes)
		hashAddOverwrite(@extradatatypehash, extradatatypes(i).id, cast(any ptr, extradatatypes(i).dtype))
	next
end sub

function extradatatypesLookup(byval id as zstring ptr) as integer
	var item = hashLookup(@extradatatypehash, id, hashHash(id))
	if item->s then
		function = cint(item->data)
	else
		function = TYPE_NONE
	end if
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type FbKeywordInfo
	id as zstring ptr
	fbkw as integer
end type

dim shared fbkeywordsinfo(0 to ...) as FbKeywordInfo => { _
	(@"ANDALSO"    , FBKW_OP), _
	(@"AND"        , FBKW_OP), _
	(@"DELETE"     , FBKW_OP), _
	(@"EQV"        , FBKW_OP), _
	(@"IMP"        , FBKW_OP), _
	(@"MOD"        , FBKW_OP), _
	(@"NEW"        , FBKW_OP), _
	(@"NOT"        , FBKW_OP), _
	(@"ORELSE"     , FBKW_OP), _
	(@"OR"         , FBKW_OP), _
	(@"SHL"        , FBKW_OP), _
	(@"SHR"        , FBKW_OP), _
	(@"XOR"        , FBKW_OP), _
	_
	(@"ABS"        , FBKW_CORE), _
	(@"ABSTRACT"   , FBKW_CORE), _
	(@"ALIAS"      , FBKW_CORE), _
	(@"ANY"        , FBKW_CORE), _
	(@"AS"         , FBKW_CORE), _
	(@"ASM"        , FBKW_CORE), _
	(@"BASE"       , FBKW_CORE), _
	(@"BYREF"      , FBKW_CORE), _
	(@"BYTE"       , FBKW_CORE), _
	(@"BYVAL"      , FBKW_CORE), _
	(@"CALL"       , FBKW_CORE), _
	(@"CASE"       , FBKW_CORE), _
	(@"CAST"       , FBKW_CORE), _
	(@"CBYTE"      , FBKW_CORE), _
	(@"CDBL"       , FBKW_CORE), _
	(@"CDECL"      , FBKW_CORE), _
	(@"CINT"       , FBKW_CORE), _
	(@"CLASS"      , FBKW_CORE), _
	(@"CLNG"       , FBKW_CORE), _
	(@"CLNGINT"    , FBKW_CORE), _
	(@"COMMON"     , FBKW_CORE), _
	(@"CONST"      , FBKW_CORE), _
	(@"CONSTRUCTOR", FBKW_CORE), _
	(@"CONTINUE"   , FBKW_CORE), _
	(@"CPTR"       , FBKW_CORE), _
	(@"CSHORT"     , FBKW_CORE), _
	(@"CSIGN"      , FBKW_CORE), _
	(@"CSNG"       , FBKW_CORE), _
	(@"CUBYTE"     , FBKW_CORE), _
	(@"CUINT"      , FBKW_CORE), _
	(@"CULNG"      , FBKW_CORE), _
	(@"CULNGINT"   , FBKW_CORE), _
	(@"CUNSG"      , FBKW_CORE), _
	(@"CUSHORT"    , FBKW_CORE), _
	(@"DECLARE"    , FBKW_CORE), _
	(@"DESTRUCTOR" , FBKW_CORE), _
	(@"DIM"        , FBKW_CORE), _
	(@"DO"         , FBKW_CORE), _
	(@"DOUBLE"     , FBKW_CORE), _
	(@"ELSE"       , FBKW_CORE), _
	(@"ELSEIF"     , FBKW_CORE), _
	(@"END"        , FBKW_CORE), _
	(@"ENDIF"      , FBKW_CORE), _
	(@"ENUM"       , FBKW_CORE), _
	(@"EXIT"       , FBKW_CORE), _
	(@"EXPORT"     , FBKW_CORE), _
	(@"EXTENDS"    , FBKW_CORE), _
	(@"EXTERN"     , FBKW_CORE), _
	(@"FIX"        , FBKW_CORE), _
	(@"FOR"        , FBKW_CORE), _
	(@"FRAC"       , FBKW_CORE), _
	(@"FUNCTION"   , FBKW_CORE), _
	(@"GOTO"       , FBKW_CORE), _
	(@"IF"         , FBKW_CORE), _
	(@"IIF"        , FBKW_CORE), _
	(@"IMPLEMENTS" , FBKW_CORE), _
	(@"IMPORT"     , FBKW_CORE), _
	(@"INTEGER"    , FBKW_CORE), _
	(@"INT"        , FBKW_CORE), _
	(@"IS"         , FBKW_CORE), _
	(@"LET"        , FBKW_CORE), _
	(@"LIB"        , FBKW_CORE), _
	(@"LONG"       , FBKW_CORE), _
	(@"LONGINT"    , FBKW_CORE), _
	(@"LOOP"       , FBKW_CORE), _
	(@"NAMESPACE"  , FBKW_CORE), _
	(@"NEXT"       , FBKW_CORE), _
	(@"OPERATOR"   , FBKW_CORE), _
	(@"OVERLOAD"   , FBKW_CORE), _
	(@"PASCAL"     , FBKW_CORE), _
	(@"PEEK"       , FBKW_CORE), _
	(@"POINTER"    , FBKW_CORE), _
	(@"POKE"       , FBKW_CORE), _
	(@"PRIVATE"    , FBKW_CORE), _
	(@"PROCPTR"    , FBKW_CORE), _
	(@"PROPERTY"   , FBKW_CORE), _
	(@"PROTECTED"  , FBKW_CORE), _
	(@"PTR"        , FBKW_CORE), _
	(@"PUBLIC"     , FBKW_CORE), _
	(@"REM"        , FBKW_CORE), _
	(@"RETURN"     , FBKW_CORE), _
	(@"SCOPE"      , FBKW_CORE), _
	(@"SELECT"     , FBKW_CORE), _
	(@"SGN"        , FBKW_CORE), _
	(@"SHARED"     , FBKW_CORE), _
	(@"SHORT"      , FBKW_CORE), _
	(@"SINGLE"     , FBKW_CORE), _
	(@"STATIC"     , FBKW_CORE), _
	(@"STDCALL"    , FBKW_CORE), _
	(@"STEP"       , FBKW_CORE), _
	(@"STRING"     , FBKW_CORE), _
	(@"SUB"        , FBKW_CORE), _
	(@"SWAP"       , FBKW_CORE), _
	(@"THEN"       , FBKW_CORE), _
	(@"TO"         , FBKW_CORE), _
	(@"TYPE"       , FBKW_CORE), _
	(@"TYPEOF"     , FBKW_CORE), _
	(@"UBYTE"      , FBKW_CORE), _
	(@"UINTEGER"   , FBKW_CORE), _
	(@"ULONG"      , FBKW_CORE), _
	(@"ULONGINT"   , FBKW_CORE), _
	(@"UNION"      , FBKW_CORE), _
	(@"UNSIGNED"   , FBKW_CORE), _
	(@"UNTIL"      , FBKW_CORE), _
	(@"USHORT"     , FBKW_CORE), _
	(@"USING"      , FBKW_CORE), _
	(@"VA_FIRST"   , FBKW_CORE), _
	(@"VAR"        , FBKW_CORE), _
	(@"VIRTUAL"    , FBKW_CORE), _
	(@"WEND"       , FBKW_CORE), _
	(@"WHILE"      , FBKW_CORE), _
	(@"WITH"       , FBKW_CORE), _
	(@"WSTRING"    , FBKW_CORE), _
	(@"ZSTRING"    , FBKW_CORE), _
	_
	(@"ACCESS"     , FBKW_QUIRK), _
	(@"ACOS"       , FBKW_QUIRK), _
	(@"APPEND"     , FBKW_QUIRK), _
	(@"ASC"        , FBKW_QUIRK), _
	(@"ASIN"       , FBKW_QUIRK), _
	(@"ATAN2"      , FBKW_QUIRK), _
	(@"ATN"        , FBKW_QUIRK), _
	(@"BINARY"     , FBKW_QUIRK), _
	(@"CHR"        , FBKW_QUIRK), _
	(@"CIRCLE"     , FBKW_QUIRK), _
	(@"CLOSE"      , FBKW_QUIRK), _
	(@"COLOR"      , FBKW_QUIRK), _
	(@"COS"        , FBKW_QUIRK), _
	(@"CVD"        , FBKW_QUIRK), _
	(@"CVI"        , FBKW_QUIRK), _
	(@"CVL"        , FBKW_QUIRK), _
	(@"CVLONGINT"  , FBKW_QUIRK), _
	(@"CVS"        , FBKW_QUIRK), _
	(@"CVSHORT"    , FBKW_QUIRK), _
	(@"DATA"       , FBKW_QUIRK), _
	(@"DEFBYTE"    , FBKW_QUIRK), _
	(@"DEFDBL"     , FBKW_QUIRK), _
	(@"DEFINED"    , FBKW_QUIRK), _
	(@"DEFINT"     , FBKW_QUIRK), _
	(@"DEFLNG"     , FBKW_QUIRK), _
	(@"DEFLONGINT" , FBKW_QUIRK), _
	(@"DEFSHORT"   , FBKW_QUIRK), _
	(@"DEFSNG"     , FBKW_QUIRK), _
	(@"DEFSTR"     , FBKW_QUIRK), _
	(@"DEFUBYTE"   , FBKW_QUIRK), _
	(@"DEFUINT"    , FBKW_QUIRK), _
	(@"DEFULNG"    , FBKW_QUIRK), _
	(@"DEFULONGINT", FBKW_QUIRK), _
	(@"DEFUSHORT"  , FBKW_QUIRK), _
	(@"DRAW"       , FBKW_QUIRK), _
	(@"DYNAMIC"    , FBKW_QUIRK), _
	(@"ENCODING"   , FBKW_QUIRK), _
	(@"ERASE"      , FBKW_QUIRK), _
	(@"ERR"        , FBKW_QUIRK), _
	(@"ERROR"      , FBKW_QUIRK), _
	(@"EXP"        , FBKW_QUIRK), _
	(@"EXPLICIT"   , FBKW_QUIRK), _
	(@"FIELD"      , FBKW_QUIRK), _
	(@"GET"        , FBKW_QUIRK), _
	(@"GOSUB"      , FBKW_QUIRK), _
	(@"IMAGECREATE", FBKW_QUIRK), _
	(@"INCLUDE"    , FBKW_QUIRK), _
	(@"INPUT"      , FBKW_QUIRK), _
	(@"INSTR"      , FBKW_QUIRK), _
	(@"INSTRREV"   , FBKW_QUIRK), _
	(@"LBOUND"     , FBKW_QUIRK), _
	(@"LCASE"      , FBKW_QUIRK), _
	(@"LEN"        , FBKW_QUIRK), _
	(@"LINE"       , FBKW_QUIRK), _
	(@"LOCAL"      , FBKW_QUIRK), _
	(@"LOCK"       , FBKW_QUIRK), _
	(@"LOG"        , FBKW_QUIRK), _
	(@"LPRINT"     , FBKW_QUIRK), _
	(@"LSET"       , FBKW_QUIRK), _
	(@"LTRIM"      , FBKW_QUIRK), _
	(@"MID"        , FBKW_QUIRK), _
	(@"MKD"        , FBKW_QUIRK), _
	(@"MKI"        , FBKW_QUIRK), _
	(@"MKL"        , FBKW_QUIRK), _
	(@"MKLONGINT"  , FBKW_QUIRK), _
	(@"MKS"        , FBKW_QUIRK), _
	(@"MKSHORT"    , FBKW_QUIRK), _
	(@"NAME"       , FBKW_QUIRK), _
	(@"ON"         , FBKW_QUIRK), _
	(@"OPEN"       , FBKW_QUIRK), _
	(@"OPTION"     , FBKW_QUIRK), _
	(@"OUTPUT"     , FBKW_QUIRK), _
	(@"PAINT"      , FBKW_QUIRK), _
	(@"PALETTE"    , FBKW_QUIRK), _
	(@"POINT"      , FBKW_QUIRK), _
	(@"PRESERVE"   , FBKW_QUIRK), _
	(@"PRESET"     , FBKW_QUIRK), _
	(@"PRINT"      , FBKW_QUIRK), _
	(@"PSET"       , FBKW_QUIRK), _
	(@"PUT"        , FBKW_QUIRK), _
	(@"RANDOM"     , FBKW_QUIRK), _
	(@"READ"       , FBKW_QUIRK), _
	(@"REDIM"      , FBKW_QUIRK), _
	(@"RESTORE"    , FBKW_QUIRK), _
	(@"RESUME"     , FBKW_QUIRK), _
	(@"RSET"       , FBKW_QUIRK), _
	(@"RTRIM"      , FBKW_QUIRK), _
	(@"SADD"       , FBKW_QUIRK), _
	(@"SCREEN"     , FBKW_QUIRK), _
	(@"SEEK"       , FBKW_QUIRK), _
	(@"SIN"        , FBKW_QUIRK), _
	(@"SIZEOF"     , FBKW_QUIRK), _
	(@"SPC"        , FBKW_QUIRK), _
	(@"SQR"        , FBKW_QUIRK), _
	(@"STR"        , FBKW_QUIRK), _
	(@"STRPTR"     , FBKW_QUIRK), _
	(@"TAB"        , FBKW_QUIRK), _
	(@"TAN"        , FBKW_QUIRK), _
	(@"THREADCALL" , FBKW_QUIRK), _
	(@"TRIM"       , FBKW_QUIRK), _
	(@"UBOUND"     , FBKW_QUIRK), _
	(@"UCASE"      , FBKW_QUIRK), _
	(@"UNLOCK"     , FBKW_QUIRK), _
	(@"VARPTR"     , FBKW_QUIRK), _
	(@"VIEW"       , FBKW_QUIRK), _
	(@"WCHR"       , FBKW_QUIRK), _
	(@"WIDTH"      , FBKW_QUIRK), _
	(@"WINDOW"     , FBKW_QUIRK), _
	(@"WINPUT"     , FBKW_QUIRK), _
	(@"WRITE"      , FBKW_QUIRK), _
	(@"WSTR"       , FBKW_QUIRK), _
	_
	(@"ALLOCATE"    , FBKW_RTL), _
	(@"ASSERT"      , FBKW_RTL), _
	(@"ASSERTWARN"  , FBKW_RTL), _
	(@"BEEP"        , FBKW_RTL), _
	(@"BIN"         , FBKW_RTL), _
	(@"BIT"         , FBKW_RTL), _
	(@"BITRESET"    , FBKW_RTL), _
	(@"BITSET"      , FBKW_RTL), _
	(@"BLOAD"       , FBKW_RTL), _
	(@"BSAVE"       , FBKW_RTL), _
	(@"CALLOCATE"   , FBKW_RTL), _
	(@"CHAIN"       , FBKW_RTL), _
	(@"CHDIR"       , FBKW_RTL), _
	(@"CLEAR"       , FBKW_RTL), _
	(@"CLS"         , FBKW_RTL), _
	(@"COMMAND"     , FBKW_RTL), _
	(@"CONDBROADCAST", FBKW_RTL), _
	(@"CONDCREATE"  , FBKW_RTL), _
	(@"CONDDESTROY" , FBKW_RTL), _
	(@"CONDSIGNAL"  , FBKW_RTL), _
	(@"CONDWAIT"    , FBKW_RTL), _
	(@"CSRLIN"      , FBKW_RTL), _
	(@"CURDIR"      , FBKW_RTL), _
	(@"DATE"        , FBKW_RTL), _
	(@"DEALLOCATE"  , FBKW_RTL), _
	(@"DIR"         , FBKW_RTL), _
	(@"DYLIBFREE"   , FBKW_RTL), _
	(@"DYLIBLOAD"   , FBKW_RTL), _
	(@"DYLIBSYMBOL" , FBKW_RTL), _
	(@"ENVIRON"     , FBKW_RTL), _
	(@"EOF"         , FBKW_RTL), _
	(@"ERFN"        , FBKW_RTL), _
	(@"ERL"         , FBKW_RTL), _
	(@"ERMN"        , FBKW_RTL), _
	(@"EXEC"        , FBKW_RTL), _
	(@"EXEPATH"     , FBKW_RTL), _
	(@"FLIP"        , FBKW_RTL), _
	(@"FRE"         , FBKW_RTL), _
	(@"FREEFILE"    , FBKW_RTL), _
	(@"GETJOYSTICK" , FBKW_RTL), _
	(@"GETKEY"      , FBKW_RTL), _
	(@"GETMOUSE"    , FBKW_RTL), _
	(@"HEX"         , FBKW_RTL), _
	(@"HIBYTE"      , FBKW_RTL), _
	(@"HIWORD"      , FBKW_RTL), _
	(@"IMAGECONVERTROW", FBKW_RTL), _
	(@"IMAGEDESTROY", FBKW_RTL), _
	(@"IMAGEINFO"   , FBKW_RTL), _
	(@"INKEY"       , FBKW_RTL), _
	(@"INP"         , FBKW_RTL), _
	(@"KILL"        , FBKW_RTL), _
	(@"LEFT"        , FBKW_RTL), _
	(@"LOBYTE"      , FBKW_RTL), _
	(@"LOC"         , FBKW_RTL), _
	(@"LOCATE"      , FBKW_RTL), _
	(@"LOF"         , FBKW_RTL), _
	(@"LOWORD"      , FBKW_RTL), _
	(@"LPOS"        , FBKW_RTL), _
	(@"MKDIR"       , FBKW_RTL), _
	(@"MULTIKEY"    , FBKW_RTL), _
	(@"MUTEXCREATE" , FBKW_RTL), _
	(@"MUTEXDESTROY", FBKW_RTL), _
	(@"MUTEXLOCK"   , FBKW_RTL), _
	(@"MUTEXUNLOCK" , FBKW_RTL), _
	(@"OBJECT"      , FBKW_RTL), _
	(@"OCT"         , FBKW_RTL), _
	(@"OFFSETOF"    , FBKW_RTL), _
	(@"OUT"         , FBKW_RTL), _
	(@"PCOPY"       , FBKW_RTL), _
	(@"PMAP"        , FBKW_RTL), _
	(@"POINTCOORD"  , FBKW_RTL), _
	(@"POS"         , FBKW_RTL), _
	(@"RANDOMIZE"   , FBKW_RTL), _
	(@"REALLOCATE"  , FBKW_RTL), _
	(@"RESET"       , FBKW_RTL), _
	(@"RGB"         , FBKW_RTL), _
	(@"RGBA"        , FBKW_RTL), _
	(@"RIGHT"       , FBKW_RTL), _
	(@"RMDIR"       , FBKW_RTL), _
	(@"RND"         , FBKW_RTL), _
	(@"RUN"         , FBKW_RTL), _
	(@"SCREENCONTROL", FBKW_RTL), _
	(@"SCREENCOPY"  , FBKW_RTL), _
	(@"SCREENEVENT" , FBKW_RTL), _
	(@"SCREENGLPROC", FBKW_RTL), _
	(@"SCREENINFO"  , FBKW_RTL), _
	(@"SCREENLIST"  , FBKW_RTL), _
	(@"SCREENLOCK"  , FBKW_RTL), _
	(@"SCREENPTR"   , FBKW_RTL), _
	(@"SCREENRES"   , FBKW_RTL), _
	(@"SCREENSET"   , FBKW_RTL), _
	(@"SCREENSYNC"  , FBKW_RTL), _
	(@"SCREENUNLOCK", FBKW_RTL), _
	(@"SETDATE"     , FBKW_RTL), _
	(@"SETENVIRON"  , FBKW_RTL), _
	(@"SETMOUSE"    , FBKW_RTL), _
	(@"SETTIME"     , FBKW_RTL), _
	(@"SHELL"       , FBKW_RTL), _
	(@"SLEEP"       , FBKW_RTL), _
	(@"SPACE"       , FBKW_RTL), _
	(@"STOP"        , FBKW_RTL), _
	(@"SYSTEM"      , FBKW_RTL), _
	(@"THREADCREATE", FBKW_RTL), _
	(@"THREADWAIT"  , FBKW_RTL), _
	(@"TIME"        , FBKW_RTL), _
	(@"TIMER"       , FBKW_RTL), _
	(@"VA_ARG"      , FBKW_RTL), _
	(@"VAL"         , FBKW_RTL), _
	(@"VALINT"      , FBKW_RTL), _
	(@"VALLNG"      , FBKW_RTL), _
	(@"VALUINT"     , FBKW_RTL), _
	(@"VALULNG"     , FBKW_RTL), _
	(@"VA_NEXT"     , FBKW_RTL), _
	(@"WAIT"        , FBKW_RTL), _
	(@"WBIN"        , FBKW_RTL), _
	(@"WHEX"        , FBKW_RTL), _
	(@"WINDOWTITLE" , FBKW_RTL), _
	(@"WOCT"        , FBKW_RTL), _
	(@"WSPACE"      , FBKW_RTL), _
	_
	(@"DEFINE"  , FBKW_PP), _
	(@"ENDMACRO", FBKW_PP), _
	(@"IFDEF"   , FBKW_PP), _
	(@"IFNDEF"  , FBKW_PP), _
	(@"INCLIB"  , FBKW_PP), _
	(@"LANG"    , FBKW_PP), _
	(@"LIBPATH" , FBKW_PP), _
	(@"MACRO"   , FBKW_PP), _
	(@"PRAGMA"  , FBKW_PP), _
	(@"UNDEF"   , FBKW_PP)  _
}

dim shared fbkeywords as THASH

sub fbkeywordsInit()
	hashInit(@fbkeywords, 8, FALSE)
	for i as integer = 0 to ubound(fbkeywordsinfo)
		with fbkeywordsinfo(i)
			assert(hashContains(@fbkeywords, .id, hashHash(.id)) = FALSE)
			hashAddOverwrite(@fbkeywords, .id, cast(any ptr, .fbkw))
		end with
	next
end sub

function fbkeywordsLookup(byval id as zstring ptr) as integer
	var ucaseid = ucase(*id, 1)
	var item = hashLookup(@fbkeywords, ucaseid, hashHash(ucaseid))
	if item->s then
		function = cint(item->data)
	else
		function = -1
	end if
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function hReadableDirExists(byref path as string) as integer
	var fixed = path
	if right(fixed, len(PATHDIV)) = PATHDIV then
		fixed = left(fixed, len(fixed) - len(PATHDIV))
	end if
	function = (len(dir(fixed, fbDirectory or fbReadOnly or fbHidden)) > 0)
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function DeclPattern.matches _
	( _
		byval parentparent as ASTNODE ptr, _
		byval parent as ASTNODE ptr, _
		byval child as ASTNODE ptr, _
		byval childindex as integer _
	) as integer

	if len(parentid) > 0 then
		if parent = NULL then exit function

		'' If it's an anonymous procptr subtype, check its parent's id instead
		dim parentid as zstring ptr
		if parentparent andalso _
		   (parent->class = ASTCLASS_PROC) andalso _
		   (parentparent->subtype = parent) then
			parentid = parentparent->text
		else
			parentid = parent->text
		end if
		if strMatch(*parentid, this.parentid) = FALSE then exit function
	end if

	if len(childid) > 0 then
		'' Match child by name
		function = strMatch(*child->text, childid)
	else
		'' Match child by index
		function = (this.childindex = childindex)
	end if
end function

sub DeclPatterns.add(byref pattern as DeclPattern)
	var i = count
	count += 1
	patterns = reallocate(patterns, sizeof(*patterns) * count)
	patterns[i].constructor()
	patterns[i] = pattern
end sub

'' Declaration pattern format:
''    [<parent-id-pattern>.]<child-id-pattern>
''    <parent-id-pattern>.<child-index>
'' TODO: match based on astclass to speed things up a bit
''       (if we have a parentpattern, the child can only be a field/param/enumconst)
'' TODO: if pattern is a simple id (no wildcards or parent pattern), add to
''       hashtb instead of adding as DeclPattern?
sub DeclPatterns.parseAndAdd(byref s as string)
	dim pattern as DeclPattern

	strSplit(s, ".", pattern.parentid, pattern.childid)

	if len(pattern.childid) > 0 then
		'' Can be given a child index instead of a child id
		if strIsNumber(pattern.childid) then
			pattern.childindex = valuint(pattern.childid)
			pattern.childid = ""
		end if
	else
		'' Just one pattern; use it as child
		swap pattern.parentid, pattern.childid
	end if


	add(pattern)
end sub

destructor DeclPatterns()
	for i as integer = 0 to count - 1
		patterns[i].destructor()
	next
	deallocate(patterns)
end destructor

function DeclPatterns.matches _
	( _
		byval parentparent as ASTNODE ptr, _
		byval parent as ASTNODE ptr, _
		byval child as ASTNODE ptr, _
		byval childindex as integer _
	) as integer
	for i as integer = 0 to count - 1
		if patterns[i].matches(parentparent, parent, child, childindex) then
			return TRUE
		end if
	next
end function

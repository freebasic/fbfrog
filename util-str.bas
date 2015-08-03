#include once "fbfrog.bi"

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

function strIsNumber(byref s as string) as integer
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

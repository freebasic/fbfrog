#include once "util-str.bi"

#include once "chars.bi"
#include once "crt.bi"
#include once "util.bi"

function hTrim(byref s as string) as string
	function = trim(s, any !" \t")
end function

function hLTrim(byref s as string) as string
	function = ltrim(s, any !" \t")
end function

function strStartsWith(byref s as string, byref lookfor as string) as integer
	function = left(s, len(lookfor)) = lookfor
end function

function strDuplicate(byval s as const zstring ptr) as zstring ptr
	if s then
		dim as zstring ptr p = callocate(len(*s) + 1)
		if p = NULL then
			oops("strDuplicate() memory allocation failed")
		end if
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

function strIsValidSymbolId(byval s as const zstring ptr) as integer
	var i = 0
	do
		select case as const (*s)[i]
		case 0
			'' Must not be empty
			if i = 0 then
				return FALSE
			end if
			exit do

		case CH_A to CH_Z, CH_L_A to CH_L_Z, CH_UNDERSCORE
			'' A-Z, a-z, _ are allowed

		case CH_0 to CH_9
			'' Numbers are allowed but not at the front
			if i = 0 then
				return FALSE
			end if

		case else
			return FALSE
		end select

		i += 1
	loop

	function = TRUE
end function

function strIsNumber(byref s as string) as integer
	if len(s) = 0 then
		return FALSE
	end if
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
function strMatch(byref s as const string, byref pattern as const string) as integer
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

''
'' StringMatcher: Prefix tree for pattern matching
'' Goal: to make matching faster than going through the patterns and doing strMatch() for each
''
'' foo
'' foobar
'' fuz
'' bar
''
'' str f
''   str oo
''     eol
''     str bar
''       eol
''   str uz
''     eol
'' str bar
''   eol
''
'' ab
'' a*b
''
'' str a
''   str b
''     eol
''   wildcard
''     str b
''       eol
''

destructor StringMatcher()
	deallocate(text)
	for i as integer = 0 to childcount - 1
		children[i].destructor()
	next
	deallocate(children)
end destructor

sub StringMatcher.addChild(byval nodekind as integer, byval text as const ubyte ptr, byval textlength as integer)
	var i = childcount
	childcount += 1
	children = reallocate(children, sizeof(*children) * childcount)
	clear(children[i], 0, sizeof(children[i]))
	with children[i]
		.nodekind = nodekind
		if text then
			.text = allocate(textlength + 1)
			if .text = NULL then
				oops("StringMatcher memory allocation failed")
			end if
			memcpy(.text, text, textlength)
			.text[textlength] = 0
			.textlength = textlength
		end if
	end with
end sub

sub StringMatcher.addChildHoldingPreviousChildren(byval nodekind as integer, byval text as const ubyte ptr, byval textlength as integer)
	var prevchildren = children
	var prevchildcount = childcount
	children = NULL
	childcount = 0
	addChild(nodekind, text, textlength)
	with children[0]
		.children = prevchildren
		.childcount = prevchildcount
	end with
end sub

sub StringMatcher.truncateTextToOnly(byval newlength as integer)
	if (text <> NULL) and (textlength > newlength) then
		(*text)[newlength] = 0
		textlength = newlength
	end if
end sub

private function findCommonPrefixLen(byval a as const zstring ptr, byval b as const zstring ptr) as integer
	var length = 0
	while ((*a)[0] = (*b)[0]) and ((*a)[0] <> 0)
		length += 1
		a += 1
		b += 1
	wend
	function = length
end function

private function findWildcardOrEol(byval s as const zstring ptr) as integer
	var i = 0
	while ((*s)[0] <> CH_STAR) and ((*s)[0] <> 0)
		i += 1
		s += 1
	wend
	function = i
end function

sub StringMatcher.addPattern(byval pattern as const zstring ptr, byval payload as any ptr)
	nonEmpty = TRUE

	''
	'' Check current choices at this level
	'' We need to check for common prefix between an existing choice and the
	'' new pattern, and then re-use it and possibly extract it.
	''
	'' insert foo2 into
	''   foo1
	''     a
	''     b
	'' =>
	''   foo
	''     1
	''       a
	''       b
	''     2
	''
	'' insert foo2 into
	''   foo1
	''   bar
	'' =>
	''   foo
	''     1
	''     2
	''   bar
	''
	for i as integer = 0 to childcount - 1
		var child = @children[i]

		select case child->nodekind
		case MatchWildcard
			if (*pattern)[0] = CH_STAR then
				'' WILDCARD node is already here, recursively add the rest
				child->addPattern(@pattern[1], payload)
				exit sub
			end if

		case MatchEol
			if (*pattern)[0] = 0 then
				'' Empty pattern, and we already have an EOL node here,
				'' so nothing more needs to be added. But overwrite the payload.
				child->payload = payload
				exit sub
			end if

		case MatchString
			var commonprefixlen = findCommonPrefixLen(child->text, pattern)
			if commonprefixlen > 0 then
				'' If there is a remainder, the node has to be split up
				if commonprefixlen < child->textlength then
					'' Move the remainder suffix into a child node
					var remainder = @child->text[commonprefixlen]
					child->addChildHoldingPreviousChildren(MatchString, remainder, strlen(remainder))

					'' Truncate to prefix only
					child->truncateTextToOnly(commonprefixlen)
				end if

				'' Recursively add the rest of the pattern
				'' (at the very least there will be the null terminator remaining,
				'' for which we have to add an EOL node)
				child->addPattern(@pattern[commonprefixlen], payload)
				exit sub
			end if
		end select
	next

	'' Otherwise, add a new choice at this level
	var eol = findWildcardOrEol(pattern)
	if eol > 0 then
		'' Add node for string prefix, recursively add the rest
		addChild(MatchString, pattern, eol)
		children[childcount-1].addPattern(@pattern[eol], payload)
	else
		select case (*pattern)[0]
		case CH_STAR
			'' Add node for wildcard, recursively add the rest
			addChild(MatchWildcard, NULL, 0)
			children[childcount-1].addPattern(@pattern[1], payload)
		case 0
			'' EOL node
			addChild(MatchEol, NULL, 0)
			children[childcount-1].payload = payload
		end select
	end if
end sub

function StringMatcher.matches(byval s as const zstring ptr, byref payload as any ptr) as integer
	select case nodekind
	case MatchRoot
		for i as integer = 0 to childcount - 1
			if children[i].matches(s, payload) then return TRUE
		next

	case MatchString
		'' The given string must have this exact prefix
		if strncmp(text, s, textlength) = 0 then
			'' Prefix matched; now check suffixes, there must be at least an EOL node
			for suffix as integer = 0 to childcount - 1
				if children[suffix].matches(@s[textlength], payload) then return TRUE
			next
		end if

	case MatchWildcard
		'' Also check suffixes, there must be at least an EOL node
		'' Check whether any of the suffixes match
		for suffix as integer = 0 to childcount - 1
			'' Have to try matches at all possible positions (i.e. expanding the wildcard),
			'' even if it's just a null terminator (so it can match an EOL)
			var i = s
			do
				if children[suffix].matches(i, payload) then return TRUE
				if (*i)[0] = 0 then exit do
				i += 1
			loop
		next

	case MatchEol
		'' The given string must be empty
		if (*s)[0] = 0 then
			payload = this.payload
			return TRUE
		end if
	end select

	'' Nothing matched
	function = FALSE
end function

function StringMatcher.dump1() as string
	dim s as string
	select case nodekind
	case MatchRoot     : s += "root nonEmpty=" & nonEmpty
	case MatchString   : s += "string """ + *text + """" : if textlength <> len(*text) then s += " textlength=" & textlength & " (INVALID)"
	case MatchWildcard : s += "wildcard"
	case MatchEol      : s += "eol payload=" & payload
	end select
	function = s
end function

sub StringMatcher.dump()
	static nestlevel as integer
	nestlevel += 1
	print space((nestlevel - 1) * 3) + dump1()
	for i as integer = 0 to childcount - 1
		children[i].dump()
	next
	nestlevel -= 1
end sub

''
'' Token buffer (implemented as a gap buffer), accessor functions
''
'' The array of tokens can be accessed via an index through the accessor
'' functions.
''
'' Careful: Some functions such as tkGetLocation() return a pointer that points
'' into the token buffer - it's only valid until the next insertion/deletion.
''

#include once "fbfrog.bi"
#include once "crt.bi"

type TOKENINFO
	text		as zstring ptr
end type

dim shared as TOKENINFO tk_info(0 to ...) = _
{ _
	(@"TK_EOF"     ), _
	(@"TK_BEGIN"   ), _
	(@"TK_END"     ), _
	(@"TK_PPMERGE" ), _
	(@"TK_ARGBEGIN"), _
	(@"TK_ARGEND"  ), _
	(@"TK_EOL"     ), _
	(@"TK_ENDINCLUDE"), _
	(@"TK_FBCODE"  ), _
	(@"TK_NUMBER"  ), _ '' Number/string literals
	(@"TK_STRING"  ), _
	(@"TK_CHAR"    ), _
	(@"TK_WSTRING" ), _
	(@"TK_WCHAR"   ), _
	(@"!"  ), _ '' Main tokens
	(@"!=" ), _
	(@"#"  ), _
	(@"##" ), _
	(@"%"  ), _
	(@"%=" ), _
	(@"&"  ), _
	(@"&=" ), _
	(@"&&" ), _
	(@"("  ), _
	(@")"  ), _
	(@"*"  ), _
	(@"*=" ), _
	(@"+"  ), _
	(@"+=" ), _
	(@"++" ), _
	(@","  ), _
	(@"-"  ), _
	(@"-=" ), _
	(@"--" ), _
	(@"->" ), _
	(@"."  ), _
	(@"..."), _
	(@"/"  ), _
	(@"/=" ), _
	(@":"  ), _
	(@";"  ), _
	(@"<"  ), _
	(@"<<" ), _
	(@"<<="), _
	(@"<=" ), _
	(@"<>" ), _
	(@"="  ), _
	(@"==" ), _
	(@">"  ), _
	(@">>" ), _
	(@">>="), _
	(@">=" ), _
	(@"?"  ), _
	(@"@"  ), _
	(@"["  ), _
	(@"\"  ), _
	(@"]"  ), _
	(@"^"  ), _
	(@"^=" ), _
	(@"_"  ), _
	(@"{"  ), _
	(@"|"  ), _
	(@"|=" ), _
	(@"||" ), _
	(@"}"  ), _
	(@"~"  ), _
	(@"TK_ID" ), _ '' TK_ID
	(@"__attribute__"), _ '' C keywords
	(@"__inline"     ), _
	(@"__inline__"   ), _
	(@"__restrict"   ), _
	(@"__restrict__" ), _
	(@"_Bool"   ), _
	(@"_Pragma" ), _
	(@"auto"    ), _
	(@"break"   ), _
	(@"case"    ), _
	(@"char"    ), _
	(@"const"   ), _
	(@"continue"), _
	(@"default" ), _
	(@"define"  ), _
	(@"defined" ), _
	(@"do"      ), _
	(@"double"  ), _
	(@"elif"    ), _
	(@"else"    ), _
	(@"endif"   ), _
	(@"enum"    ), _
	(@"error"   ), _
	(@"extern"  ), _
	(@"float"   ), _
	(@"for"     ), _
	(@"goto"    ), _
	(@"if"      ), _
	(@"ifdef"   ), _
	(@"ifndef"  ), _
	(@"include" ), _
	(@"inline"  ), _
	(@"int"     ), _
	(@"long"    ), _
	(@"pragma"  ), _
	(@"register"), _
	(@"restrict"), _
	(@"return"  ), _
	(@"short"   ), _
	(@"signed"  ), _
	(@"sizeof"  ), _
	(@"static"  ), _
	(@"struct"  ), _
	(@"switch"  ), _
	(@"typedef" ), _
	(@"undef"   ), _
	(@"union"   ), _
	(@"unsigned"), _
	(@"void"    ), _
	(@"volatile"), _
	(@"warning" ), _
	(@"while"   ), _
	_
	(@"TK_ARGSFILE"), _
	(@"-o"            ), _
	(@"-v"            ), _
	(@"-nodefaultscript"), _
	(@"-windowsms"    ), _
	(@"-clong32"      ), _
	(@"-fixunsizedarrays"), _
	(@"-disableconstants"), _
	(@"-fixmingwaw"   ), _
	(@"-nofunctionbodies"), _
	(@"-dropmacrobodyscopes"), _
	(@"-replacements" ), _
	(@"-renametypedef"), _
	(@"-renametag"    ), _
	(@"-renameproc"   ), _
	(@"-renamedefine" ), _
	(@"-renamemacroparam"), _
	(@"-removedefine" ), _
	(@"-removeproc"   ), _
	(@"-removevar"    ), _
	(@"-dropprocbody" ), _
	(@"-typedefhint"  ), _
	(@"-addforwarddecl"), _
	(@"-undefbeforedecl"), _
	(@"-nostring"     ), _
	(@"-noexpand"     ), _
	(@"-removeinclude"), _
	(@"-setarraysize" ), _
	(@"-moveabove"    ), _
	(@"-define"       ), _
	(@"-include"      ), _
	(@"-fbfroginclude"), _
	(@"-incdir"       ), _
	(@"-inclib"       ), _
	(@"-undef"        ), _
	(@"-addinclude"   ), _
	(@"-emit"         ), _
	(@"-dontemit"     ), _
	(@"-declaredefines"), _
	(@"-declareversions"), _
	(@"-declarebool"  ), _
	(@"-select"       ), _
	(@"-case"         ), _
	(@"-caseelse"     ), _
	(@"-endselect"    ), _
	(@"-ifdef"        ), _
	(@"-else"         ), _
	(@"-endif"        )  _
}

#assert ubound(tk_info) = TK__COUNT - 1

function tkInfoText(byval id as integer) as zstring ptr
	function = tk_info(id).text
end function

function tkInfoPretty(byval id as integer) as string
	select case id
	case TK_EOF    : function = "end of file"
	case TK_EOL    : function = "end of line"
	case TK_ID     : function = "identifier"
	case TK_STRING : function = """..."" string literal"
	case else
		function = "'" + *tk_info(id).text + "'"
	end select
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' TODO: pack
type ONETOKEN
	id		as integer  '' TK_*
	flags		as integer  '' TKFLAG_*

	'' TK_ID: Identifier
	''
	'' TK_STRING: String literal's content with escape sequences solved out,
	'' except for \\ and \0 (so it can still be represented as
	'' null-terminated string)
	''
	'' TK_DECNUM/TK_HEXNUM/TK_OCTNUM: Original token text without octal/hex
	'' prefixes ('0' or '0x'), this is enough for
	''    - parsing code to easily retrieve the integer values by doing
	''      valulng("&h" + *text)
	''    - CPP code to differentiate '0', '0x', '0x0', etc. when doing
	''      ## merging
	''
	'' rest: NULL
	text		as zstring ptr

	location	as TkLocation   '' where this token was found
end type

type TKBUFFER
	'' Gap buffer of tokens
	p		as ONETOKEN ptr  '' Buffer containing: front,gap,back
	front		as integer  '' Front length; the gap's offset
	gap		as integer  '' Gap length
	size		as integer  '' Front + back

	newgapsize	as integer  '' Size to use for new gap when increasing the buffer size

	'' Static EOF token for out-of-bounds accesses
	eof		as ONETOKEN
end type

dim shared as TKBUFFER tk

sub tkInit()
	clear(tk, 0, sizeof(tk))
	tk.newgapsize = 1 shl 12
end sub

private function tkAccess(byval x as integer) as ONETOKEN ptr
	'' Inside end?
	if x >= tk.front then
		'' Invalid?
		if x >= tk.size then
			return @tk.eof
		end if
		x += tk.gap
	else
		'' Invalid?
		if x < 0 then
			return @tk.eof
		end if
	end if

	function = tk.p + x
end function

sub tkEnd()
	tkRemove(0, tk.size - 1)
	deallocate(tk.p)
end sub

private function tkDumpBasic(byval id as integer, byval text as zstring ptr) as string
	var s = "["
	s += *tk_info(id).text
	if text then
		s += " """ + strMakePrintable(*text) + """"
	end if
	s += "]"
	function = s
end function

function tkDumpOne(byval x as integer) as string
	var p = tkAccess(x)
	var s = str(x) + " " + tkDumpBasic(p->id, p->text)

	#macro checkFlag(a)
		if tkGetFlags(x) and TKFLAG_##a then s += " " + lcase(#a, 1)
	#endmacro
	checkFlag(BEHINDSPACE)
	checkFlag(NOEXPAND)
	checkFlag(REMOVE)
	checkFlag(ROOTFILE)
	checkFlag(PREINCLUDE)
	checkFlag(DIRECTIVE)
	checkFlag(EXPANSION)

	's += " " + hDumpLocation(@p->location)

	function = s
end function

sub tkDump overload(byval first as integer, byval last as integer)
	for i as integer = first to last
		print tkDumpOne(i)
	next
end sub

sub tkDump overload()
	tkDump(0, tk.size - 1)
end sub

private sub hMoveTo(byval x as integer)
	if x < 0 then
		x = 0
	elseif x > tk.size then
		x = tk.size
	end if

	var old = tk.front
	if x < old then
		'' Move gap left
		var p = tk.p + x
		memmove(p + tk.gap, p, (old - x) * sizeof(ONETOKEN))
	elseif x > old then
		'' Move gap right
		var p = tk.p + old
		memmove(p, p + tk.gap, (x - old) * sizeof(ONETOKEN))
	end if

	tk.front = x
end sub

function tkGetCount() as integer
	function = tk.size
end function

'' Insert new token in front of token at the given position,
'' so that the new token ends up at that position
sub tkInsert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr _
	)

	dim as ONETOKEN ptr p = any

	'' Move gap in front of the position
	hMoveTo(x)

	'' Make room for the new data, if necessary
	if tk.gap = 0 then
		'' Reallocate the buffer, then move the back block to the
		'' end of the new buffer, so that the gap in the middle grows.
		tk.newgapsize shl= 1
		tk.p = reallocate(tk.p, (tk.size + tk.newgapsize) * sizeof(ONETOKEN))
		p = tk.p + tk.front
		if tk.size > tk.front then
			memmove(p + tk.newgapsize, p + tk.gap, _
			         (tk.size - tk.front) * sizeof(ONETOKEN))
		end if
		tk.gap = tk.newgapsize
	else
		p = tk.p + tk.front
	end if

	clear(*p, 0, sizeof(*p))
	p->id = id
	p->text = strDuplicate(text)

	'' Extend front part of the buffer
	tk.front += 1
	tk.gap -= 1
	tk.size += 1

end sub

sub tkRemove(byval first as integer, byval last as integer)
	if first < 0 then first = 0
	if last >= tk.size then last = tk.size - 1
	if first > last then exit sub

	for i as integer = first to last
		var p = tkAccess(i)
		deallocate(p->text)
	next

	var delta = last - first + 1

	'' Gap is in front of first token to delete?
	if tk.front = first then
		'' Then do a forward deletion
		assert(delta <= (tk.size - tk.front))
	else
		'' Otherwise, move the gap behind the last token,
		'' and do a backwards deletion
		hMoveTo(last + 1)
		assert(delta <= tk.front)
		tk.front -= delta
	end if

	tk.gap += delta
	tk.size -= delta
end sub

'' Copy tokens first..last and insert them in front of x
sub tkCopy _
	( _
		byval x as integer, _
		byval first as integer, _
		byval last as integer, _
		byval flagmask as integer _
	)

	if first < 0 then first = 0
	if last >= tk.size then last = tk.size - 1
	if first > last then exit sub
	if (x < 0) or (x > tk.size) then exit sub
	assert((x <= first) or (x > last))

	do
		var src = tkAccess(first)
		tkInsert(x, src->id, src->text)
		'' Careful when inserting before the source range, the position
		'' offsets shift by 1 everytime
		if x <= first then
			first += 1
			last += 1
		end if

		src = tkAccess(first)
		var dst = tkAccess(x)
		dst->flags          = src->flags and flagmask
		dst->location       = src->location

		x += 1
		first += 1
	loop while first <= last
end sub

function tkGet(byval x as integer) as integer
	function = tkAccess(x)->id
end function

function tkGetText(byval x as integer) as zstring ptr
	function = tkAccess(x)->text
end function

function tkSpellId(byval x as integer) as zstring ptr
	var p = tkAccess(x)
	assert(p->id >= TK_ID)
	if p->id = TK_ID then
		function = p->text
	else
		function = tk_info(p->id).text
	end if
end function

sub tkSetLocation(byval x as integer, byval location as TkLocation)
	var p = tkAccess(x)
	if p->id <> TK_EOF then
		p->location = location
	end if
end sub

function tkGetLocation(byval x as integer) as TkLocation
	function = tkAccess(x)->location
end function

sub tkSetFlags(byval x as integer, byval flags as integer)
	var p = tkAccess(x)
	if p->id <> TK_EOF then
		p->flags = flags
	end if
end sub

sub tkAddFlags(byval first as integer, byval last as integer, byval flags as integer)
	for x as integer = first to last
		var p = tkAccess(x)
		if p->id <> TK_EOF then
			p->flags or= flags
		end if
	next
end sub

sub tkSetRemove overload(byval x as integer)
	tkAddFlags(x, x, TKFLAG_REMOVE)
end sub

sub tkSetRemove overload(byval first as integer, byval last as integer)
	tkAddFlags(first, last, TKFLAG_REMOVE)
end sub

function tkGetFlags(byval x as integer) as integer
	function = tkAccess(x)->flags
end function

sub tkApplyRemoves()
	var x = 0
	while tkGet(x) <> TK_EOF
		if tkGetFlags(x) and TKFLAG_REMOVE then
			tkRemove(x, x)
			x -= 1
		end if
		x += 1
	wend
end sub

sub tkTurnCPPTokensIntoCIds()
	for x as integer = 0 to tkGetCount()-1
		var tk = tkGet(x)
		select case tk
		case KW_DEFINE, KW_DEFINED, KW_INCLUDE, KW_ELIF, KW_IFDEF, KW_IFNDEF, _
		     KW_ENDIF, KW_UNDEF, KW_PRAGMA, KW_ERROR, KW_WARNING
			'' Turn the KW_* into a TK_ID, but preserve the flags/macro expansion level/etc.
			var p = tkAccess(x)
			p->id = TK_ID
			p->text = strDuplicate(tkInfoText(tk))
		end select
	next
end sub

function tkCTokenRangesAreEqual(byval a as integer, byval b as integer, byval length as integer) as integer
	while length > 0
		var pa = tkAccess(a)
		var pb = tkAccess(b)

		if pa->id <> pb->id then exit function
		if (pa->text <> NULL) <> (pb->text <> NULL) then exit function
		if pa->text then
			if *pa->text <> *pb->text then exit function
		end if

		a += 1
		b += 1
		length -= 1
	wend
	function = TRUE
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function hSpellStrLit(byval text as zstring ptr) as string
	dim s as string

	do
		select case (*text)[0]
		case 0
			exit do

		'' Internal format: can contain \\ and \0 escape sequences to
		'' encode embedded null chars
		case CH_BACKSLASH
			text += 1
			assert((text[0] = CH_BACKSLASH) or (text[0] = CH_0))
			s += "\"
			s += chr((*text)[0])

		case CH_DQUOTE    : s += "\"""
		case CH_QUOTE     : s += "\'"
		case CH_QUEST     : s += "\?"
		case CH_BELL      : s += "\a"
		case CH_BACKSPACE : s += "\b"
		case CH_FORMFEED  : s += "\f"
		case CH_LF        : s += "\n"
		case CH_CR        : s += "\r"
		case CH_TAB       : s += "\t"
		case CH_VTAB      : s += "\v"
		case is < 32, 127 : s += "\" + oct((*text)[0])
		case else         : s += chr((*text)[0])
		end select

		text += 1
	loop

	function = s
end function

function tkSpell overload(byval x as integer) as string
	dim as string s

	var id = tkGet(x)
	var text = tkGetText(x)
	var flags = tkGetFlags(x)

	select case as const id
	case TK_EOL      : s = !"\n"
	case TK_PPMERGE  : s = "##"
	case TK_STRING   : s = """"  + hSpellStrLit(*text) + """"
	case TK_CHAR     : s = "'"   + hSpellStrLit(*text) + "'"
	case TK_WSTRING  : s = "L""" + hSpellStrLit(*text) + """"
	case TK_WCHAR    : s = "L'"  + hSpellStrLit(*text) + "'"
	case TK_NUMBER   : s = *text
	case TK_ID       : s = *text
	case TK_ARGSFILE : s = "@" + *text

	case TK_EXCL to TK_TILDE, KW__C_FIRST to KW__C_LAST, OPT__FIRST to OPT__LAST
		s = *tk_info(id).text

	case else
		s = tkDumpBasic(id, text)
	end select

	function = s
end function

function tkSpell overload(byval first as integer, byval last as integer) as string
	dim as string s

	for i as integer = first to last
		if tkGet(i) = TK_EOL then
			assert(i = last)
			exit for
		end if

		if i > first then
			var add_space = ((tkGetFlags(i) and TKFLAG_BEHINDSPACE) <> 0)
			add_space or= (tkGet(i - 1) >= TK_ID) and (tkGet(i) >= TK_ID)
			if add_space then
				if right(s, 1) <> " " then s += " "
			end if
		end if

		s += tkSpell(i)
	next

	function = s
end function

function hFindClosingParen _
	( _
		byval x as integer, _
		byval inside_directive as integer, _
		byval ignore_directive as integer _
	) as integer

	var opening = tkGet(x)
	var level = 0

	dim as integer closing
	select case opening
	case TK_LBRACE   : closing = TK_RBRACE
	case TK_LBRACKET : closing = TK_RBRACKET
	case TK_LPAREN   : closing = TK_RPAREN
	case else
		assert(FALSE)
	end select

	do
		x += 1

		select case tkGet(x)
		case opening
			level += 1

		case closing
			if level = 0 then
				exit do
			end if
			level -= 1

		'' Stop at # (CPP directives)?
		case TK_HASH
			if tkIsOriginal(x) and (not inside_directive) then
				if ignore_directive = FALSE then
					x -= 1
					exit do
				end if
				x = hSkipToEol(x)
			end if

		case TK_EOL
			if inside_directive then
				exit do
			end if

		case TK_EOF, TK_ENDINCLUDE
			x -= 1
			exit do
		end select
	loop

	function = x
end function

function tkIsEolOrEof(byval x as integer) as integer
	var tk = tkGet(x)
	function = (tk = TK_EOL) or (tk = TK_EOF)
end function

function hSkipToEol(byval x as integer) as integer
	while tkIsEolOrEof(x) = FALSE
		x += 1
	wend
	function = x
end function

function hSkipConstruct(byval x as integer, byval ignore_directives as integer) as integer
	select case as const tkGet(x)
	case TK_EOF
		return x

	case TK_SEMI, TK_ENDINCLUDE, TK_FBCODE
		return x + 1

	case TK_HASH
		if tkIsOriginal(x) then
			if ignore_directives = FALSE then
				return hSkipToEol(x) + 1
			end if
			x = hSkipToEol(x)
		end if
	end select

	do
		x += 1

		select case as const tkGet(x)
		case TK_SEMI
			x += 1
			exit do

		case TK_EOF, TK_ENDINCLUDE, TK_RBRACE, TK_EOL, TK_FBCODE
			'' '}': Apparently we didn't see the '{' of this '{...}' block (otherwise we would
			'' have skipped the whole block already), thus we're inside the block and this construct
			'' is a nested one. Thus the '}' indicates the end of the construct, and is not part of it.
			'' EOL: The same, just for #define bodies.
			exit do

		case TK_HASH
			if tkIsOriginal(x) then
				if ignore_directives = FALSE then
					exit do
				end if
				x = hSkipToEol(x)
			end if

		case TK_LBRACE
			var probably_is_function_body = (tkGet(x - 1) = TK_RPAREN)

			x = hFindClosingParen(x, FALSE, ignore_directives)

			'' Stop after function bodies
			if probably_is_function_body and (tkGet(x) = TK_RBRACE) then
				x += 1
				exit do
			end if

		case TK_LPAREN, TK_LBRACKET
			x = hFindClosingParen(x, FALSE, ignore_directives)

		end select
	loop

	function = x
end function

private sub hFindConstructBoundaries(byval x as integer, byref first as integer, byref last as integer)
	'' Start at BOF and find the construct that contains x (easier than
	'' parsing backwards!?)
	var y = 0
	while tkGet(y) <> TK_EOF
		var begin = y
		y = hSkipConstruct(y, FALSE)
		if (begin <= x) and (x < y) then
			first = begin
			last = y - 1
			exit sub
		end if
	wend
	first = 0
	last = tkGetCount() - 1
end sub

private function hReportConstructTokens _
	( _
		byval x as integer, _
		byval first as integer, _
		byval last as integer _
	) as string

	const INDENT = 4
	const MAXWIDTH = 80 - INDENT

	var xcolumn = -1, xlength = 0

	dim text as string
	for i as integer = first to last
		if (len(text) > 0) and (right(text, 1) <> " ") then
			text += " "
		end if

		if i = x then
			xcolumn = len(text)
		end if

		if tkIsEolOrEof(i) = FALSE then
			text += tkSpell(i)
		end if

		if i = x then
			xlength = len(text) - xcolumn
		end if
	next

	dim offset as integer
	hCalcErrorLine(xcolumn, MAXWIDTH, text, offset)

	function = _
		!"\n" + space(INDENT) + text + _
		!"\n" + hErrorMarker(INDENT + offset, xlength)
end function

'' Report a message about some token that is part of some construct.
'' Besides showing the message, this should also show the code where the error
'' was encountered.
function tkReport(byval x as integer, byval message as zstring ptr) as string
	if x >= tkGetCount() then
		x = tkGetCount()-1
	end if

	dim as integer first, last
	hFindConstructBoundaries(x, first, last)

	if tkGet(x) = TK_END   then x -= 1
	if tkGet(x) = TK_BEGIN then x -= 1

	function = hReport(tkGetLocation(x), message) + _
	           hReportConstructTokens(x, first, last)
end function

sub tkOops(byval x as integer, byval message as zstring ptr)
	print tkReport(x, message)
	end 1
end sub

function tkButFound(byval x as integer) as string
	function = " but found '" + tkSpell(x) + "'"
end function

function tkMakeExpectedMessage(byval x as integer, byval something as zstring ptr) as string
	select case tkGet(x)
	case TK_EOL, TK_EOF, TK_END
		function = "missing " + *something
	case else
		function = "expected " + *something + tkButFound(x)
	end select
end function

sub tkOopsExpected(byval x as integer, byval message as zstring ptr)
	print tkReport(x, tkMakeExpectedMessage(x, message))
	end 1
end sub

sub tkExpect(byval x as integer, byval tk as integer, byval message as zstring ptr)
	if tkGet(x) <> tk then
		tkOopsExpected(x, tkInfoPretty(tk) + " " + *message)
	end if
end sub

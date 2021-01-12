''
'' Token buffer (implemented as a gap buffer), accessor functions
''
'' The array of tokens can be accessed via an index through the accessor
'' functions.
''
'' Careful: Some functions such as tkGetLocation() return a pointer that points
'' into the token buffer - it's only valid until the next insertion/deletion.
''

#include once "tk.bi"

#include once "util-str.bi"
#include once "util.bi"

#include once "crt.bi"

using tktokens

type TOKENINFO
	text		as zstring ptr
end type

dim shared as const TOKENINFO tk_info(0 to ...) = _
{ _
	(@"TK_EOF"     ), _
	(@"TK_STRAYBYTE"), _
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
	(@"="  ), _
	(@"==" ), _
	(@">"  ), _
	(@">>" ), _
	(@">>="), _
	(@">=" ), _
	(@"?"  ), _
	(@"["  ), _
	(@"]"  ), _
	(@"^"  ), _
	(@"^=" ), _
	(@"{"  ), _
	(@"|"  ), _
	(@"|=" ), _
	(@"||" ), _
	(@"}"  ), _
	(@"~"  ), _
	(@"TK_ID" ), _ '' TK_ID
	(@"__asm"        ), _ '' C keywords
	(@"__asm__"      ), _
	(@"__attribute"  ), _
	(@"__attribute__"), _
	(@"__inline"     ), _
	(@"__inline__"   ), _
	(@"__restrict"   ), _
	(@"__restrict__" ), _
	(@"_Bool"   ), _
	(@"_Pragma" ), _
	(@"asm"     ), _
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
	(@"include_next"), _
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
	_
	(@"-o"            ), _
	(@"-emit"         ), _
	(@"-dontemit"     ), _
	(@"-i"            ), _
	(@"-v"            ), _
	(@"-target"       ), _
	(@"-title"        ), _
	_
	(@"-declareversions"), _
	(@"-declarebool"  ), _
	(@"-selecttarget" ), _
	(@"-selectversion"), _
	(@"-selectdefine" ), _
	(@"-case"         ), _
	(@"-caseelse"     ), _
	(@"-endselect"    ), _
	(@"-iftarget"     ), _
	(@"-ifdef"        ), _
	(@"-else"         ), _
	(@"-endif"        ), _
	_
	(@"-define"       ), _
	(@"-include"      ), _
	(@"-fbfroginclude"), _
	(@"-incdir"       ), _
	_
	(@"-windowsms"    ), _
	(@"-clong32"      ), _
	(@"-fixunsizedarrays"), _
	(@"-nofunctionbodies"), _
	(@"-dropmacrobodyscopes"), _
	(@"-replacements" ), _
	(@"-renametypedef"), _
	(@"-renametag"    ), _
	(@"-renameproc"   ), _
	(@"-renamedefine" ), _
	(@"-renamemacroparam"), _
	(@"-rename"       ), _
	(@"-removeEmptyReservedDefines"), _
	(@"-rename_"      ), _
	(@"-remove"       ), _
	(@"-removedefine" ), _
	(@"-removeproc"   ), _
	(@"-removevar"    ), _
	(@"-remove1st"    ), _
	(@"-remove2nd"    ), _
	(@"-dropprocbody" ), _
	(@"-typedefhint"  ), _
	(@"-addforwarddecl"), _
	(@"-undefbeforedecl"), _
	(@"-ifndefdecl"   ), _
	(@"-convbodytokens"), _
	(@"-forcefunction2macro"), _
	(@"-expandindefine"), _
	(@"-noexpand"     ), _
	(@"-expand"       ), _
	(@"-nostring"     ), _
	(@"-string"       ), _
	(@"-removeinclude"), _
	(@"-setarraysize" ), _
	(@"-moveabove"    ), _
	(@"-inclib"       ), _
	(@"-undef"        ), _
	(@"-addinclude"   )  _
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

constructor TokenBuffer(byval sourcectx as SourceContext ptr)
	this.sourcectx = sourcectx
	newgapsize = 1 shl 12
end constructor

destructor TokenBuffer()
	remove(0, size - 1)
	deallocate(buffer)
end destructor

'' Static EOF token for out-of-bounds accesses
dim shared EOF_TOKEN as ONETOKEN = (NULL, type<TkLocation>(0), TK_EOF, 0)

function TokenBuffer.lookup(byval x as integer) as ONETOKEN ptr
	'' Inside end?
	if x >= front then
		'' Invalid?
		if x >= size then
			return @EOF_TOKEN
		end if
		x += gap
	else
		'' Invalid?
		if x < 0 then
			return @EOF_TOKEN
		end if
	end if
	return buffer + x
end function

private function tkDumpBasic(byval id as integer, byval text as zstring ptr) as string
	var s = "["
	s += *tk_info(id).text
	if text then
		s += " " + strMakePrintable(*text)
	end if
	s += "]"
	function = s
end function

function TokenBuffer.dumpOne(byval x as integer) as string
	var p = lookup(x)
	var s = str(x) + " " + tkDumpBasic(p->id, p->text)

	#macro checkFlag(a)
		if getFlags(x) and TKFLAG_##a then s += " " + lcase(#a, 1)
	#endmacro
	checkFlag(BEHINDSPACE)
	checkFlag(NOEXPAND)
	checkFlag(REMOVE)
	checkFlag(STARTOFDIRECTIVE)
	checkFlag(ROOTFILE)
	checkFlag(PREINCLUDE)
	checkFlag(DIRECTIVE)
	checkFlag(EXPANSION)

	s += " " + sourcectx->dump(p->location)

	function = s
end function

sub TokenBuffer.dump(byval first as integer, byval last as integer)
	for i as integer = first to last
		print dumpOne(i)
	next
end sub

sub TokenBuffer.dump()
	dump(0, size - 1)
end sub

sub TokenBuffer.moveTo(byval x as integer)
	if x < 0 then
		x = 0
	elseif x > size then
		x = size
	end if
	var old = front
	if x < old then
		'' Move gap left
		var p = buffer + x
		memmove(p + gap, p, (old - x) * sizeof(ONETOKEN))
	elseif x > old then
		'' Move gap right
		var p = buffer + old
		memmove(p, p + gap, (x - old) * sizeof(ONETOKEN))
	end if
	front = x
end sub

function TokenBuffer.count() as integer
	return size
end function

'' Insert new token in front of token at the given position,
'' so that the new token ends up at that position
sub TokenBuffer.insert(byval x as integer, byval id as integer, byval text as zstring ptr)
	'' Move gap in front of the position
	moveTo(x)

	'' Make room for the new data, if necessary
	dim p as ONETOKEN ptr
	if gap = 0 then
		'' Reallocate the buffer, then move the back block to the
		'' end of the new buffer, so that the gap in the middle grows.
		newgapsize shl= 1
		buffer = reallocate(buffer, (size + newgapsize) * sizeof(ONETOKEN))
		if buffer = NULL then
			oops("tk buffer memory allocation failed")
		end if
		p = buffer + front
		if size > front then
			memmove(p + newgapsize, p + gap, _
			         (size - front) * sizeof(ONETOKEN))
		end if
		gap = newgapsize
	else
		p = buffer + front
	end if

	clear(*p, 0, sizeof(*p))
	p->id = id
	p->text = strDuplicate(text)

	'' Extend front part of the buffer
	front += 1
	gap -= 1
	size += 1
end sub

sub TokenBuffer.remove(byval first as integer, byval last as integer)
	if first < 0 then first = 0
	if last >= size then last = size - 1
	if first > last then exit sub

	for i as integer = first to last
		deallocate(lookup(i)->text)
	next

	var delta = last - first + 1

	'' Gap is in front of first token to delete?
	if front = first then
		'' Then do a forward deletion
		assert(delta <= (size - front))
	else
		'' Otherwise, move the gap behind the last token,
		'' and do a backwards deletion
		moveTo(last + 1)
		assert(delta <= front)
		front -= delta
	end if

	gap += delta
	size -= delta
end sub

'' Copy tokens first..last and insert them in front of x
sub TokenBuffer.copy(byval x as integer, byval first as integer, byval last as integer, byval flagmask as integer)
	if first < 0 then first = 0
	if last >= size then last = size - 1
	if first > last then exit sub
	if (x < 0) or (x > size) then exit sub
	assert((x <= first) or (x > last))

	do
		scope
			var src = lookup(first)
			insert(x, src->id, src->text)
		end scope

		'' Careful when inserting before the source range, the position
		'' offsets shift by 1 everytime
		if x <= first then
			first += 1
			last += 1
		end if

		scope
			var src = lookup(first)
			var dst = lookup(x)
			dst->flags    = src->flags and flagmask
			dst->location = src->location
		end scope

		x += 1
		first += 1
	loop while first <= last
end sub

function TokenBuffer.get(byval x as integer) as integer
	return lookup(x)->id
end function

function TokenBuffer.getText(byval x as integer) as zstring ptr
	return lookup(x)->text
end function

function TokenBuffer.spellId(byval x as integer) as zstring ptr
	var p = lookup(x)
	assert(p->id >= TK_ID)
	if p->id = TK_ID then
		function = p->text
	else
		function = tk_info(p->id).text
	end if
end function

sub TokenBuffer.setLocation(byval x as integer, byval location as TkLocation)
	var p = lookup(x)
	if p->id <> TK_EOF then
		p->location = location
	end if
end sub

function TokenBuffer.getLocation(byval x as integer) as TkLocation
	return lookup(x)->location
end function

sub TokenBuffer.setFlags(byval x as integer, byval flags as integer)
	var p = lookup(x)
	if p->id <> TK_EOF then
		p->flags = flags
	end if
end sub

sub TokenBuffer.addFlags(byval first as integer, byval last as integer, byval flags as integer)
	for x as integer = first to last
		var p = lookup(x)
		if p->id <> TK_EOF then
			p->flags or= flags
		end if
	next
end sub

sub TokenBuffer.setRemove(byval x as integer)
	addFlags(x, x, TKFLAG_REMOVE)
end sub

sub TokenBuffer.setRemove(byval first as integer, byval last as integer)
	addFlags(first, last, TKFLAG_REMOVE)
end sub

function TokenBuffer.getFlags(byval x as integer) as integer
	return lookup(x)->flags
end function

sub TokenBuffer.applyRemoves()
	var x = 0
	while get(x) <> TK_EOF
		if getFlags(x) and TKFLAG_REMOVE then
			remove(x, x)
			x -= 1
		end if
		x += 1
	wend
end sub

function TokenBuffer.isOriginal(byval x as integer) as integer
	return ((getFlags(x) and TKFLAG_EXPANSION) = 0)
end function

function TokenBuffer.isDirective(byval x as integer) as integer
	return ((getFlags(x) and TKFLAG_DIRECTIVE) <> 0)
end function

function TokenBuffer.isStartOfDirective(byval x as integer) as integer
	return ((getFlags(x) and TKFLAG_STARTOFDIRECTIVE) <> 0)
end function

function TokenBuffer.isKwThatShouldBecomeId(byval x as integer) as integer
	select case get(x)
	case KW_DEFINE, KW_INCLUDE, KW_INCLUDE_NEXT, KW_ELIF, KW_IFDEF, KW_IFNDEF, _
	     KW_ENDIF, KW_UNDEF, KW_PRAGMA, KW_ERROR, KW_WARNING
		function = TRUE

	case KW_DEFINED
		'' Allow "defined" to stay a keyword if inside a macro body.
		'' This way we can translate macro bodies doing "defined(...)",
		'' without misinterpreting the defined() as function call.
		function = (not isDirective(x))
	end select
end function

sub TokenBuffer.turnCPPTokensIntoCIds()
	for x as integer = 0 to count()-1
		if isKwThatShouldBecomeId(x) then
			'' Turn the KW_* into a TK_ID, but preserve the flags/macro expansion level/etc.
			var p = lookup(x)
			p->text = strDuplicate(tkInfoText(p->id))
			p->id = TK_ID
		end if
	next
end sub

function TokenBuffer.areCTokenRangesEqual(byval a as integer, byval b as integer, byval length as integer) as integer
	while length > 0
		var pa = lookup(a)
		var pb = lookup(b)

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

function TokenBuffer.spell(byval x as integer) as string
	dim as string s

	var id = get(x)
	var text = getText(x)

	select case as const id
	case TK_PPMERGE  : s = "##"
	case TK_ARGSFILE : s = "@" + *text
	case TK_STRING, TK_CHAR, TK_WSTRING, TK_WCHAR, TK_NUMBER, TK_ID
		s = *text
	case TK_EXCL to TK_TILDE, KW__C_FIRST to KW__C_LAST, OPT__FIRST to OPT__LAST
		s = *tk_info(id).text
	case else
		s = tkDumpBasic(id, text)
	end select

	function = s
end function

function TokenBuffer.spell(byval first as integer, byval last as integer) as string
	dim as string s
	for i as integer = first to last
		if i > first then
			var add_space = ((getFlags(i) and TKFLAG_BEHINDSPACE) <> 0)
			add_space or= (get(i - 1) >= TK_ID) and (get(i) >= TK_ID)
			add_space and= not ((len(s) > 0) and (right(s, 1) = " "))
			if add_space then
				s += " "
			end if
		end if
		if get(i) <> TK_EOL then
			s += spell(i)
		end if
	next
	if right(s, 1) = " " then
		s = left(s, len(s) - 1)
	end if
	function = s
end function

function TokenBuffer.findClosingParen(byval x as integer, byval inside_directive as integer, byval ignore_directive as integer) as integer
	var opening = get(x)
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

		select case get(x)
		case opening
			level += 1

		case closing
			if level = 0 then
				exit do
			end if
			level -= 1

		'' Stop at # (CPP directives)?
		case TK_HASH
			if isStartOfDirective(x) and (not inside_directive) then
				if ignore_directive = FALSE then
					x -= 1
					exit do
				end if
				x = skipToEol(x)
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

function TokenBuffer.isEolOrEof(byval x as integer) as integer
	var tk = get(x)
	function = (tk = TK_EOL) or (tk = TK_EOF)
end function

function TokenBuffer.skipToEol(byval x as integer) as integer
	while isEolOrEof(x) = FALSE
		x += 1
	wend
	function = x
end function

function TokenBuffer.skipConstruct(byval x as integer, byval ignore_directives as integer) as integer
	select case as const get(x)
	case TK_EOF
		return x

	case TK_SEMI, TK_ENDINCLUDE, TK_FBCODE
		return x + 1

	case TK_HASH
		if isStartOfDirective(x) then
			if ignore_directives = FALSE then
				return skipToEol(x) + 1
			end if
			x = skipToEol(x)
		end if
	end select

	do
		x += 1

		select case as const get(x)
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
			if isStartOfDirective(x) then
				if ignore_directives = FALSE then
					exit do
				end if
				x = skipToEol(x)
			end if

		case TK_LBRACE
			var probably_is_function_body = (get(x - 1) = TK_RPAREN)

			x = findClosingParen(x, FALSE, ignore_directives)

			'' Stop after function bodies
			if probably_is_function_body and (get(x) = TK_RBRACE) then
				x += 1
				exit do
			end if

		case TK_LPAREN, TK_LBRACKET
			x = findClosingParen(x, FALSE, ignore_directives)

		end select
	loop

	function = x
end function

sub TokenBuffer.findConstructBoundaries(byval x as integer, byref first as integer, byref last as integer)
	'' Start at BOF and find the construct that contains x (easier than
	'' parsing backwards!?)
	var y = 0
	while get(y) <> TK_EOF
		var begin = y
		y = skipConstruct(y, FALSE)
		if (begin <= x) and (x < y) then
			first = begin
			last = y - 1
			exit sub
		end if
	wend
	first = 0
	last = count() - 1
end sub

function TokenBuffer.reportConstructTokens(byval x as integer, byval first as integer, byval last as integer) as string
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

		if isEolOrEof(i) = FALSE then
			text += spell(i)
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
function TokenBuffer.report(byval x as integer, byval message as zstring ptr) as string
	if x >= count() then
		x = count()-1
	end if

	dim as integer first, last
	findConstructBoundaries(x, first, last)

	if get(x) = TK_END   then x -= 1
	if get(x) = TK_BEGIN then x -= 1

	function = hReport(sourcectx->decode(getLocation(x)), message) + reportConstructTokens(x, first, last)
end function

sub TokenBuffer.showErrorAndAbort(byval x as integer, byval message as zstring ptr)
	print report(x, message)
	end 1
end sub

function TokenBuffer.butFound(byval x as integer) as string
	function = " but found '" + spell(x) + "'"
end function

function TokenBuffer.makeExpectedMessage(byval x as integer, byval something as zstring ptr) as string
	select case get(x)
	case TK_EOL, TK_EOF, TK_END
		function = "missing " + *something
	case else
		function = "expected " + *something + butFound(x)
	end select
end function

sub TokenBuffer.oopsExpected(byval x as integer, byval message as zstring ptr)
	print report(x, makeExpectedMessage(x, message))
	end 1
end sub

sub TokenBuffer.expect(byval x as integer, byval tk as integer, byval message as zstring ptr)
	if get(x) <> tk then
		oopsExpected(x, tkInfoPretty(tk) + " " + *message)
	end if
end sub

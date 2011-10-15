#include once "fbfrog.bi"
#include once "crt.bi"

dim shared as zstring ptr token_text(0 to (TK__COUNT - 1)) = _
{ _
	@"<eof>"        , _
	@"<todo>"       , _
	@"<byte>"       , _
	@"<eol>"        , _
	@"<space>"      , _
	@"<comment>"    , _
	@"<linecomment>", _
	@"<decnum>"     , _ '' Number literals
	@"<hexnum>"     , _
	@"<octnum>"     , _
	@"<string>"     , _ '' String literals
	@"<char>"       , _
	@"<wstring>"    , _
	@"<wchar>"      , _
	@"<estring>"    , _
	@"<echar>"      , _
	@"<ewstring>"   , _
	@"<ewchar>"     , _
	@"!"      , _ '' Main tokens
	@"<>"     , _ '' !=
	@"#"      , _
	@"##"     , _
	@"mod"    , _ '' %
	@"mod="   , _ '' %=
	@"and"    , _ '' &
	@"and="   , _ '' &=
	@"andalso", _ '' &&
	@"("      , _
	@")"      , _
	@"*"      , _
	@"*="     , _
	@"+"      , _
	@"+="     , _
	@"++"     , _
	@","      , _
	@"-"      , _
	@"-="     , _
	@"--"     , _
	@"->"     , _
	@"."      , _
	@"..."    , _
	@"/"      , _
	@"/="     , _
	@":"      , _
	@";"      , _
	@"<"      , _
	@"shl"    , _ '' <<
	@"shl="   , _ '' <<=
	@"<="     , _
	@"="      , _
	@"="      , _ '' ==
	@">"      , _
	@"shr"    , _ '' >>
	@"shr="   , _ '' >>=
	@">="     , _
	@"?"      , _
	@"["      , _
	@"\"      , _
	@"]"      , _
	@"xor"    , _ '' ^
	@"xor="   , _ '' ^=
	@"_"      , _
	@"{"      , _
	@"or"     , _ '' |
	@"or="    , _ '' |=
	@"orelse" , _ '' ||
	@"}"      , _
	@"not"    , _ '' ~
	@"<id>"      , _ '' TK_ID
	@"auto"      , _ '' C/FB keywords
	@"break"     , _
	@"case"      , _
	@"char"      , _
	@"const"     , _
	@"continue"  , _
	@"default"   , _
	@"define"    , _
	@"defined"   , _
	@"do"        , _
	@"double"    , _
	@"elif"      , _
	@"else"      , _
	@"endif"     , _
	@"enum"      , _
	@"extern"    , _
	@"float"     , _
	@"for"       , _
	@"goto"      , _
	@"if"        , _
	@"ifdef"     , _
	@"ifndef"    , _
	@"include"   , _
	@"inline"    , _
	@"int"       , _
	@"long"      , _
	@"pragma"    , _
	@"register"  , _
	@"restrict"  , _
	@"return"    , _
	@"short"     , _
	@"signed"    , _
	@"sizeof"    , _
	@"static"    , _
	@"struct"    , _
	@"switch"    , _
	@"typedef"   , _
	@"undef"     , _
	@"union"     , _
	@"unsigned"  , _
	@"void"      , _
	@"volatile"  , _
	@"while"     , _
	@"alias"      , _  '' FB-only keywords
	@"any"        , _
	@"as"         , _
	@"byte"       , _
	@"byval"      , _
	@"cast"       , _
	@"cdecl"      , _
	@"cptr"       , _
	@"declare"    , _
	@"dim"        , _
	@"elseif"     , _
	@"end"        , _
	@"exit"       , _
	@"export"     , _
	@"field"      , _
	@"function"   , _
	@"iif"        , _
	@"integer"    , _
	@"longint"    , _
	@"loop"       , _
	@"next"       , _
	@"pascal"     , _
	@"private"    , _
	@"ptr"        , _
	@"scope"      , _
	@"select"     , _
	@"shared"     , _
	@"single"     , _
	@"stdcall"    , _
	@"sub"        , _
	@"then"       , _
	@"to"         , _
	@"type"       , _
	@"ubyte"      , _
	@"uinteger"   , _
	@"ulong"      , _
	@"ulongint"   , _
	@"ushort"     , _
	@"wend"       , _
	@"wstr"       , _
	@"wstring"    , _
	@"zstring"      _
}

dim shared as zstring ptr mark_text(0 to (MARK__COUNT - 1)) = _
{ _
	@"", _
	@"pp", _
	@"extern", _
	@"endextern", _
	@"struct", _
	@"endenum", _
	@"endstruct", _
	@"endunion", _
	@"enumconst", _
	@"typedef", _
	@"topdecl", _
	@"procdecl", _
	@"vardecl", _
	@"fielddecl" _
}

type OneToken field = 1
	as short id         '' TK_*
	as short mark       '' MARK_*
	as zstring ptr text '' Identifiers and number/string literals, or NULL
end type

type TokenBuffer
	'' Gap buffer of tokens
	as OneToken ptr p   '' Buffer containing: front,gap,back
	as integer front    '' Front length; the gap's offset
	as integer gap      '' Gap length
	as integer size     '' Front + back
end type

dim shared as TokenBuffer tk

private function direct_ptr(byval x as integer) as OneToken ptr
	'' Static EOF token for "error recovery"
	static as OneToken static_eof = (TK_EOF, MARK_TOPLEVEL, NULL)

	'' Inside end?
	if (x >= tk.front) then
		'' Invalid?
		if (x >= tk.size) then
			return @static_eof
		end if
		x += tk.gap
	else
		'' Invalid?
		if (x < 0) then
			return @static_eof
		end if
	end if

	return tk.p + x
end function

private sub tk_move_to(byval x as integer)
	if (x < 0) then
		x = 0
	elseif (x > tk.size) then
		x = tk.size
	end if

	dim as integer old = tk.front
	if (x < old) then
		'' Move gap left
		dim as OneToken ptr p = tk.p + x
		memmove(p + tk.gap, p, (old - x) * sizeof(OneToken))
	elseif (x > old) then
		'' Move gap right
		dim as OneToken ptr p = tk.p + old
		memmove(p, p + tk.gap, (x - old) * sizeof(OneToken))
	end if

	tk.front = x
end sub

'' Insert token at current position, the current position moves forward.
private sub tk_in_raw _
	( _
		byval id as integer, _
		byval text as ubyte ptr, _
		byval length as integer _
	)

	dim as OneToken ptr p = any

	'' Make room for the new data, if necessary
	if (tk.gap = 0) then
		const NEWGAP = 512

		tk.p = xreallocate(tk.p, (tk.size + NEWGAP) * sizeof(OneToken))
		p = tk.p + tk.front

		'' Move the back block to the end of the new buffer,
		'' so that the gap in the middle grows.
		if (tk.size > tk.front) then
			memmove(p + NEWGAP, p + tk.gap, _
			        (tk.size - tk.front) * sizeof(OneToken))
		end if

		tk.gap = NEWGAP
	else
		p = tk.p + tk.front
	end if

	p->id = id
	p->mark = MARK_TOPLEVEL
	if (length > 0) then
		p->text = xallocate(length + 1)
		memcpy(p->text, text, length)
		p->text[length] = 0
	else
		p->text = NULL
	end if

	tk.front += 1
	tk.gap -= 1
	tk.size += 1
end sub

private sub tk_in(byval id as integer, byval text as zstring ptr)
	dim as integer length = any
	if (text) then
		length = len(*text)
	else
		length = 0
	end if
	tk_in_raw(id, text, length)
end sub

sub tk_insert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr _
	)
	tk_move_to(x)
	tk_in(id, text)
end sub

sub tk_insert_space(byval x as integer)
	tk_insert(x, TK_SPACE, " ")
end sub

sub tk_copy _
	( _
		byval x as integer, _
		byval first as integer, _
		byval last as integer _
	)
	for i as integer = 0 to (last - first)
		dim as integer target = x + i
		dim as integer source = first + i
		tk_insert(target, tk_get(source), tk_text(source))
		tk_set_mark(tk_mark(source), target, target)
	next
end sub

'' Delete token in front of current position (backwards deletion)
private sub tk_out()
	if (tk.front < 1) then return
	tk.front -= 1
	tk.gap += 1
	tk.size -= 1
	deallocate(tk.p[tk.front].text)
end sub

sub tk_remove(byval first as integer, byval last as integer)
	tk_move_to(last + 1)
	while (last >= first)
		tk_out()
		last -= 1
	wend
end sub

sub tk_set_mark _
	( _
		byval mark as integer, _
		byval first as integer, _
		byval last as integer _
	)
	for i as integer = first to last
		dim as OneToken ptr p = direct_ptr(i)
		if (p->id <> TK_EOF) then
			p->mark = mark
		end if
	next
end sub

function tk_get(byval x as integer) as integer
	return direct_ptr(x)->id
end function

function tk_text(byval x as integer) as zstring ptr
	return direct_ptr(x)->text
end function

function tk_mark(byval x as integer) as integer
	return direct_ptr(x)->mark
end function

sub tk_init()
	tk.p = NULL
	tk.front = 0
	tk.gap = 0
	tk.size = 0
end sub

sub tk_end()
	for i as integer = 0 to (tk.size - 1)
		deallocate(direct_ptr(i)->text)
	next
	deallocate(tk.p)
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' Lexing

type LexStuff
	as ubyte ptr buffer '' File content buffer
	as ubyte ptr i      '' Current char, will always be <= limit
	as ubyte ptr limit  '' (end of buffer)
	as HashTable kwhash '' C keywords
end type

dim shared as LexStuff lex

private sub load_file(byref filename as string)
	'' Read in the whole file content
	dim as integer f = freefile()
	if (open(filename, for binary, access read, as #f)) then
		oops("couldn't open file: '" & filename & "'")
	end if

	dim as longint filesize = lof(f)
	if (filesize > &h40000000) then
		oops("a header file bigger than 1 GiB? no way...")
	end if

	'' An extra 0 byte at the end of the buffer so we can look ahead
	'' without bound checks, and don't need to give special treatment
	'' to empty files
	dim as integer size = filesize
	lex.buffer = xallocate(size + 1)
	lex.buffer[size] = 0
	lex.i = lex.buffer
	lex.limit = lex.buffer + size

	if (size > 0) then
		dim as integer found = 0
		dim as integer result = get(#f, , *lex.buffer, size, found)
		if (result or (found <> size)) then
			oops("file I/O failed")
		end if
	end if

	close #f
end sub

private sub lex_init(byref filename as string)
	load_file(filename)

	'' Load C keywords if not yet done
	if (lex.kwhash.items = NULL) then
		hash_init(@lex.kwhash, 7)
		for i as integer = KW__C_FIRST to (KW__FB_FIRST - 1)
			hash_add(@lex.kwhash, token_text(i), i)
		next
	end if
end sub

private sub lex_end()
	deallocate(lex.buffer)
end sub

enum
	CH_BELL     = &h07  '' \a
	CH_BKSPC    = &h08  '' \b
	CH_TAB      = &h09  '' \t
	CH_LF       = &h0A  '' \n
	CH_VTAB     = &h0B  '' \v
	CH_FORMFEED = &h0C  '' \f
	CH_CR       = &h0D  '' \r
	CH_ESC      = &h1B

	CH_SPACE    = &h20
	CH_EXCL         '' !
	CH_DQUOTE       '' "
	CH_HASH         '' #
	CH_DOLLAR       '' $
	CH_PERCENT      '' %
	CH_AMP          '' &
	CH_QUOTE        '' '
	CH_LPAREN       '' (
	CH_RPAREN       '' )
	CH_MUL          '' *
	CH_PLUS         '' +
	CH_COMMA        '' ,
	CH_MINUS        '' -
	CH_DOT          '' .
	CH_SLASH        '' /

	CH_0, CH_1, CH_2, CH_3, CH_4, CH_5, CH_6, CH_7, CH_8, CH_9

	CH_COLON        '' :
	CH_SEMI         '' ;
	CH_LT           '' <
	CH_EQ           '' =
	CH_GT           '' >
	CH_QUESTION     '' ?
	CH_AT           '' @

	CH_A, CH_B, CH_C, CH_D, CH_E, CH_F, CH_G
	CH_H, CH_I, CH_J, CH_K, CH_L, CH_M, CH_N, CH_O, CH_P
	CH_Q, CH_R, CH_S, CH_T, CH_U, CH_V, CH_W
	CH_X, CH_Y, CH_Z

	CH_LBRACKET     '' [
	CH_BACKSLASH    '' \
	CH_RBRACKET     '' ]
	CH_CIRCUMFLEX   '' ^
	CH_UNDERSCORE   '' _
	CH_GRAVE        '' `

	CH_L_A, CH_L_B, CH_L_C, CH_L_D, CH_L_E, CH_L_F, CH_L_G
	CH_L_H, CH_L_I, CH_L_J, CH_L_K, CH_L_L, CH_L_M, CH_L_N, CH_L_O, CH_L_P
	CH_L_Q, CH_L_R, CH_L_S, CH_L_T, CH_L_U, CH_L_V, CH_L_W
	CH_L_X, CH_L_Y, CH_L_Z

	CH_LBRACE       '' {
	CH_PIPE         '' |
	CH_RBRACE       '' }
	CH_TILDE        '' ~
end enum

#macro read_bytes(n, id)
	lex.i += n
	tk_in(id, NULL)
#endmacro

private sub read_space()
	dim as ubyte ptr begin = lex.i
	do
		lex.i += 1
	loop while ((lex.i[0] = CH_TAB) or (lex.i[0] = CH_SPACE))
	tk_in_raw(TK_SPACE, begin, culng(lex.i) - culng(begin))
end sub

private sub read_linecomment()
	'' Line comments, starting at the first '/' of '// foo...'
	'' The whole comment body except for the // will be put into the token.
	'' EOL remains a separate token.
	lex.i += 2
	dim as ubyte ptr begin = lex.i

	do
		select case (lex.i[0])
		case CH_LF, CH_CR, 0
			exit do
		end select

		lex.i += 1
	loop

	tk_in_raw(TK_LINECOMMENT, begin, culng(lex.i) - culng(begin))
end sub

private sub read_comment()
	lex.i += 2
	dim as ubyte ptr begin = lex.i

	dim as integer saw_end = FALSE
	do
		select case (lex.i[0])
		case 0
			tk_in(TK_TODO, "comment left open")
			exit do

		case CH_MUL		'' *
			if (lex.i[1] = CH_SLASH) then	'' */
				saw_end = TRUE
				exit do
			end if

		end select

		lex.i += 1
	loop

	tk_in_raw(TK_COMMENT, begin, culng(lex.i) - culng(begin))

	if (saw_end) then
		lex.i += 2
	end if
end sub

private sub read_id()
	'' Identifier/keyword parsing: sequences of a-z, A-Z, 0-9, _, $
	'' The current char is one of those already. The whole identifier
	'' will be stored into a TK_ID, or if it's a keyword the proper KW_*
	'' is used instead of TK_ID and the text is not stored.
	dim as ubyte ptr begin = lex.i

	do
		lex.i += 1

		select case as const (lex.i[0])
		case CH_A   to CH_Z  , _
		     CH_L_A to CH_L_Z, _
		     CH_0   to CH_9  , _
		     CH_UNDERSCORE   , _
		     CH_DOLLAR

		case else
			exit do

		end select
	loop

	'' Is this a keyword? Then use the proper KW_* instead of TK_ID
	dim as integer length = culng(lex.i) - culng(begin)
	dim as HashItem ptr item = _
		hash_lookup(@lex.kwhash, begin, length, _
		            hash_hash(begin, length))
	if (item->s) then
		tk_in_raw(item->data, NULL, 0)
	else
		tk_in_raw(TK_ID, begin, length)
	end if
end sub

private sub read_number()
	'' Number literal parsing starting at '0'-'9' or '.'
	'' These are covered:
	''    123
	''    .123       (float)
	''    123.123    (float)
	''    0xAABBCCDD (hexadecimal)
	''    010        (octal)
	'' There also is some simple float exponent and type suffix parsing.
	'' TODO: hex floats?
	''
	'' In case of oct/hex literals only the important part of the number
	'' literals is stored into the token, so it can easily be translated
	'' to FB, for example:
	''    0xAABBCCDD -> TK_HEXLIT, AABBCCDD -> &hAABBCCDD
	''    010        -> TK_OCTLIT, 10       -> &o10

	dim as integer numbase = 10
	dim as integer id = TK_DECNUM
	if (lex.i[0] = CH_0) then '' 0
		if (lex.i[1] = CH_L_X) then '' 0x
			lex.i += 2
			numbase = 16
			id = TK_HEXNUM
		elseif ((lex.i[1] >= CH_0) and (lex.i[1] <= CH_9)) then
			lex.i += 1
			numbase = 8
			id = TK_OCTNUM
		end if
	end if

	dim as ubyte ptr begin = lex.i
	dim as integer found_dot = FALSE
	do
		dim as integer digit = lex.i[0]

		if (digit = CH_DOT) then
			'' Only one dot allowed
			if (found_dot) then
				exit do
			end if
			found_dot = TRUE
		else
			select case as const (digit)
			case CH_A to CH_F
				digit -= (CH_A - 10)

			case CH_L_A to CH_L_F
				digit -= (CH_L_A - 10)

			case CH_0 to CH_9
				digit -= CH_0

			case else
				exit do
			end select

			'' Do not allow A-F in decimal numbers, etc.
			if (digit >= numbase) then
				exit do
			end if
		end if

		lex.i += 1
	loop

	'' Exponent? (can be used even without fractional part, e.g. '1e1')
	select case (lex.i[0])
	case CH_E, CH_L_E   '' 'E', 'e'
		lex.i += 1

		'' ['+' | '-']
		select case (lex.i[0])
		case CH_PLUS, CH_MINUS
			lex.i += 1
		end select

		'' ['0'-'9']*
		while ((lex.i[0] >= CH_0) and (lex.i[0] <= CH_9))
			lex.i += 1
		wend

	end select

	'' Type suffixes
	if (found_dot) then
		select case (lex.i[0])
		case CH_F, CH_L_F, _    '' 'F' | 'f'
		     CH_D, CH_L_D       '' 'D' | 'd'
			lex.i += 1
		end select
	else
		select case (lex.i[0])
		case CH_U, CH_L_U       '' 'U' | 'u'
			lex.i += 1
		end select

		select case (lex.i[0])
		case CH_L, CH_L_L       '' 'L' | 'l'
			lex.i += 1
			select case (lex.i[0])
			case CH_L, CH_L_L       '' 'L' | 'l'
				lex.i += 1
			end select
		end select
	end if

	tk_in_raw(id, begin, culng(lex.i) - culng(begin))
end sub

enum
	STRFLAG_CHAR = &b001 '' is char literal?
	STRFLAG_WIDE = &b010 '' is wide (wchar) literal?
	STRFLAG_ESC  = &b100 '' contains escape sequences?
end enum

private sub read_string()
	'' String/char literal parsing, starting at ", ', or L, covering:
	''    'a'
	''    "foo"
	''    L"foo"
	'' The string content is stored into the token, but not the quotes.
	'' If escape sequences are found, we'll generate a TK_ESTRING instead
	'' of TK_STRING, so the emitter can prepend an '!', for example:
	''    "\n" -> \n -> !"\n"

	dim as uinteger strflags = 0

	dim as integer id = TK_STRING
	if (lex.i[0] = CH_L) then
		lex.i += 1
		strflags or= STRFLAG_WIDE
	end if

	dim as integer quotechar = lex.i[0]
	if (quotechar = CH_QUOTE) then
		strflags or= STRFLAG_CHAR
	end if

	lex.i += 1
	dim as ubyte ptr begin = lex.i
	dim as integer saw_end = FALSE
	do
		select case (lex.i[0])
		case quotechar
			saw_end = TRUE
			exit do

		case CH_LF, CH_CR, 0
			tk_in(TK_TODO, "string/char literal left open")
			exit do

		case CH_BACKSLASH	'' \
			strflags or= STRFLAG_ESC
			select case (lex.i[0])
			case CH_BACKSLASH, _ '' \\
			     quotechar       '' \" | \'
				lex.i += 1
			end select

		end select

		lex.i += 1
	loop

	if (strflags) then
		if (strflags and STRFLAG_CHAR) then
			tk_in(TK_TODO, "char literal")
		else
			tk_in(TK_TODO, "non-trivial string literal")
		end if
	end if

	tk_in_raw(id, begin, culng(lex.i) - culng(begin))

	if (saw_end) then
		lex.i += 1
	end if
end sub

private sub tokenize_next()
	'' Identify the next token
	select case as const (lex.i[0])
	case CH_CR
		if (lex.i[0] = CH_LF) then	'' CRLF
			lex.i += 1
		end if
		read_bytes(1, TK_EOL)

	case CH_LF
		read_bytes(1, TK_EOL)

	case CH_TAB, CH_SPACE
		read_space()

	case CH_EXCL		'' !
		if (lex.i[1] = CH_EQ) then	'' !=
			read_bytes(2, TK_NE)
		else
			read_bytes(1, TK_LOGNOT)
		end if

	case CH_DQUOTE		'' "
		read_string()

	case CH_HASH		'' #
		if (lex.i[1] = CH_HASH) then	'' ##
			read_bytes(2, TK_MERGE)
		else
			read_bytes(1, TK_HASH)
		end if

	case CH_DOLLAR		'' $
		read_id()

	case CH_PERCENT		'' %
		if (lex.i[1] = CH_EQ) then	'' %=
			read_bytes(2, TK_SELFMOD)
		else
			read_bytes(1, TK_MOD)
		end if

	case CH_AMP		'' &
		select case (lex.i[1])
		case CH_AMP	'' &&
			read_bytes(2, TK_LOGAND)
		case CH_EQ	'' &=
			read_bytes(2, TK_SELFBITAND)
		case else
			read_bytes(1, TK_BITAND)
		end select

	case CH_QUOTE		'' '
		read_string()

	case CH_LPAREN		'' (
		read_bytes(1, TK_LPAREN)

	case CH_RPAREN		'' )
		read_bytes(1, TK_RPAREN)

	case CH_MUL		'' *
		if (lex.i[1] = CH_EQ) then	'' *=
			read_bytes(2, TK_SELFMUL)
		else
			read_bytes(1, TK_MUL)
		end if

	case CH_PLUS		'' +
		select case (lex.i[1])
		case CH_PLUS	'' ++
			read_bytes(2, TK_INCREMENT)
		case CH_EQ	'' +=
			read_bytes(2, TK_SELFADD)
		case else
			read_bytes(1, TK_ADD)
		end select

	case CH_COMMA		'' ,
		read_bytes(1, TK_COMMA)

	case CH_MINUS		'' -
		select case (lex.i[1])
		case CH_GT	'' ->
			read_bytes(2, TK_FIELDDEREF)
		case CH_MINUS	'' --
			read_bytes(2, TK_DECREMENT)
		case CH_EQ	'' -=
			read_bytes(2, TK_SELFSUB)
		case else
			read_bytes(1, TK_SUB)
		end select

	case CH_DOT		'' .
		select case (lex.i[1])
		case CH_0 to CH_9   '' 0-9 (Decimal float beginning with '.')
			read_number()
		case CH_DOT
			if (lex.i[2] = CH_DOT) then	'' ...
				read_bytes(3, TK_ELLIPSIS)
			else
				read_bytes(1, TK_DOT)
			end if
		case else
			read_bytes(1, TK_DOT)
		end select

	case CH_SLASH		'' /
		select case (lex.i[1])
		case CH_EQ	'' /=
			read_bytes(2, TK_SELFDIV)
		case CH_SLASH	'' //
			read_linecomment()
		case CH_MUL	'' /*
			read_comment()
		case else
			read_bytes(1, TK_DIV)
		end select

	case CH_0 to CH_9	'' 0 - 9
		read_number()

	case CH_COLON		'' :
		read_bytes(1, TK_COLON)

	case CH_SEMI		'' ;
		read_bytes(1, TK_SEMI)

	case CH_LT		'' <
		select case (lex.i[1])
		case CH_LT	'' <<
			if (lex.i[2] = CH_EQ) then	'' <<=
				read_bytes(3, TK_SELFSHL)
			else
				read_bytes(2, TK_SHL)
			end if
		case CH_EQ	'' <=
			read_bytes(2, TK_LE)
		case else
			read_bytes(1, TK_LT)
		end select

	case CH_EQ		'' =
		if (lex.i[1] = CH_EQ) then	'' ==
			read_bytes(2, TK_EQ)
		else
			read_bytes(1, TK_ASSIGN)
		end if

	case CH_GT		'' >
		select case (lex.i[1])
		case CH_GT	'' >>
			if (lex.i[2] = CH_EQ) then	'' >>=
				read_bytes(3, TK_SELFSHR)
			else
				read_bytes(2, TK_SHR)
			end if
		case CH_EQ	'' >=
			read_bytes(2, TK_GE)
		case else
			read_bytes(1, TK_GT)
		end select

	case CH_QUESTION	'' ?
		read_bytes(1, TK_QUESTION)

	case CH_A       to (CH_L - 1), _	'' A-Z except L
	     (CH_L + 1) to CH_Z
		read_id()

	case CH_L		'' L
		if (lex.i[1] = CH_DQUOTE) then
			read_string()
		else
			read_id()
		end if

	case CH_LBRACKET	'' [
		read_bytes(1, TK_LBRACKET)

	case CH_BACKSLASH	'' \
		read_bytes(1, TK_BACKSLASH)

	case CH_RBRACKET	'' ]
		read_bytes(1, TK_RBRACKET)

	case CH_CIRCUMFLEX	'' ^
		if (lex.i[1] = CH_EQ) then	'' ^=
			read_bytes(2, TK_SELFBITXOR)
		else
			read_bytes(1, TK_BITXOR)
		end if

	case CH_UNDERSCORE	'' _
		read_id()

	case CH_L_A to CH_L_Z	'' a-z
		read_id()

	case CH_LBRACE		'' {
		read_bytes(1, TK_LBRACE)

	case CH_PIPE		'' |
		select case (lex.i[1])
		case CH_PIPE	'' ||
			read_bytes(2, TK_LOGOR)
		case CH_EQ	'' |=
			read_bytes(2, TK_SELFBITOR)
		case else
			read_bytes(1, TK_BITOR)
		end select

	case CH_RBRACE		'' }
		read_bytes(1, TK_RBRACE)

	case CH_TILDE		'' ~
		read_bytes(1, TK_BITNOT)

	case else
		tk_in(TK_TODO, "unexpected character: &h" + hex(lex.i[0], 2))
		tk_in_raw(TK_BYTE, lex.i, 1)
		lex.i += 1

	end select
end sub

private sub complain_about_nulls()
	while (lex.i < lex.limit)
		if (lex.i[0] = 0) then
			oops("file has embedded nulls, please fix that first!")
		end if
		lex.i += 1
	wend
	lex.i = lex.buffer
end sub

sub tk_insert_file(byval x as integer, byref filename as string)
	tk_move_to(x)

	lex_init(filename)

	'' Currently tokens store text as null-terminated strings, so they
	'' can't allow embedded nulls.
	complain_about_nulls()

	while (lex.i < lex.limit)
		tokenize_next()
	wend

	lex_end()
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' Emitting

type EmitterStuff
	as integer fo '' Output file
end type

dim shared as EmitterStuff emitter

private sub emit(byval text as zstring ptr)
	dim as integer length = len(*text)
	if (put(#emitter.fo, , *cptr(ubyte ptr, text), length)) then
		oops("file I/O failed")
	end if
end sub

private sub emit_token(byval x as integer)
	select case as const (tk_get(x))
	case TK_EOL
		#if defined(__FB_WIN32__) or defined(__FB_DOS__)
			emit(!"\r\n")
		#else
			emit(!"\n")
		#endif

	case TK_TODO
		dim as zstring ptr text = tk_text(x)
		if (text) then
			emit("/' TODO: ")
			emit(text)
			emit(" '/")
		else
			emit("/' TODO '/")
		end if

	case TK_COMMENT
		emit("/'")
		emit(tk_text(x))
		emit("'/")

	case TK_LINECOMMENT
		emit("''")
		emit(tk_text(x))

	case TK_DECNUM
		emit(tk_text(x))

	case TK_HEXNUM
		emit("&h")
		emit(tk_text(x))

	case TK_OCTNUM
		emit("&o")
		emit(tk_text(x))

	case TK_STRING
		emit("""")
		emit(tk_text(x))
		emit("""")

	case TK_WSTRING
		emit("wstr(""")
		emit(tk_text(x))
		emit(""")")

	case TK_ESTRING
		emit("!""")
		emit(tk_text(x))
		emit("""")

	case TK_EWSTRING
		emit("wstr(!""")
		emit(tk_text(x))
		emit(""")")

	case else
		dim as zstring ptr text = tk_text(x)
		if (text) then
			emit(text)
		else
			text = token_text(tk_get(x))
			if (text) then
				emit(text)
			else
				emit("/' TODO: token " & tk_get(x) & " '/")
			end if
		end if

	end select
end sub

sub tk_emit_file(byref filename as string)
	emitter.fo = freefile()
	if (open(filename, for binary, access write, as #emitter.fo)) then
		oops("could not open output file: '" & filename & "'")
	end if

	dim as integer x = 0
	while (tk_get(x) <> TK_EOF)
		emit_token(x)
		x += 1
	wend

	close #emitter.fo
	emitter.fo = 0
end sub

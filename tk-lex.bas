#include once "tk.bi"
#include once "common.bi"
#include once "hash.bi"

'' Same order as in TK_* enum
dim shared as zstring ptr c_keywords(0 to (KW__C_COUNT - 1)) = _
{ _
	@"_Bool"     , _
	@"_Complex"  , _
	@"_Imaginary", _
	@"auto"      , _
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
	@"while"       _
}

type LexStuff
	as ubyte ptr buffer '' File content buffer
	as ubyte ptr i      '' Current char, will always be <= limit
	as ubyte ptr limit  '' (end of buffer)

	as HashTable kwhash '' C keywords

	as integer x        '' Current token index
end type

dim shared as LexStuff lex

private sub load_file(byref filename as string)
	'' Read in the whole file content
	dim as integer f = freefile()
	if (open(filename, for binary, access read, as #f)) then
		xoops("couldn't open file: '" & filename & "'")
	end if

	dim as longint filesize = lof(f)
	if (filesize > &h40000000) then
		xoops("a header file  bigger than 1 GiB? no way...")
	end if

	tk_count_input_size(filesize)

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
			xoops("file I/O failed")
		end if
	end if

	close #f
end sub

private sub lex_init(byval x as integer, byref filename as string)
	lex.x = x
	load_file(filename)

	'' Load C keywords if not yet done
	if (lex.kwhash.items = NULL) then
		hash_init(@lex.kwhash, 7)
		for i as integer = 0 to (KW__C_COUNT - 1)
			hash_add(@lex.kwhash, c_keywords(i), i + KW__C_FIRST)
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
	CH_QUEST        '' ?
	CH_AT           '' @

	CH_A, CH_B, CH_C, CH_D, CH_E, CH_F, CH_G
	CH_H, CH_I, CH_J, CH_K, CH_L, CH_M, CH_N, CH_O, CH_P
	CH_Q, CH_R, CH_S, CH_T, CH_U, CH_V, CH_W
	CH_X, CH_Y, CH_Z

	CH_LBRACKET     '' [
	CH_BACKSLASH    '' \
	CH_RBRACKET     '' ]
	CH_CIRCUMFLEX   '' ^
	CH_UNDERLINE    '' _
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

private sub add_tk(byval id as integer, byval text as zstring ptr)
	tk_insert(lex.x, id, text)
	lex.x += 1
end sub

private sub add_tk_raw _
	( _
		byval id as integer, _
		byval text as ubyte ptr, _
		byval length as integer _
	)
	tk_insert_raw(lex.x, id, text, length)
	lex.x += 1
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
		     CH_UNDERLINE    , _
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
		add_tk_raw(item->data, NULL, 0)
	else
		add_tk_raw(TK_ID, begin, length)
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
		lex.i += 1
		if (lex.i[0] = CH_L_X) then '' 0x
			lex.i += 1
			numbase = 16
			id = TK_HEXNUM
		else
			numbase = 8
			id = TK_OCTNUM
		end if
	end if

	dim as ubyte ptr begin = lex.i
	dim as integer found_dot = FALSE
	do
		dim as integer digit = lex.i[0]

		select case as const (digit)
		case CH_A to CH_F
			digit -= (CH_A - 10)

		case CH_L_A to CH_L_F
			digit -= (CH_L_A - 10)

		case CH_0 to CH_9
			digit -= CH_0

		case CH_DOT
			'' Only one dot allowed
			if (found_dot) then
				exit do
			end if

			found_dot = TRUE

		case else
			exit do
		end select

		'' Do not allow A-F in decimal numbers, etc.
		if (digit >= numbase) then
			exit do
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

	add_tk_raw(id, begin, culng(lex.i) - culng(begin))
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

	do
		select case (lex.i[0])
		case quotechar
			lex.i += 1
			exit do

		case CH_LF, CH_CR, 0
			add_tk(TK_TODO, "string/char literal left open")
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
			add_tk(TK_TODO, "char literal")
		else
			add_tk(TK_TODO, "non-trivial string literal")
		end if
	end if

	add_tk_raw(id, begin, culng(lex.i) - culng(begin))
end sub

#macro read_bytes(n, id)
	lex.i += n
	add_tk(id, NULL)
#endmacro

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

	add_tk_raw(TK_LINECOMMENT, begin, culng(lex.i) - culng(begin))
end sub

private sub read_blockcomment()
	lex.i += 2
	dim as ubyte ptr begin = lex.i

	dim as integer saw_end = FALSE
	do
		select case (lex.i[0])
		case 0
			add_tk(TK_TODO, "block comment left open")
			exit do

		case CH_MUL		'' *
			if (lex.i[1] = CH_SLASH) then	'' */
				saw_end = TRUE
				exit do
			end if

		end select

		lex.i += 1
	loop

	add_tk_raw(TK_BLOCKCOMMENT, begin, culng(lex.i) - culng(begin))

	if (saw_end) then
		lex.i += 2
	end if
end sub

private sub tokenize_next()
	'' Skip spaces in front of next token
	while ((lex.i[0] = CH_TAB) or (lex.i[0] = CH_SPACE))
		lex.i += 1
	wend

	'' Identify the next token
	select case as const (lex.i[0])
	case CH_CR
		if (lex.i[0] = CH_LF) then	'' CRLF
			lex.i += 1
		end if
		read_bytes(1, TK_EOL)

	case CH_LF
		read_bytes(1, TK_EOL)

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
			read_blockcomment()
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

	case CH_QUEST		'' ?
		read_bytes(1, TK_QUEST)

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

	case CH_UNDERLINE	'' _
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
		add_tk(TK_TODO, "unexpected character: &h" + hex(lex.i[0], 2))
		add_tk_raw(TK_BYTE, lex.i, 1)
		lex.i += 1

	end select
end sub

private sub complain_about_nulls()
	while (lex.i < lex.limit)
		if (lex.i[0] = 0) then
			xoops("file has embedded NULLs, please fix that first!")
		end if
		lex.i += 1
	wend
	lex.i = lex.buffer
end sub

sub tk_insert_file(byval x as integer, byref filename as string)
	lex_init(x, filename)

	'' Currently tokens store text as NULL-terminated strings, so they
	'' can't allow embedded NULLs.
	complain_about_nulls()

	while (lex.i < lex.limit)
		tokenize_next()
		tk_count_input_token()
	wend

	lex_end()
end sub

'' C lexer

#include once "fbfrog.bi"

type LexStuff
	as ubyte ptr buffer '' File content buffer
	as ubyte ptr i      '' Current char, will always be <= limit
	as ubyte ptr limit  '' (end of buffer)
end type

dim shared as LexStuff lex

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
	CH_STAR         '' *
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

private sub add_text_token_raw _
	( _
		byval tk as integer, _
		byval text as ubyte ptr, _
		byval length as integer _
	)

	'' Just store the token text. If this text string is already stored,
	'' we'll get the existing pointer, otherwise our text will be copied,
	'' a null terminator will be added, and we get that new string.
	'' Note: This can reuse even keyword text, e.g. for string literals
	'' like "int".
	dim as integer dat = -1
	text = storage_store(text, length, @dat)

	'' Is this a keyword? Then use the proper KW_* instead of TK_ID
	if ((tk = TK_ID) and (dat >= 0)) then
		tk_raw_insert(dat, NULL)
	else
		tk_raw_insert(tk, text)
	end if
end sub

private sub add_text_token(byval tk as integer, byval begin as ubyte ptr)
	add_text_token_raw(tk, begin, culng(lex.i) - culng(begin))
end sub

private sub add_todo(byval text as zstring ptr)
	add_text_token_raw(TK_TODO, text, len(*text))
end sub

private sub add_plain_token(byval tk as integer)
	tk_raw_insert(tk, NULL)
end sub

#macro read_bytes(n, id)
	lex.i += n
	add_plain_token(id)
#endmacro

private sub read_space()
	dim as ubyte ptr begin = lex.i

	do
		lex.i += 1
	loop while ((lex.i[0] = CH_TAB) or (lex.i[0] = CH_SPACE))

	add_text_token(TK_SPACE, begin)
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

	add_text_token(TK_LINECOMMENT, begin)
end sub

private sub read_comment()
	lex.i += 2
	dim as ubyte ptr begin = lex.i

	dim as integer saw_end = FALSE
	do
		select case (lex.i[0])
		case 0
			add_todo("comment left open")
			exit do

		case CH_STAR		'' *
			if (lex.i[1] = CH_SLASH) then	'' */
				saw_end = TRUE
				exit do
			end if

		end select

		lex.i += 1
	loop

	add_text_token(TK_COMMENT, begin)

	if (saw_end) then
		lex.i += 2
	end if
end sub

private sub read_id()
	'' Identifier/keyword parsing: sequences of a-z, A-Z, 0-9, _
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
		     CH_UNDERSCORE

		case else
			exit do

		end select
	loop

	add_text_token(TK_ID, begin)
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

	add_text_token(id, begin)
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
			add_todo("string/char literal left open")
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
			add_todo("char literal")
		else
			add_todo("non-trivial string literal")
		end if
	end if

	add_text_token(id, begin)

	if (saw_end) then
		lex.i += 1
	end if
end sub

private sub lex_next()
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
			read_bytes(2, TK_EXCLEQ)
		else
			read_bytes(1, TK_EXCL)
		end if

	case CH_DQUOTE		'' "
		read_string()

	case CH_HASH		'' #
		if (lex.i[1] = CH_HASH) then	'' ##
			read_bytes(2, TK_HASHHASH)
		else
			read_bytes(1, TK_HASH)
		end if

	case CH_PERCENT		'' %
		if (lex.i[1] = CH_EQ) then	'' %=
			read_bytes(2, TK_PERCENTEQ)
		else
			read_bytes(1, TK_PERCENT)
		end if

	case CH_AMP		'' &
		select case (lex.i[1])
		case CH_AMP	'' &&
			read_bytes(2, TK_AMPAMP)
		case CH_EQ	'' &=
			read_bytes(2, TK_AMPEQ)
		case else
			read_bytes(1, TK_AMP)
		end select

	case CH_QUOTE		'' '
		read_string()

	case CH_LPAREN		'' (
		read_bytes(1, TK_LPAREN)

	case CH_RPAREN		'' )
		read_bytes(1, TK_RPAREN)

	case CH_STAR		'' *
		if (lex.i[1] = CH_EQ) then	'' *=
			read_bytes(2, TK_STAREQ)
		else
			read_bytes(1, TK_STAR)
		end if

	case CH_PLUS		'' +
		select case (lex.i[1])
		case CH_PLUS	'' ++
			read_bytes(2, TK_PLUSPLUS)
		case CH_EQ	'' +=
			read_bytes(2, TK_PLUSEQ)
		case else
			read_bytes(1, TK_PLUS)
		end select

	case CH_COMMA		'' ,
		read_bytes(1, TK_COMMA)

	case CH_MINUS		'' -
		select case (lex.i[1])
		case CH_GT	'' ->
			read_bytes(2, TK_ARROW)
		case CH_MINUS	'' --
			read_bytes(2, TK_MINUSMINUS)
		case CH_EQ	'' -=
			read_bytes(2, TK_MINUSEQ)
		case else
			read_bytes(1, TK_MINUS)
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
			read_bytes(2, TK_SLASHEQ)
		case CH_SLASH	'' //
			read_linecomment()
		case CH_STAR	'' /*
			read_comment()
		case else
			read_bytes(1, TK_SLASH)
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
				read_bytes(3, TK_LTLTEQ)
			else
				read_bytes(2, TK_LTLT)
			end if
		case CH_EQ	'' <=
			read_bytes(2, TK_LTEQ)
		case else
			read_bytes(1, TK_LT)
		end select

	case CH_EQ		'' =
		if (lex.i[1] = CH_EQ) then	'' ==
			read_bytes(2, TK_EQEQ)
		else
			read_bytes(1, TK_EQ)
		end if

	case CH_GT		'' >
		select case (lex.i[1])
		case CH_GT	'' >>
			if (lex.i[2] = CH_EQ) then	'' >>=
				read_bytes(3, TK_GTGTEQ)
			else
				read_bytes(2, TK_GTGT)
			end if
		case CH_EQ	'' >=
			read_bytes(2, TK_GTEQ)
		case else
			read_bytes(1, TK_GT)
		end select

	case CH_QUEST	'' ?
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
			read_bytes(2, TK_CIRCUMFLEXEQ)
		else
			read_bytes(1, TK_CIRCUMFLEX)
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
			read_bytes(2, TK_PIPEPIPE)
		case CH_EQ	'' |=
			read_bytes(2, TK_PIPEEQ)
		case else
			read_bytes(1, TK_PIPE)
		end select

	case CH_RBRACE		'' }
		read_bytes(1, TK_RBRACE)

	case CH_TILDE		'' ~
		read_bytes(1, TK_TILDE)

	case else
		add_todo("unexpected character")
		lex.i += 1
		add_text_token(TK_BYTE, lex.i - 1)

	end select
end sub

private sub load_file(byref filename as string)
	'' Read in the whole file content into lex.buffer
	dim as integer f = freefile()
	if (open(filename, for binary, access read, as #f)) then
		oops("could not open file: '" & filename & "'")
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

private sub complain_about_embedded_nulls()
	'' Currently tokens store text as null-terminated strings, so they
	'' can't allow embedded nulls, and null also indicates EOF to the lexer.
	while (lex.i < lex.limit)
		if (lex.i[0] = 0) then
			oops("file has embedded nulls, please fix that first!")
		end if
		lex.i += 1
	wend
	lex.i = lex.buffer
end sub

private sub init_keywords()
	'' Load C keywords if not yet done
	static as integer lazy = FALSE

	if (lazy) then
		return
	end if

	lazy = TRUE

	for i as integer = KW__C_FIRST to (KW__FB_FIRST - 1)
		dim as zstring ptr s = token_text(i)
		dim as integer length = len(*s)

		'' Note: passing @i here. If the keyword would already be
		'' stored, then this would overwrite i with the hash item data.
		'' However since this is only done once, the keyword won't
		'' exist yet, and storage_store() will only read from i.
		storage_store(s, length, @i)
	next
end sub

private sub lex_init(byref filename as string)
	load_file(filename)
	complain_about_embedded_nulls()
	init_keywords()
end sub

private sub lex_end()
	deallocate(lex.buffer)
end sub

function lex_insert_file _
	( _
		byval x as integer, _
		byref filename as string _
	) as integer

	lex_init(filename)

	dim as integer oldcount = tk_count()

	'' Tokenize and insert into tk buffer
	tk_raw_move_to(x)
	while (lex.i < lex.limit)
		lex_next()
	wend

	dim as integer newtokens = tk_count() - oldcount

	if (frog.verbose) then
		print using "  lexer: read in & bytes, produced & tokens"; _
			(culng(lex.limit) - culng(lex.buffer)); _
			newtokens
	end if

	lex_end()

	return x + newtokens
end function

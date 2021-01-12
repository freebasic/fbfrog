#include once "c-lex.bi"
#include once "chars.bi"

using tktokens

constructor CLexer()
	initKeywords(KW__C_FIRST, KW__C_LAST)
end constructor

sub CLexer.addTextToken(byval t as integer, byval begin as ubyte ptr)
	'' Insert a null terminator temporarily
	var old = i[0]
	i[0] = 0

	if t = TK_ID then
		'' If it's a C keyword, insert the corresponding KW_* (without
		'' storing any string data). Otherwise, if it's a random symbol,
		'' just insert a TK_ID and store the string data on it.
		t = lookupKeyword(begin, TK_ID)
		if t <> TK_ID then
			begin = NULL
		end if
	end if

	tk->insert(x, t, begin)
	setLocation()

	i[0] = old
end sub

sub CLexer.readBytes(byval t as integer, byval length as integer)
	i += length
	tk->insert(x, t)
	setLocation()
end sub

'' // C++ comment
'' The EOL behind the comment is not skipped; it's a (separate) token.
'' The comment may contain escaped newlines ('\' [Spaces] EOL)
'' which means the comment continues on the next line.
sub CLexer.skipLineComment()
	i += 2
	var escaped = FALSE

	do
		select case i[0]
		case 0
			exit do

		case CH_CR
			if escaped = FALSE then
				exit do
			end if

			if i[1] = CH_LF then	'' CRLF
				i += 1
			end if
			i += 1
			newLine()

		case CH_LF
			if escaped = FALSE then
				exit do
			end if
			i += 1
			newLine()

		case CH_SPACE, CH_TAB
			'' Spaces don't change escaped status
			'' (at least gcc/clang support spaces between \ and EOL)
			i += 1

		case CH_BACKSLASH
			escaped = TRUE
			i += 1

		case else
			escaped = FALSE
			i += 1
		end select
	loop
end sub

'' /* C comment */
sub CLexer.skipComment()
	'' /*
	i += 2

	do
		select case i[0]
		case 0
			showErrorAndAbort("comment left open")

		case CH_STAR		'' *
			if i[1] = CH_SLASH then	'' */
				exit do
			end if
			i += 1

		case CH_CR
			if i[1] = CH_LF then	'' CRLF
				i += 1
			end if
			i += 1
			newLine()

		case CH_LF
			i += 1
			newLine()

		case else
			i += 1
		end select
	loop

	'' */
	i += 2
end sub

'' Identifier/keyword lexing: sequences of A-Za-z0-9_
'' Starting out at A-Za-z_
'' Resulting in a TK_ID or KW_* if it's a keyword.
sub CLexer.readId()
	var begin = i
	do
		i += 1
		select case as const i[0]
		case CH_A   to CH_Z  , _
		     CH_L_A to CH_L_Z, _
		     CH_0   to CH_9  , _
		     CH_UNDERSCORE
		case else
			exit do
		end select
	loop
	addTextToken(TK_ID, begin)
end sub

'' Number literal lexing: sequences of A-Za-z0-9_. and +- if preceded by eE.
'' Here in the lexer we don't validate the syntax. Tokens such as '08' or
'' '123foobar' are allowed here even though they're invalid number literals,
'' because they can be used with the ## operator. The syntax checks are done
'' later in hNumberLiteral() when the parsers encounter a TK_NUMBER.
sub CLexer.readNumber()
	var begin = i

	do
		i += 1

		select case as const i[0]
		case CH_A to CH_Z, CH_L_A to CH_L_Z, CH_0 to CH_9, CH_UNDERSCORE, CH_DOT

		'' +|- is only allowed if preceded by e|E (in other cases, +|-
		'' starts a new token separate from the number literal)
		case CH_PLUS, CH_MINUS
			assert(begin < i)
			select case i[-1]
			case CH_E, CH_L_E
			case else
				exit do
			end select

		case else
			exit do
		end select
	loop

	addTextToken(TK_NUMBER, begin)
end sub

'' Look-ahead and check whether the \ is an escaped EOL, and if yes, skip it
function CLexer.skipEscapedEol() as integer
	var j = 0
	assert(i[0] = CH_BACKSLASH)
	j += 1

	'' Skip space between \ and EOL
	while (i[j] = CH_TAB) or (i[j] = CH_SPACE)
		j += 1
	wend

	select case i[j]
	case CH_CR
		j += 1
		if i[j] = CH_LF then	'' CRLF
			j += 1
		end if
	case CH_LF
		j += 1
	case else
		return FALSE
	end select

	i += j
	newLine()
	function = TRUE
end function

'' String/char literal lexing, starting at ", ', or L.
'' String literals may contain escaped EOLs and continue on the next line.
'' For correct lexing, some escape sequences have to be handled here too.
'' The entire string literal is stored into the token as-is, except for escaped
'' EOLs. The real parsing/evaluation is done later by hStringLiteral().
sub CLexer.readString()
	'' Collect the string literal into lex.text.
	'' We can't just read it from the file buffer because we may solve out
	'' escaped EOLs.
	var j = 0  '' current write position in lex.text

	'' L => wstring, else zstring
	var id = TK_STRING
	if i[0] = CH_L then
		id = TK_WSTRING
		text[j] = CH_L : j += 1
		i += 1
	end if

	'' String or char?
	var quotechar = i[0]
	if quotechar = CH_QUOTE then
		id = iif((id = TK_WSTRING), TK_WCHAR, TK_CHAR)
	end if
	text[j] = quotechar : j += 1
	i += 1

	do
		select case i[0]
		case quotechar
			exit do

		case CH_LF, CH_CR, 0
			showErrorAndAbort("string/char literal left open")

		case CH_BACKSLASH	'' \
			if skipEscapedEol() = FALSE then
				'' Store backslash as-is, not resolving escape sequences
				text[j] = CH_BACKSLASH : j += 1
				i += 1

				select case i[0]
				case CH_BACKSLASH '' \\
					text[j] = CH_BACKSLASH : j += 1
					i += 1
				case quotechar    '' \" or \'
					text[j] = quotechar : j += 1
					i += 1
				end select
			end if

		case else
			text[j] = i[0] : j += 1
			i += 1
		end select

		if j > MAXTEXTLEN then
			showErrorAndAbort("string literal too long, MAXTEXTLEN=" & MAXTEXTLEN)
		end if
	loop

	'' closing quote
	text[j] = i[0] : j += 1
	i += 1

	'' null-terminator
	text[j] = 0

	tk->insert(x, id, text)
	setLocation()
end sub

sub CLexer.tokenize()
	'' Identify the next token
	select case as const i[0]
	case CH_TAB, CH_SPACE
		i += 1
		behindspace = TRUE

	case CH_CR
		if i[1] = CH_LF then	'' CRLF
			i += 1
		end if
		readBytes(TK_EOL, 1)
		newLine()

	case CH_LF
		readBytes(TK_EOL, 1)
		newLine()

	case CH_FORMFEED
		i += 1

	case CH_EXCL		'' !
		if i[1] = CH_EQ then	'' !=
			readBytes(TK_EXCLEQ, 2)
		else
			readBytes(TK_EXCL, 1)
		end if

	case CH_DQUOTE		'' "
		readString()

	case CH_HASH		'' #
		if i[1] = CH_HASH then	'' ##
			readBytes(TK_HASHHASH, 2)
		else
			readBytes(TK_HASH, 1)
		end if

	case CH_PERCENT		'' %
		if i[1] = CH_EQ then	'' %=
			readBytes(TK_PERCENTEQ, 2)
		else
			readBytes(TK_PERCENT, 1)
		end if

	case CH_AMP		'' &
		select case i[1]
		case CH_AMP	'' &&
			readBytes(TK_AMPAMP, 2)
		case CH_EQ	'' &=
			readBytes(TK_AMPEQ, 2)
		case else
			readBytes(TK_AMP, 1)
		end select

	case CH_QUOTE		'' '
		readString()

	case CH_LPAREN		'' (
		readBytes(TK_LPAREN, 1)

	case CH_RPAREN		'')
		readBytes(TK_RPAREN, 1)

	case CH_STAR		'' *
		if i[1] = CH_EQ then	'' *=
			readBytes(TK_STAREQ, 2)
		else
			readBytes(TK_STAR, 1)
		end if

	case CH_PLUS		'' +
		select case i[1]
		case CH_PLUS	'' ++
			readBytes(TK_PLUSPLUS, 2)
		case CH_EQ	'' +=
			readBytes(TK_PLUSEQ, 2)
		case else
			readBytes(TK_PLUS, 1)
		end select

	case CH_COMMA		'' ,
		readBytes(TK_COMMA, 1)

	case CH_MINUS		'' -
		select case i[1]
		case CH_GT	'' ->
			readBytes(TK_ARROW, 2)
		case CH_MINUS	'' --
			readBytes(TK_MINUSMINUS, 2)
		case CH_EQ	'' -=
			readBytes(TK_MINUSEQ, 2)
		case else
			readBytes(TK_MINUS, 1)
		end select

	case CH_DOT		'' .
		select case i[1]
		case CH_0 to CH_9   '' 0-9 (Decimal float beginning with '.')
			readNumber()
		case CH_DOT
			if i[2] = CH_DOT then	'' ...
				readBytes(TK_ELLIPSIS, 3)
			else
				readBytes(TK_DOT, 1)
			end if
		case else
			readBytes(TK_DOT, 1)
		end select

	case CH_SLASH		'' /
		select case i[1]
		case CH_EQ	'' /=
			readBytes(TK_SLASHEQ, 2)
		case CH_SLASH	'' //
			skipLineComment()
			behindspace = TRUE
		case CH_STAR	'' /*
			skipComment()
			behindspace = TRUE
		case else
			readBytes(TK_SLASH, 1)
		end select

	case CH_0 to CH_9	'' 0 - 9
		readNumber()

	case CH_COLON		'' :
		readBytes(TK_COLON, 1)

	case CH_SEMI		'' ;
		readBytes(TK_SEMI, 1)

	case CH_LT		'' <
		select case i[1]
		case CH_LT	'' <<
			if i[2] = CH_EQ then	'' <<=
				readBytes(TK_LTLTEQ, 3)
			else
				readBytes(TK_LTLT, 2)
			end if
		case CH_EQ	'' <=
			readBytes(TK_LTEQ, 2)
		case else
			readBytes(TK_LT, 1)
		end select

	case CH_EQ		'' =
		if i[1] = CH_EQ then	'' ==
			readBytes(TK_EQEQ, 2)
		else
			readBytes(TK_EQ, 1)
		end if

	case CH_GT		'' >
		select case i[1]
		case CH_GT	'' >>
			if i[2] = CH_EQ then	'' >>=
				readBytes(TK_GTGTEQ, 3)
			else
				readBytes(TK_GTGT, 2)
			end if
		case CH_EQ	'' >=
			readBytes(TK_GTEQ, 2)
		case else
			readBytes(TK_GT, 1)
		end select

	case CH_QUEST	'' ?
		readBytes(TK_QUEST, 1)

	case CH_A       to (CH_L - 1), _	'' A-Z except L
	     (CH_L + 1) to CH_Z
		readId()

	case CH_L		'' L
		select case i[1]
		case CH_QUOTE, CH_DQUOTE	'' ', "
			readString()
		case else
			readId()
		end select

	case CH_LBRACKET	'' [
		readBytes(TK_LBRACKET, 1)

	case CH_BACKSLASH	'' \
		'' Check for escaped EOLs and solve them out
		if skipEscapedEol() then
			behindspace = TRUE
		else
			i += 1
			addTextToken(TK_STRAYBYTE, i - 1)
		end if

	case CH_RBRACKET	'' ]
		readBytes(TK_RBRACKET, 1)

	case CH_CIRC		'' ^
		if i[1] = CH_EQ then	'' ^=
			readBytes(TK_CIRCEQ, 2)
		else
			readBytes(TK_CIRC, 1)
		end if

	case CH_L_A to CH_L_Z, _	'' a-z
	     CH_UNDERSCORE		'' _
		readId()

	case CH_LBRACE		'' {
		readBytes(TK_LBRACE, 1)

	case CH_PIPE		'' |
		select case i[1]
		case CH_PIPE	'' ||
			readBytes(TK_PIPEPIPE, 2)
		case CH_EQ	'' |=
			readBytes(TK_PIPEEQ, 2)
		case else
			readBytes(TK_PIPE, 1)
		end select

	case CH_RBRACE		'' }
		readBytes(TK_RBRACE, 1)

	case CH_TILDE		'' ~
		readBytes(TK_TILDE, 1)

	case else
		i += 1
		addTextToken(TK_STRAYBYTE, i - 1)
	end select
end sub

''
'' C lexer entry point
''
function lexLoadC(byref sourcectx as SourceContext, byref tk as TokenBuffer, byval x as integer, byval source as SourceInfo ptr) as integer
	assert(source->text)

	dim clex as CLexer
	clex.sourcectx = @sourcectx
	clex.tk = @tk
	clex.x = x
	clex.location.source = source
	clex.location.linenum = 1
	clex.i = source->text

	'' Tokenize and insert into tk buffer
	'' Loop until EOF...
	do
		'' Loop until the next token was added...
		'' (loop while skipping white-space)
		clex.behindspace = FALSE
		x = clex.x
		do
			if clex.i[0] = 0 then exit do, do
			clex.tokenize()
		loop while clex.x = x
	loop

	function = clex.x
end function

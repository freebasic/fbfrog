''
'' C source code lexer, command line argument lexer
''

#include once "lex.bi"

#include once "tk.bi"
#include once "util-hash.bi"
#include once "util-str.bi"

using tktokens

const MAXTEXTLEN = 1 shl 12

type LexStuff
	i		as ubyte ptr  '' Current char, will always be <= limit
	x		as integer
	location	as TkLocation
	behindspace	as integer

	ckeywords	as THash = THash(12)
	frogoptions	as THash = THash(12)

	'' +2 extra room to allow for some "overflowing" to reduce the amount
	'' of checking needed, +1 for null terminator.
	text		as zstring * MAXTEXTLEN+2+1

	declare constructor()
	declare operator let(byref as const LexStuff) '' declared but not implemented
end type

dim shared lex as LexStuff

private sub hInitKeywords(byref h as THash, byval first as integer, byval last as integer)
	for i as integer = first to last
		h.addOverwrite(tkInfoText(i), cast(any ptr, i))
	next
end sub

constructor LexStuff()
	hInitKeywords(lex.ckeywords, KW__C_FIRST, KW__C_LAST)
	hInitKeywords(lex.frogoptions, OPT__FIRST, OPT__LAST)
end constructor

private function hLookupKeyword(byref h as THash, byval id as zstring ptr, byval defaulttk as integer) as integer
	var item = h.lookup(id, hashHash(id))
	if item->s then
		'' Return the corresponding KW_* (C keyword) or OPT_* (command line option)
		function = cint(item->data)
	else
		function = defaulttk
	end if
end function

private sub lexOops(byref message as string)
	oopsLocation(lex.location, message)
end sub

private sub hSetLocation(byval flags as integer = 0)
	tkSetLocation(lex.x, lex.location)
	if lex.behindspace then
		flags or= TKFLAG_BEHINDSPACE
	end if
	tkAddFlags(lex.x, lex.x, flags)
	lex.x += 1
end sub

private sub hAddTextToken(byval tk as integer, byval begin as ubyte ptr)
	'' Insert a null terminator temporarily
	var old = lex.i[0]
	lex.i[0] = 0

	if tk = TK_ID then
		'' If it's a C keyword, insert the corresponding KW_* (without
		'' storing any string data). Otherwise, if it's a random symbol,
		'' just insert a TK_ID and store the string data on it.
		tk = hLookupKeyword(lex.ckeywords, begin, TK_ID)
		if tk <> TK_ID then
			begin = NULL
		end if
	end if

	tkInsert(lex.x, tk, begin)
	hSetLocation()

	lex.i[0] = old
end sub

private sub hReadBytes(byval tk as integer, byval length as integer)
	lex.i += length
	tkInsert(lex.x, tk)
	hSetLocation()
end sub

private sub hNewLine()
	lex.location.linenum += 1
end sub

'' // C++ comment
'' The EOL behind the comment is not skipped; it's a (separate) token.
'' The comment may contain escaped newlines ('\' [Spaces] EOL)
'' which means the comment continues on the next line.
private sub hSkipLineComment()
	lex.i += 2
	var escaped = FALSE

	do
		select case lex.i[0]
		case 0
			exit do

		case CH_CR
			if escaped = FALSE then
				exit do
			end if

			if lex.i[1] = CH_LF then	'' CRLF
				lex.i += 1
			end if
			lex.i += 1
			hNewLine()

		case CH_LF
			if escaped = FALSE then
				exit do
			end if
			lex.i += 1
			hNewLine()

		case CH_SPACE, CH_TAB
			'' Spaces don't change escaped status
			'' (at least gcc/clang support spaces between \ and EOL)
			lex.i += 1

		case CH_BACKSLASH
			escaped = TRUE
			lex.i += 1

		case else
			escaped = FALSE
			lex.i += 1
		end select
	loop
end sub

'' /* C comment */
private sub hSkipComment()
	'' /*
	lex.i += 2

	do
		select case lex.i[0]
		case 0
			lexOops("comment left open")

		case CH_STAR		'' *
			if lex.i[1] = CH_SLASH then	'' */
				exit do
			end if
			lex.i += 1

		case CH_CR
			if lex.i[1] = CH_LF then	'' CRLF
				lex.i += 1
			end if
			lex.i += 1
			hNewLine()

		case CH_LF
			lex.i += 1
			hNewLine()

		case else
			lex.i += 1
		end select
	loop

	'' */
	lex.i += 2
end sub

'' Identifier/keyword lexing: sequences of A-Za-z0-9_
'' Starting out at A-Za-z_
'' Resulting in a TK_ID or KW_* if it's a keyword.
private sub hReadId()
	var begin = lex.i

	do
		lex.i += 1

		select case as const lex.i[0]
		case CH_A   to CH_Z  , _
		     CH_L_A to CH_L_Z, _
		     CH_0   to CH_9  , _
		     CH_UNDERSCORE

		case else
			exit do
		end select
	loop

	hAddTextToken(TK_ID, begin)
end sub

'' Number literal lexing: sequences of A-Za-z0-9_. and +- if preceded by eE.
'' Here in the lexer we don't validate the syntax. Tokens such as '08' or
'' '123foobar' are allowed here even though they're invalid number literals,
'' because they can be used with the ## operator. The syntax checks are done
'' later in hNumberLiteral() when the parsers encounter a TK_NUMBER.
private sub hReadNumber()
	var begin = lex.i

	do
		lex.i += 1

		select case as const lex.i[0]
		case CH_A to CH_Z, CH_L_A to CH_L_Z, CH_0 to CH_9, CH_UNDERSCORE, CH_DOT

		'' +|- is only allowed if preceded by e|E (in other cases, +|-
		'' starts a new token separate from the number literal)
		case CH_PLUS, CH_MINUS
			assert(begin < lex.i)
			select case lex.i[-1]
			case CH_E, CH_L_E
			case else
				exit do
			end select

		case else
			exit do
		end select
	loop

	hAddTextToken(TK_NUMBER, begin)
end sub

'' Look-ahead and check whether the \ is an escaped EOL, and if yes, skip it
private function hSkipEscapedEol() as integer
	var i = 0
	assert(lex.i[0] = CH_BACKSLASH)
	i += 1

	'' Skip space between \ and EOL
	while (lex.i[i] = CH_TAB) or (lex.i[i] = CH_SPACE)
		i += 1
	wend

	select case lex.i[i]
	case CH_CR
		i += 1
		if lex.i[i] = CH_LF then	'' CRLF
			i += 1
		end if
	case CH_LF
		i += 1
	case else
		return FALSE
	end select

	lex.i += i
	hNewLine()
	function = TRUE
end function

'' String/char literal lexing, starting at ", ', or L.
'' String literals may contain escaped EOLs and continue on the next line.
'' For correct lexing, some escape sequences have to be handled here too.
'' The entire string literal is stored into the token as-is, except for escaped
'' EOLs. The real parsing/evaluation is done later by hStringLiteral().
private sub hReadString()
	'' Collect the string literal into lex.text.
	'' We can't just read it from the file buffer because we may solve out
	'' escaped EOLs.
	var j = 0  '' current write position in lex.text

	'' L => wstring, else zstring
	var id = TK_STRING
	if lex.i[0] = CH_L then
		id = TK_WSTRING
		lex.text[j] = CH_L : j += 1
		lex.i += 1
	end if

	'' String or char?
	var quotechar = lex.i[0]
	if quotechar = CH_QUOTE then
		id = iif((id = TK_WSTRING), TK_WCHAR, TK_CHAR)
	end if
	lex.text[j] = quotechar : j += 1
	lex.i += 1

	do
		select case lex.i[0]
		case quotechar
			exit do

		case CH_LF, CH_CR, 0
			lexOops("string/char literal left open")

		case CH_BACKSLASH	'' \
			if hSkipEscapedEol() = FALSE then
				'' Store backslash as-is, not resolving escape sequences
				lex.text[j] = CH_BACKSLASH : j += 1
				lex.i += 1

				select case lex.i[0]
				case CH_BACKSLASH '' \\
					lex.text[j] = CH_BACKSLASH : j += 1
					lex.i += 1
				case quotechar    '' \" or \'
					lex.text[j] = quotechar : j += 1
					lex.i += 1
				end select
			end if

		case else
			lex.text[j] = lex.i[0] : j += 1
			lex.i += 1
		end select

		if j > MAXTEXTLEN then
			lexOops("string literal too long, MAXTEXTLEN=" & MAXTEXTLEN)
		end if
	loop

	'' closing quote
	lex.text[j] = lex.i[0] : j += 1
	lex.i += 1

	'' null-terminator
	lex.text[j] = 0

	tkInsert(lex.x, id, lex.text)
	hSetLocation()
end sub

private sub lexNext()
	'' Identify the next token
	select case as const lex.i[0]
	case CH_TAB, CH_SPACE
		lex.i += 1
		lex.behindspace = TRUE

	case CH_CR
		if lex.i[1] = CH_LF then	'' CRLF
			lex.i += 1
		end if
		hReadBytes(TK_EOL, 1)
		hNewLine()

	case CH_LF
		hReadBytes(TK_EOL, 1)
		hNewLine()

	case CH_FORMFEED
		lex.i += 1

	case CH_EXCL		'' !
		if lex.i[1] = CH_EQ then	'' !=
			hReadBytes(TK_EXCLEQ, 2)
		else
			hReadBytes(TK_EXCL, 1)
		end if

	case CH_DQUOTE		'' "
		hReadString()

	case CH_HASH		'' #
		if lex.i[1] = CH_HASH then	'' ##
			hReadBytes(TK_HASHHASH, 2)
		else
			hReadBytes(TK_HASH, 1)
		end if

	case CH_PERCENT		'' %
		if lex.i[1] = CH_EQ then	'' %=
			hReadBytes(TK_PERCENTEQ, 2)
		else
			hReadBytes(TK_PERCENT, 1)
		end if

	case CH_AMP		'' &
		select case lex.i[1]
		case CH_AMP	'' &&
			hReadBytes(TK_AMPAMP, 2)
		case CH_EQ	'' &=
			hReadBytes(TK_AMPEQ, 2)
		case else
			hReadBytes(TK_AMP, 1)
		end select

	case CH_QUOTE		'' '
		hReadString()

	case CH_LPAREN		'' (
		hReadBytes(TK_LPAREN, 1)

	case CH_RPAREN		'')
		hReadBytes(TK_RPAREN, 1)

	case CH_STAR		'' *
		if lex.i[1] = CH_EQ then	'' *=
			hReadBytes(TK_STAREQ, 2)
		else
			hReadBytes(TK_STAR, 1)
		end if

	case CH_PLUS		'' +
		select case lex.i[1]
		case CH_PLUS	'' ++
			hReadBytes(TK_PLUSPLUS, 2)
		case CH_EQ	'' +=
			hReadBytes(TK_PLUSEQ, 2)
		case else
			hReadBytes(TK_PLUS, 1)
		end select

	case CH_COMMA		'' ,
		hReadBytes(TK_COMMA, 1)

	case CH_MINUS		'' -
		select case lex.i[1]
		case CH_GT	'' ->
			hReadBytes(TK_ARROW, 2)
		case CH_MINUS	'' --
			hReadBytes(TK_MINUSMINUS, 2)
		case CH_EQ	'' -=
			hReadBytes(TK_MINUSEQ, 2)
		case else
			hReadBytes(TK_MINUS, 1)
		end select

	case CH_DOT		'' .
		select case lex.i[1]
		case CH_0 to CH_9   '' 0-9 (Decimal float beginning with '.')
			hReadNumber()
		case CH_DOT
			if lex.i[2] = CH_DOT then	'' ...
				hReadBytes(TK_ELLIPSIS, 3)
			else
				hReadBytes(TK_DOT, 1)
			end if
		case else
			hReadBytes(TK_DOT, 1)
		end select

	case CH_SLASH		'' /
		select case lex.i[1]
		case CH_EQ	'' /=
			hReadBytes(TK_SLASHEQ, 2)
		case CH_SLASH	'' //
			hSkipLineComment()
			lex.behindspace = TRUE
		case CH_STAR	'' /*
			hSkipComment()
			lex.behindspace = TRUE
		case else
			hReadBytes(TK_SLASH, 1)
		end select

	case CH_0 to CH_9	'' 0 - 9
		hReadNumber()

	case CH_COLON		'' :
		hReadBytes(TK_COLON, 1)

	case CH_SEMI		'' ;
		hReadBytes(TK_SEMI, 1)

	case CH_LT		'' <
		select case lex.i[1]
		case CH_LT	'' <<
			if lex.i[2] = CH_EQ then	'' <<=
				hReadBytes(TK_LTLTEQ, 3)
			else
				hReadBytes(TK_LTLT, 2)
			end if
		case CH_EQ	'' <=
			hReadBytes(TK_LTEQ, 2)
		case else
			hReadBytes(TK_LT, 1)
		end select

	case CH_EQ		'' =
		if lex.i[1] = CH_EQ then	'' ==
			hReadBytes(TK_EQEQ, 2)
		else
			hReadBytes(TK_EQ, 1)
		end if

	case CH_GT		'' >
		select case lex.i[1]
		case CH_GT	'' >>
			if lex.i[2] = CH_EQ then	'' >>=
				hReadBytes(TK_GTGTEQ, 3)
			else
				hReadBytes(TK_GTGT, 2)
			end if
		case CH_EQ	'' >=
			hReadBytes(TK_GTEQ, 2)
		case else
			hReadBytes(TK_GT, 1)
		end select

	case CH_QUEST	'' ?
		hReadBytes(TK_QUEST, 1)

	case CH_A       to (CH_L - 1), _	'' A-Z except L
	     (CH_L + 1) to CH_Z
		hReadId()

	case CH_L		'' L
		select case lex.i[1]
		case CH_QUOTE, CH_DQUOTE	'' ', "
			hReadString()
		case else
			hReadId()
		end select

	case CH_LBRACKET	'' [
		hReadBytes(TK_LBRACKET, 1)

	case CH_BACKSLASH	'' \
		'' Check for escaped EOLs and solve them out
		if hSkipEscapedEol() then
			lex.behindspace = TRUE
		else
			lex.i += 1
			hAddTextToken(TK_STRAYBYTE, lex.i - 1)
		end if

	case CH_RBRACKET	'' ]
		hReadBytes(TK_RBRACKET, 1)

	case CH_CIRC		'' ^
		if lex.i[1] = CH_EQ then	'' ^=
			hReadBytes(TK_CIRCEQ, 2)
		else
			hReadBytes(TK_CIRC, 1)
		end if

	case CH_L_A to CH_L_Z, _	'' a-z
	     CH_UNDERSCORE		'' _
		hReadId()

	case CH_LBRACE		'' {
		hReadBytes(TK_LBRACE, 1)

	case CH_PIPE		'' |
		select case lex.i[1]
		case CH_PIPE	'' ||
			hReadBytes(TK_PIPEPIPE, 2)
		case CH_EQ	'' |=
			hReadBytes(TK_PIPEEQ, 2)
		case else
			hReadBytes(TK_PIPE, 1)
		end select

	case CH_RBRACE		'' }
		hReadBytes(TK_RBRACE, 1)

	case CH_TILDE		'' ~
		hReadBytes(TK_TILDE, 1)

	case else
		lex.i += 1
		hAddTextToken(TK_STRAYBYTE, lex.i - 1)
	end select
end sub

''
'' C lexer entry point
''
function lexLoadC(byval x as integer, byval code as zstring ptr, byref source as SourceInfo) as integer
	lex.x = x
	lex.location.source = @source
	lex.location.linenum = 1
	lex.i = code

	'' Tokenize and insert into tk buffer
	'' Loop until EOF...
	do
		'' Loop until the next token was added...
		'' (loop while skipping white-space)
		lex.behindspace = FALSE
		x = lex.x
		do
			if lex.i[0] = 0 then exit do, do
			lexNext()
		loop while lex.x = x
	loop

	function = lex.x
end function

private sub hReadArg(byval tk as integer)
	var j = 0
	var begin = lex.i

	do
		select case lex.i[0]
		case 0, CH_TAB, CH_SPACE, CH_VTAB, CH_FORMFEED, CH_CR, CH_LF
			exit do

		case CH_DQUOTE, CH_QUOTE
			var quotechar = lex.i[0]

			'' String, skip until closing dquote or EOL/EOF
			do
				lex.i += 1

				select case lex.i[0]
				case quotechar
					exit do

				'' Handle \\ and \" if inside "..." string
				'' (so no escape sequences inside '...')
				case CH_BACKSLASH
					if quotechar = CH_DQUOTE then
						select case lex.i[1]
						case CH_BACKSLASH
							lex.text[j] = lex.i[0] : j += 1
							lex.i += 1
						case quotechar
							lex.i += 1
							lex.text[j] = lex.i[0] : j += 1
						case else
							lex.text[j] = lex.i[0] : j += 1
						end select
					else
						lex.text[j] = lex.i[0] : j += 1
					end if

				case 0, CH_CR, CH_LF
					lexOops("string literal left open")

				case else
					lex.text[j] = lex.i[0] : j += 1

				end select

				if j > MAXTEXTLEN then
					lexOops("argument too long, MAXTEXTLEN=" & MAXTEXTLEN)
				end if
			loop

		case else
			lex.text[j] = lex.i[0] : j += 1
		end select

		if j > MAXTEXTLEN then
			lexOops("argument too long, MAXTEXTLEN=" & MAXTEXTLEN)
		end if

		lex.i += 1
	loop

	'' null terminator
	lex.text[j] = 0
	var text = @lex.text

	select case tk
	case TK_MINUS
		tk = hLookupKeyword(lex.frogoptions, text, TK_STRING)
		if tk = TK_STRING then
			oopsLocation(lex.location, "unknown command line option '" + *text + "'")
		end if
		text = NULL
	case TK_STRING
		if strIsValidSymbolId(text) then
			tk = TK_ID
		end if
	end select

	tkInsert(lex.x, tk, text)
	hSetLocation()
end sub

''
'' "Command line" argument lexer, used for turning fbfrog's command line into
'' tokens, and also for turning @files into tokens.
''
'' Syntax rules:
''   * white-space chars separate arguments
''   * any non-white-space sequence is an argument
''   * "..." or '...' can be used in arguments to include whitespace
''   * "..." allows \" and \\ escape sequences
''   * #comments
''
function lexLoadArgs(byval x as integer, byval args as zstring ptr, byref source as SourceInfo) as integer
	lex.x = x
	lex.location.source = @source
	lex.location.linenum = 1
	lex.i = args
	lex.behindspace = TRUE

	do
		select case lex.i[0]
		case 0
			exit do

		case CH_TAB, CH_SPACE, CH_VTAB, CH_FORMFEED
			lex.i += 1

		case CH_CR
			lex.i += 1
			if lex.i[0] = CH_LF then
				lex.i += 1
			end if
			hNewLine()

		case CH_LF
			lex.i += 1
			hNewLine()

		'' #comments
		case CH_HASH
			do
				lex.i += 1
			loop until (lex.i[0] = CH_CR) or (lex.i[0] = CH_LF)

		'' -option
		case CH_MINUS
			hReadArg(TK_MINUS)

		'' @filename
		case CH_AT
			lex.i += 1
			hReadArg(TK_ARGSFILE)

		case else
			'' Non-whitespace: argument starts here, until whitespace/EOF
			hReadArg(TK_STRING)

		end select
	loop

	function = lex.x
end function

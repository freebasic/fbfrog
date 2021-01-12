#include once "fbfrog-args-lex.bi"

#include once "chars.bi"
#include once "util-str.bi"

using tktokens

constructor ArgLexer()
	initKeywords(OPT__FIRST, OPT__LAST)
end constructor

sub ArgLexer.readArg(byval t as integer)
	const MAXTEXTLEN = 1 shl 12
	dim text as zstring * MAXTEXTLEN+2+1
	var j = 0
	var begin = i

	do
		select case i[0]
		case 0, CH_TAB, CH_SPACE, CH_VTAB, CH_FORMFEED, CH_CR, CH_LF
			exit do

		case CH_DQUOTE, CH_QUOTE
			var quotechar = i[0]

			'' String, skip until closing dquote or EOL/EOF
			do
				i += 1

				select case i[0]
				case quotechar
					exit do

				'' Handle \\ and \" if inside "..." string
				'' (so no escape sequences inside '...')
				case CH_BACKSLASH
					if quotechar = CH_DQUOTE then
						select case i[1]
						case CH_BACKSLASH
							text[j] = i[0] : j += 1
							i += 1
						case quotechar
							i += 1
							text[j] = i[0] : j += 1
						case else
							text[j] = i[0] : j += 1
						end select
					else
						text[j] = i[0] : j += 1
					end if

				case 0, CH_CR, CH_LF
					showErrorAndAbort("string literal left open")

				case else
					text[j] = i[0] : j += 1

				end select

				if j > MAXTEXTLEN then
					showErrorAndAbort("argument too long, MAXTEXTLEN=" & MAXTEXTLEN)
				end if
			loop

		case else
			text[j] = i[0] : j += 1
		end select

		if j > MAXTEXTLEN then
			showErrorAndAbort("argument too long, MAXTEXTLEN=" & MAXTEXTLEN)
		end if

		i += 1
	loop

	'' null terminator
	text[j] = 0
	var ptext = @text

	select case t
	case TK_MINUS
		t = lookupKeyword(ptext, TK_STRING)
		if t = TK_STRING then
			oopsLocation(location, "unknown command line option '" + *ptext + "'")
		end if
		ptext = NULL
	case TK_STRING
		if strIsValidSymbolId(ptext) then
			t = TK_ID
		end if
	end select

	tk->insert(x, t, ptext)
	setLocation()
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
function lexLoadArgs(byref sourcectx as SourceContext, byref tk as TokenBuffer, byval x as integer, byval source as SourceInfo ptr) as integer
	dim lex as ArgLexer
	lex.sourcectx = @sourcectx
	lex.tk = @tk
	lex.x = x
	lex.location.source = source
	lex.location.linenum = 1
	lex.i = source->text
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
			lex.newLine()

		case CH_LF
			lex.i += 1
			lex.newLine()

		'' #comments
		case CH_HASH
			do
				lex.i += 1
			loop until (lex.i[0] = 0) or (lex.i[0] = CH_CR) or (lex.i[0] = CH_LF)

		'' -option
		case CH_MINUS
			lex.readArg(TK_MINUS)

		'' @filename
		case CH_AT
			lex.i += 1
			lex.readArg(TK_ARGSFILE)

		case else
			'' Non-whitespace: argument starts here, until whitespace/EOF
			lex.readArg(TK_STRING)

		end select
	loop

	function = lex.x
end function

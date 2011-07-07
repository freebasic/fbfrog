#include once "lex.bi"
#include once "hash.bi"

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

	CH_DEL      = 127

	CH_EOF      = 256
end enum

type LexLine
	as integer num     '' Current line
	as ubyte ptr begin '' Pointer into current buffer
end type

type LexToken
	as integer tk      '' TK_*
	as ubyte ptr i     '' Original text in current buffer
	as integer length
	as LexLine line    '' Where this token was read (error reporting)
end type

'' Determines how much look ahead is available.
'' We're expecting it to be a power of 2.
const LEX_TOKENCOUNT_EXPONENT = 1
const LEX_TOKENCOUNT = (1 shl LEX_TOKENCOUNT_EXPONENT)

'' Token text buffer for lex_text(); this limits the max. possible length of
'' a token, if its text is retrieved during parsing, but it helps providing
'' a nice lexer interface.
const LEX_TEXTCACHE = (1 shl 13)

type LexStuff
	as string filename      '' File name from #include

	as ubyte ptr buffer     '' File content buffer
	as ubyte ptr i          '' Current char, will always be <= limit
	as ubyte ptr limit      '' (end of buffer)
	as LexLine line         '' Current line

	as integer ch           '' Current char or CH_EOF if i = limit

	as LexToken queue(0 to (LEX_TOKENCOUNT - 1))
	as integer token        '' Current token
	as integer aheadcount   '' Number of loaded look ahead tokens

	as integer prevtk       '' Previous token's TK_*

	as zstring * (LEX_TEXTCACHE + 1) text_cache '' lex_text() buffer
	as ubyte ptr cached_token

	as HashTable kwhash     '' Keyword hash table

	#ifdef ENABLE_STATS
		as integer tokencount    '' Tokens overall
	#endif
end type

dim shared as LexStuff lex

'' Retrieves the text of the line containing the location and makes it ready
'' for display during error reporting.
private function peek_line_at(byval linebegin as ubyte ptr) as zstring ptr
	const MAX_PEEKLINE = 512
	static as zstring * (MAX_PEEKLINE + 1) ln

	dim as ubyte ptr r      = linebegin
	dim as ubyte ptr rlimit = lex.limit
	dim as ubyte ptr w      = @ln
	dim as ubyte ptr wlimit = w + MAX_PEEKLINE

	while ((r < rlimit) and (w < wlimit))
		dim as integer ch = *r

		select case as const (ch)
		case CH_LF, CH_CR
			exit while

		'' Replace NULLs, tabs, and other control chars with space.
		'' Otherwise the indicator would be misaligned when tabs are
		'' used in the input code...
		case 0           to (CH_LF    - 1), _
		     (CH_LF + 1) to (CH_CR    - 1), _
		     (CH_CR + 1) to (CH_SPACE - 1), _
		     CH_DEL
			ch = CH_SPACE
		end select

		*w = ch : w += 1
		r += 1
	wend

	'' NULL terminator
	*w = 0

	return @ln
end function

'' Displays a line of the input source code, with an indicator showing the
'' exact location. For example:
''		int foo(int bar#)
''		               ^
private sub print_oops_line(byval token as ubyte ptr, byval ln as LexLine ptr)
	'' Get the current line of source code
	dim as string s = *peek_line_at(ln->begin)

	'' Specifies where the "^" goes
	dim as integer offset = culng(token) - culng(ln->begin)

	'' Determine how many chars can be printed for the error line:
	'' Normally we can fill a line in the console, so get the console width.
	const MIN_LENGTH = 20
	dim as integer length = loword(width()) - 1
	if (length < MIN_LENGTH) then
		length = MIN_LENGTH
	end if

	'' Line too long to fit in console? Then we do some cutting/scrolling.
	if (len(s) > length) then
		dim as integer shift = 0

		'' If the offset is still well visible (i.e. the indicator is
		'' inside the front 75% of the line), just left-align the line.
		'' Otherwise, the line must be scrolled to the left (and the
		'' indicator offset too, of course) to make the location, that
		'' we want to indicate, visible.
		if (offset < ((length * 3) / 4)) then
			'' Still well visible, so just left-align
			s = left(s, length)
		else
			'' When scrolling to the left there are two cases we
			'' may encounter:
			'' a) The line may be so long that we don't hit its end
			''    even when scrolling to the left. In that case we
			''    can just center it so the indicator appears in
			''    the middle of the screen.
			'' b) Or we may hit EOL and then right-align that, to
			''    get the best fit.

			'' Enough chars behind the offset to fill up a half?
			dim as integer half = length / 2
			if ((len(s) - offset) >= half) then
				'' Center
				shift = offset - length + half
				s = mid(s, shift + 1, length)
			else
				'' Right-align
				shift = len(s) - length
				s = right(s, length)
			end if
		end if

		offset -= shift
	end if

	print s + !"\n" + space(offset) + "^"
end sub

private sub private_xoops _
	( _
		byval token as ubyte ptr, _
		byval ln as LexLine ptr, _
		byref message as string _
	)
	print lex.filename & "(" & ln->num & "): oops, " & message
	print_oops_line(token, ln)
	end 1
end sub

private sub tokenizer_xoops(byref message as string)
	private_xoops(lex.i, @lex.line, message)
end sub

'' Skip current char and go to the next
private sub skip_char()
	lex.i += 1
	if (lex.i < lex.limit) then
		lex.ch = lex.i[0]
	else
		'' Keep lex.i <= lex.limit
		if (lex.i > lex.limit) then
			lex.i -= 1
		end if
		lex.ch = CH_EOF
	end if
end sub

function lookahead_char(byval n as integer) as integer
	assert(n > 0)
	if ((lex.i + n) < lex.limit) then
		return lex.i[n]
	end if
	return CH_EOF
end function

private sub token_extend(byval token as LexToken ptr)
	skip_char()
end sub

'' Reads a-z, A-Z, 0-9, _, $ sequences (identifiers, keywords).
private sub read_id(byval token as LexToken ptr)
	token->tk = TK_ID

	do
		skip_char()

		select case as const (lex.ch)
		case CH_A   to CH_Z  , _
		     CH_L_A to CH_L_Z, _
		     CH_0   to CH_9  , _
		     CH_UNDERLINE    , _
		     CH_DOLLAR

		case else
			exit do

		end select
	loop
end sub

private sub read_number_literal(byval token as LexToken ptr)
	token->tk = TK_NUMLIT

	dim as integer numbase = 10
	if (lex.ch = CH_0) then		'' 0
		if (lex.ch = CH_L_X) then	'' 0x
			numbase = 16
			skip_char()
		else
			numbase = 8
		end if
	end if

	skip_char()

	dim as integer found_dot = FALSE
	do
		dim as integer digit = lex.ch

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

		skip_char()
	loop

	'' Exponent? (can be used even without fractional part, e.g. '1e1')
	select case as const (lex.ch)
	case CH_E, CH_L_E   '' 'E', 'e'
		skip_char()

		'' ['+' | '-']
		select case as const (lex.ch)
		case CH_PLUS, CH_MINUS
			skip_char()
		end select

		'' ['0'-'9']
		do
			select case as const (lex.ch)
			case CH_0 to CH_9

			case else
				exit do
			end select

			skip_char()
		loop

	end select

	'' Type suffixes
	'' TODO: are all possible combinations covered?
	'' C is more strict than FB here.
	if (found_dot) then
		select case as const (lex.ch)
		case CH_F, CH_L_F       '' 'F' | 'f'
			skip_char()

		case CH_D, CH_L_D       '' 'D' | 'd'
			skip_char()

		end select
	else
		select case as const (lex.ch)
		case CH_U, CH_L_U       '' 'U' | 'u'
			skip_char()
		end select

		select case as const (lex.ch)
		case CH_L, CH_L_L       '' 'L' | 'l'
			skip_char()
			select case as const (lex.ch)
			case CH_L, CH_L_L       '' 'L' | 'l'
				skip_char()
			end select
		end select
	end if
end sub

'' String/char literal parser
private sub read_string_literal(byval token as LexToken ptr)
	token->tk = TK_STRLIT

	if (lex.ch = CH_L) then
		skip_char()
	end if

	dim as integer quotechar = lex.ch
	if (quotechar = CH_QUOTE) then
		token->tk = TK_CHRLIT
	end if

	'' Opening quote
	skip_char()

	do
		select case (lex.ch)
		case quotechar
			exit do

		case CH_LF, CH_CR, CH_EOF
			tokenizer_xoops("string/char literal is left open")

		case CH_BACKSLASH	'' \
			select case (lex.ch)
			case CH_BACKSLASH, _ '' \\
			     CH_DQUOTE   , _ '' \"
			     CH_QUOTE        '' \'
				skip_char()
			end select

		end select

		skip_char()
	loop

	'' Closing quote
	skip_char()
end sub

private sub read_one(byval token as LexToken ptr, byval tk as integer)
	token->tk = tk
	skip_char()
end sub

private sub skip_line_comment()
	skip_char()
	do
		skip_char()
		select case (lex.ch)
		case CH_LF, CH_CR, CH_EOF
			exit do
		end select
	loop
end sub

private sub skip_multi_comment()
	skip_char()
	do
		skip_char()
		select case (lex.ch)
		case CH_EOF
			tokenizer_xoops("multi-line comment is left open")
		case CH_MUL		'' *
			if (lookahead_char(1) = CH_SLASH) then	'' */
				exit do
			end if
		end select
	loop
	skip_char()
	skip_char()
end sub

'' Parses the next token.
'' Note: must fill in the token structure correctly!
private sub lex_tokenize(byval token as LexToken ptr)
	'' Skip spaces in front of next token
	do
		select case as const (lex.ch)
		case CH_TAB, CH_SPACE

		case CH_SLASH		'' /
			select case (lookahead_char(1))
			case CH_SLASH	'' //
				skip_line_comment()
			case CH_MUL	'' /*
				skip_multi_comment()
			case else
				exit do
			end select

		case else
			exit do
		end select

		skip_char()
	loop

	'' TODO: di/trigraph handling doesn't work with char look ahead!
	'' It would be difficult to handle during file input too, because
	'' string literals shouldn't be affected...
	'' It's not a problem though, this isn't used much anways.
#if 0
	'' Handle di/trigraphs (this must be done only once, not recursively)
	select case as const (lex.ch)
	case CH_QUEST		'' ?
		if (lookahead_char(1) = CH_QUEST) then		'' ??
			'' Trigraphs
			select case as const (lookahead_char(2))
			case CH_EQ				'' ??=	#
				lex.ch = CH_HASH
			case CH_SLASH				'' ??/	\
				lex.ch = CH_BACKSLASH
			case CH_QUOTE				'' ??'	^
				lex.ch = CH_CIRCUMFLEX
			case CH_LPAREN				'' ??(	[
				lex.ch = CH_LBRACKET
			case CH_RPAREN				'' ??)	]
				lex.ch = CH_RBRACKET
			case CH_EXCL				'' ??!	|
				lex.ch = CH_PIPE
			case CH_LT				'' ??<	{
				lex.ch = CH_LBRACE
			case CH_GT				'' ??>	}
				lex.ch = CH_RBRACE
			case CH_MINUS				'' ??-	~
				lex.ch = CH_TILDE
			end select
		end if

	case CH_COLON		'' :
		select case as const (lookahead_char(1))
		case CH_GT	'' :>	]
			lex.ch = CH_RBRACKET
		end select

	case CH_LT		'' <
		select case as const (lookahead_char(1))
		case CH_COLON	'' <:	[
			lex.ch = CH_LBRACKET
		case CH_PERCENT	'' <%	{
			lex.ch = CH_LBRACE
		end select

	case CH_PERCENT		'' %
		select case as const (lookahead_char(1))
		case CH_GT	'' %>	}
			lex.ch = CH_RBRACE
		case CH_PERCENT	'' %:	#
			lex.ch = CH_LBRACE
		end select

	end select
#endif

	'' Next token starts here
	token->line = lex.line
	token->i = lex.i
	token->length = 0

	'' Identify the next token
	select case as const (lex.ch)
	case CH_LF, CH_CR
		token->tk = TK_EOL

		'' CRLF?
		if (lex.ch = CH_CR) then
			if (lookahead_char(1) = CH_LF) then
				'' CR
				skip_char()
			end if
		end if

		'' CR | LF
		skip_char()

		'' After skipping EOL, update the current line
		lex.line.num += 1
		lex.line.begin = lex.i

	case CH_EXCL		'' !
		read_one(token, TK_LOGNOT)
		if (lex.ch = CH_EQ) then	'' !=
			read_one(token, TK_NE)
		end if

	case CH_DQUOTE		'' "
		read_string_literal(token)

	case CH_HASH		'' #
		read_one(token, TK_HASH)
		if (lex.ch = CH_HASH) then	'' ##
			read_one(token, TK_MERGE)
		end if

	case CH_DOLLAR		'' $
		read_id(token)

	case CH_PERCENT		'' %
		read_one(token, TK_MOD)
		if (lex.ch = CH_EQ) then	'' %=
			read_one(token, TK_SELFMOD)
		end if

	case CH_AMP		'' &
		read_one(token, TK_BITAND)
		select case (lex.ch)
		case CH_AMP	'' &&
			read_one(token, TK_LOGAND)
		case CH_EQ	'' &=
			read_one(token, TK_SELFBITAND)
		end select

	case CH_QUOTE		'' '
		read_string_literal(token)

	case CH_LPAREN		'' (
		read_one(token, TK_LPAREN)

	case CH_RPAREN		'' )
		read_one(token, TK_RPAREN)

	case CH_MUL		'' *
		read_one(token, TK_MUL)
		if (lex.ch = CH_EQ) then	'' *=
			read_one(token, TK_SELFMUL)
		end if

	case CH_PLUS		'' +
		read_one(token, TK_ADD)
		select case (lex.ch)
		case CH_PLUS	'' ++
			read_one(token, TK_INCREMENT)
		case CH_EQ	'' +=
			read_one(token, TK_SELFADD)
		end select

	case CH_COMMA		'' ,
		read_one(token, TK_COMMA)

	case CH_MINUS		'' -
		read_one(token, TK_MINUS)
		select case (lex.ch)
		case CH_GT	'' ->
			read_one(token, TK_FIELDDEREF)
		case CH_MINUS	'' --
			read_one(token, TK_DECREMENT)
		case CH_EQ	'' -=
			read_one(token, TK_SELFMINUS)
		end select

	case CH_DOT		'' .
		select case as const (lookahead_char(1))
		case CH_0 to CH_9   '' 0-9 (Decimal float beginning with '.')
			read_number_literal(token)
		case CH_DOT
			if (lookahead_char(2) = CH_DOT) then	'' ...
				read_one(token, TK_ELLIPSIS)
				skip_char()
				skip_char()
			else
				read_one(token, TK_DOT)
			end if
		case else
			read_one(token, TK_DOT)
		end select

	case CH_SLASH		'' /
		read_one(token, TK_DIV)
		if (lex.ch = CH_EQ) then	'' /=
			read_one(token, TK_SELFDIV)
		end if

	case CH_0 to CH_9	'' 0 - 9
		read_number_literal(token)

	case CH_COLON		'' :
		read_one(token, TK_COLON)

	case CH_SEMI		'' ;
		read_one(token, TK_SEMI)

	case CH_LT		'' <
		read_one(token, TK_LT)
		select case (lex.ch)
		case CH_LT	'' <<
			read_one(token, TK_SHL)
			if (lex.ch = CH_EQ) then	'' <<=
				read_one(token, TK_SELFSHL)
			end if
		case CH_EQ	'' <=
			read_one(token, TK_LE)
		end select

	case CH_EQ		'' =
		read_one(token, TK_ASSIGN)
		if (lex.ch = CH_EQ) then	'' ==
			read_one(token, TK_EQ)
		end if

	case CH_GT		'' >
		read_one(token, TK_GT)
		select case (lex.ch)
		case CH_GT	'' >>
			read_one(token, TK_SHR)
			if (lex.ch = CH_EQ) then	'' >>=
				read_one(token, TK_SELFSHR)
			end if
		case CH_EQ	'' >=
			read_one(token, TK_GE)
		end select

	case CH_QUEST		'' ?
		read_one(token, TK_IIF)

	case CH_A       to (CH_L - 1), _	'' A-Z except L
	     (CH_L + 1) to CH_Z
		read_id(token)

	case CH_L		'' L
		if (lookahead_char(1) = CH_DQUOTE) then
			read_string_literal(token)
		else
			read_id(token)
		end if

	case CH_LBRACKET	'' [
		read_one(token, TK_LBRACKET)

	case CH_BACKSLASH	'' \
		read_one(token, TK_BACKSLASH)

	case CH_RBRACKET	'' ]
		read_one(token, TK_RBRACKET)

	case CH_CIRCUMFLEX	'' ^
		read_one(token, TK_BITXOR)
		if (lex.ch = CH_EQ) then	'' ^=
			read_one(token, TK_SELFBITXOR)
		end if

	case CH_UNDERLINE	'' _
		read_id(token)

	case CH_L_A to CH_L_Z	'' a-z
		read_id(token)

	case CH_LBRACE		'' {
		read_one(token, TK_LBRACE)

	case CH_PIPE		'' |
		read_one(token, TK_BITOR)
		select case (lex.ch)
		case CH_PIPE	'' ||
			read_one(token, TK_LOGOR)
		case CH_EQ	'' |=
			read_one(token, TK_SELFBITOR)
		end select

	case CH_RBRACE		'' }
		read_one(token, TK_RBRACE)

	case CH_TILDE		'' ~
		read_one(token, TK_BITNOT)

	case CH_EOF
		token->tk = TK_EOF

	case else
		tokenizer_xoops("unexpected character: '" & chr(lex.ch) & "'" & _
				", &h" + hex(lex.ch, 2))

	end select

	'' The token ends here
	token->length = culng(lex.i) - culng(token->i)

	if (token->tk = TK_ID) then
		'' Is this a keyword?
		dim as HashItem ptr item = _
		        hash_lookup(@lex.kwhash, _
		                    token->i, _
		                    token->length, _
		                    hash_hash(token->i, token->length))
		if (item->s) then
			token->tk = item->data
		end if
	end if

	#ifdef ENABLE_STATS
		lex.tokencount += 1
	#endif
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' Parser interface

sub lex_xoops(byref message as string)
	with (lex.queue(lex.token))
		private_xoops(.i, @.line, message)
	end with
end sub

function lex_at_line_begin() as integer
	return (lex.prevtk = TK_EOL)
end function

function lex_tk() as integer
	return lex.queue(lex.token).tk
end function

function lex_text() as zstring ptr
	'' Tokens don't store their text; that'd require the text to be stored
	'' for every single token. For most it's never needed though, so we can
	'' just use one buffer (the "text cache") and copy token text into that
	'' as requested, so that we can return a nice zstring ptr.
	with (lex.queue(lex.token))
		if (lex.cached_token <> .i) then
			if (.length > LEX_TEXTCACHE) then
				lex_xoops("token too big, the soft-limit needs to be raised!")
			end if
			fb_MemCopy(byval @lex.text_cache, byval .i, .length)
			lex.text_cache[.length] = 0
		end if
	end with
	return @lex.text_cache
end function

function lex_lookahead_tk(byval n as integer) as integer
	assert((n > 0) and (n < LEX_TOKENCOUNT))
	return lex.queue((lex.token + n) and (LEX_TOKENCOUNT - 1)).tk
end function

'' Skips the current token and loads the next one, if there's none waiting.
sub lex_skip()
	lex.prevtk = lex.queue(lex.token).tk
	lex.token = (lex.token + 1) and (LEX_TOKENCOUNT - 1)
	if (lex.aheadcount > 0) then
		lex.aheadcount -= 1
	else
		lex_tokenize(@lex.queue(lex.token))
	end if
end sub

private sub load_file(byref filename as string)
	lex.filename = filename

	dim as integer f = freefile()
	if (open(filename, for binary, access read, as #f)) then
		xoops("could not open this file, maybe it doesn't exist?")
	end if

	dim as longint size = lof(f)
	if (size >= &h7FFFFFFFull) then
		xoops("wow, where did you get a big header file like that?")
	end if

	if (size > 0) then
		lex.buffer = xallocate(size)

		dim as integer found = 0
		dim as integer result = get(#f, , *lex.buffer, size, found)
		if (result or (found <> size)) then
			xoops("could not read from this file, even though I opened it!")
		end if

		lex.limit = lex.buffer + found
	else
		lex.buffer = NULL
		lex.limit = NULL
	end if

	lex.i = lex.buffer

	close #f
end sub

'' Sets up the lexer to parse a specific file. Future lex_*() calls will
'' tokenize that file's content, until the next file is selected...
sub lex_open(byref filename as string)
	load_file(filename)

	lex.line.num = 1
	lex.line.begin = lex.i

	lex.aheadcount = 0 '' Reset token queue

	'' Fake a TK_EOL here at the beginning of the file, so
	'' lex_at_line_begin() will return TRUE after the first lex_skip(),
	'' in order to find #directives at file begin.
	lex.prevtk = TK_EOL

	'' This reads the first char from the file (and may set the current
	'' char to CH_EOF), but no token will be parsed until the first
	'' lex_skip().
	'' (hack... this will underflow if lex.i is NULL on an empty file,
	'' and then skip_char() will overflow it again and reach EOF)
	lex.i -= 1
	skip_char()

	'' Read first token
	lex_skip()
end sub

sub lex_close()
	if (lex.buffer) then
		deallocate(lex.buffer)
	end if
	lex.buffer = NULL
	lex.i = NULL
	lex.limit = NULL
end sub

'' Careful: keep in sync with the lex.bi:TK_* token id enum!
dim shared as zstring ptr keywords(0 to (TK__KWCOUNT - 1)) = _
{ _
	@"_Bool"   , _
	@"_Complex", _
	@"_Imaginary", _
	@"auto"    , _
	@"break"   , _
	@"case"    , _
	@"char"    , _
	@"const"   , _
	@"continue", _
	@"default" , _
	@"define"  , _
	@"defined" , _
	@"do"      , _
	@"double"  , _
	@"else"    , _
	@"elif"    , _
	@"endif"   , _
	@"enum"    , _
	@"extern"  , _
	@"float"   , _
	@"for"     , _
	@"goto"    , _
	@"if"      , _
	@"ifdef"   , _
	@"ifndef"  , _
	@"include" , _
	@"inline"  , _
	@"int"     , _
	@"long"    , _
	@"pragma"  , _
	@"register", _
	@"restrict", _
	@"return"  , _
	@"short"   , _
	@"signed"  , _
	@"sizeof"  , _
	@"static"  , _
	@"struct"  , _
	@"switch"  , _
	@"typedef" , _
	@"undef"   , _
	@"union"   , _
	@"unsigned", _
	@"void"    , _
	@"volatile", _
	@"while"     _
}

sub lex_global_init()
	'' Load the keywords
	hash_init(@lex.kwhash, 7)
	for i as integer = 0 to (TK__KWCOUNT - 1)
		dim as zstring ptr kw = keywords(i)
		dim as integer length = len(*kw)
		dim as uinteger hash = hash_hash(kw, length)
		dim as HashItem ptr item = hash_lookup(@lex.kwhash, kw, length, hash)
		assert(item->s = NULL)
		item->s = kw
		item->length = length
		item->hash = hash
		item->data = i + TK__FIRSTKW
		lex.kwhash.count += 1
	next
end sub

sub lex_global_end()
	#ifdef ENABLE_STATS
		print "lex stats:"
		print !"\t" & lex.tokencount & " tokens"
		hash_stats(@lex.kwhash)
	#endif
	hash_end(@lex.kwhash)
end sub

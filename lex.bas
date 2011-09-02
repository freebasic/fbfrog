#include once "lex.bi"
#include once "hash.bi"

dim shared as HashTable fbkwhash

dim shared as zstring ptr fbkeywords(0 to ...) = _
{ _
	@"ABS"        , @"ACCESS"     , @"ACOS"       , @"ALIAS"      , _
	@"AND"        , @"ANDALSO"    , @"ANY"        , @"APPEND"     , _
	@"AS"         , @"ASC"        , @"ASIN"       , @"ASM"        , _
	@"ATAN2"      , @"ATN"        , _
	@"BASE"       , @"BINARY"     , @"BYREF"      , @"BYTE"       , _
	@"BYVAL"      , _
	@"CALL"       , @"CASE"       , @"CAST"       , @"CBYTE"      , _
	@"CDBL"       , @"CDECL"      , @"CHR"        , @"CINT"       , _
	@"CIRCLE"     , @"CLASS"      , @"CLNG"       , @"CLNGINT"    , _
	@"CLOSE"      , @"COLOR"      , @"COMMON"     , @"CONST"      , _
	@"CONSTRUCTOR", @"CONTINUE"   , @"COS"        , @"CPTR"       , _
	@"CSHORT"     , @"CSIGN"      , @"CSNG"       , @"CUBYTE"     , _
	@"CUINT"      , @"CULNG"      , @"CULNGINT"   , @"CUNSG"      , _
	@"CUSHORT"    , @"CVD"        , @"CVI"        , @"CVL"        , _
	@"CVLONGINT"  , @"CVS"        , @"CVSHORT"    , _
	@"DATA"       , @"DECLARE"    , @"DEFBYTE"    , @"DEFDBL"     , _
	@"DEFINE"     , @"DEFINED"    , @"DEFINT"     , @"DEFLNG"     , _
	@"DEFLONGINT" , @"DEFSHORT"   , @"DEFSNG"     , @"DEFSTR"     , _
	@"DEFUBYTE"   , @"DEFUINT"    , @"DEFULNG"    , @"DEFULONGINT", _
	@"DEFUSHORT"  , @"DELETE"     , @"DESTRUCTOR" , @"DIM"        , _
	@"DO"         , @"DOUBLE"     , @"DRAW"       , @"DYNAMIC"    , _
	@"ELSE"       , @"ELSEIF"     , @"ENCODING"   , @"END"        , _
	@"ENDIF"      , @"ENDMACRO"   , @"ENUM"       , @"EQV"        , _
	@"ERASE"      , @"ERR"        , @"ERROR"      , @"ESCAPE"     , _
	@"EXIT"       , @"EXP"        , @"EXPLICIT"   , @"EXPORT"     , _
	@"EXTERN"     , _
	@"FIELD"      , @"FIX"        , @"FOR"        , @"FRAC"       , _
	@"FUNCTION"   , _
	@"GET"        , @"GOSUB"      , @"GOTO"       , _
	@"IF"         , @"IFDEF"      , @"IFNDEF"     , @"IIF"        , _
	@"IMAGECREATE", @"IMP"        , @"IMPORT"     , @"INCLIB"     , _
	@"INCLUDE"    , @"INPUT"      , @"INSTR"      , @"INSTRREV"   , _
	@"INT"        , @"INTEGER"    , @"IS"         , _
	@"LANG"       , @"LBOUND"     , @"LEN"        , @"LET"        , _
	@"LIB"        , @"LIBPATH"    , @"LINE"       , @"LOCAL"      , _
	@"LOCK"       , @"LOG"        , @"LONG"       , @"LONGINT"    , _
	@"LOOP"       , @"LPRINT"     , @"LSET"       , @"LTRIM"      , _
	@"MACRO"      , @"MID"        , @"MKD"        , @"MKI"        , _
	@"MKL"        , @"MKLONGINT"  , @"MKS"        , @"MKSHORT"    , _
	@"MOD"        , @"MSBITFIELDS", _
	@"NAKED"      , @"NAME"       , @"NAMESPACE"  , @"NEW"        , _
	@"NEXT"       , @"NOGOSUB"    , @"NOKEYWORD"  , @"NOT"        , _
	@"ON"         , @"ONCE"       , @"OPEN"       , @"OPERATOR"   , _
	@"OPTION"     , @"OR"         , @"ORELSE"     , @"OUTPUT"     , _
	@"OVERLOAD"   , _
	@"PAINT"      , @"PALETTE"    , @"PASCAL"     , @"PEEK"       , _
	@"POINT"      , @"POINTER"    , @"POKE"       , @"POP"        , _
	@"PUSH"       , @"PRAGMA"     , @"PRESERVE"   , @"PRESET"     , _
	@"PRINT"      , @"PRIVATE"    , @"PROCPTR"    , @"PROPERTY"   , _
	@"PROTECTED"  , @"PSET"       , @"PTR"        , @"PUBLIC"     , _
	@"PUT"        , _
	@"RANDOM"     , @"READ"       , @"REDIM"      , @"REM"        , _
	@"RESTORE"    , @"RESUME"     , @"RETURN"     , @"RSET"       , _
	@"RTRIM"      , _
	@"SADD"       , @"SCOPE"      , @"SCREEN"     , @"SCREENRES"  , _
	@"SEEK"       , @"SELECT"     , @"SGN"        , @"SHARED"     , _
	@"SHL"        , @"SHORT"      , @"SHR"        , @"SIN"        , _
	@"SINGLE"     , @"SIZEOF"     , @"SPC"        , @"SQR"        , _
	@"STATIC"     , @"STDCALL"    , @"STEP"       , @"STR"        , _
	@"STRING"     , @"STRPTR"     , @"SUB"        , @"SWAP"       , _
	@"TAB"        , @"TAN"        , @"THEN"       , @"TO"         , _
	@"TRIM"       , @"TYPE"       , @"TYPEOF"     , _
	@"UBOUND"     , @"UBYTE"      , @"UINTEGER"   , @"ULONG"      , _
	@"ULONGINT"   , @"UNDEF"      , @"UNION"      , @"UNLOCK"     , _
	@"UNSIGNED"   , @"UNTIL"      , @"USHORT"     , @"USING"      , _
	@"VAR"        , @"VARPTR"     , @"VA_FIRST"   , @"VIEW"       , _
	@"WCHR"       , @"WEND"       , @"WHILE"      , @"WIDTH"      , _
	@"WINDOW"     , @"WITH"       , @"WRITE"      , @"WSTR"       , _
	@"WSTRING"    , _
	@"XOR"        , _
	@"ZSTRING"      _
}

type LexToken
	as integer id       '' TK_*
	as zstring ptr text '' Identifiers and number/string literals, or NULL
end type
sub transforms_global_init()
	hash_init(@emit.fbkwhash, 9)
	for i as integer = 0 to ubound(fbkeywords)
		dim as zstring ptr kw = fbkeywords(i)
		dim as integer length = len(*kw)
		dim as uinteger hash = hash_hash(kw, length)
		dim as HashItem ptr item = hash_lookup(@emit.fbkwhash, kw, length, hash)
		assert(item->s = NULL)
		item->s = kw
		item->length = length
		item->hash = hash
		emit.fbkwhash.count += 1
	next
end sub

private function is_fb_keyword(byval id as zstring ptr) as integer
	dim as integer length = len(*id)
	return (hash_lookup(@emit.fbkwhash, id, length, _
	                    hash_hash(id, length))->s <> NULL)
end function

type LexStuff
	as LexToken ptr p
	as integer count
	as integer room
end type

dim shared as LexStuff lex

sub lex_init()
	'' Load the keywords
	hash_init(@file.kwhash, 7)
	for i as integer = 0 to (TK__KWCOUNT - 1)
		dim as zstring ptr kw = keywords(i)
		dim as integer length = len(*kw)
		dim as uinteger hash = hash_hash(kw, length)
		dim as HashItem ptr item = hash_lookup(@file.kwhash, _
		                                       kw, length, hash)
		assert(item->s = NULL)
		item->s = kw
		item->length = length
		item->hash = hash
		item->data = i + TK__FIRSTKW
		file.kwhash.count += 1
	next
end sub

sub lex_insert _
	( _
		byval i as integer, _
		byval id as integer, _
		byval text as zstring ptr _
	)
	dim as integer length = any
	if (text) then
		length = len(*text)
	else
		length = 0
	end if
	lex_insert_raw(i, id, text, length)
end sub

sub lex_insert_raw _
	( _
		byval i as integer, _
		byval id as integer, _
		byval text as zstring ptr, _
		byval length as integer _
	)

	if (lex.room = lex.count) then
		lex.room += 512
		lex.p = xreallocate(lex.p, lex.room * sizeof(LexToken))
	end if
	dim as LexToken ptr p = lex.p + i
	if (i < lex.count) then
		memmove(p + 1, p, (lex.count - i) * sizeof(LexToken))
	end if
	lex.count += 1

	p->id = id
	if (length > 0) then
		assert(text)
		p->text = xallocate(length + 1)
		memcpy(p->text, text, length)
		p->text[length] = 0
	else
		p->text = NULL
	end if
end sub

sub lex_remove(byval i as integer)
	xassert((i >= 0) and (i < lex.count))

	dim as LexToken ptr p = lex.p + i
	deallocate(p->text)

	lex.count -= 1
	if (i < lex.count) then
		memmove(p, p + 1, (lex.count - i) * sizeof(LexToken))
	end if
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

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

type TokenizerFile
	as string name          '' File name

	as ubyte ptr buffer     '' File content buffer
	as ubyte ptr i          '' Current char, will always be <= limit
	as ubyte ptr limit      '' (end of buffer)

	as integer linenum      '' Current line
	as ubyte ptr linebegin  '' Current line's begin

	as integer tk           '' Current token (TK_*)
	as ubyte ptr tkbegin    '' Its text
	as integer tklength     '' Length of its text

	as HashTable kwhash     '' Keyword hash table
end type

dim shared as TokenizerFile file

'' Retrieves the text of the line containing the location and makes it ready
'' for display during error reporting.
private function peek_line_at(byval linebegin as ubyte ptr) as zstring ptr
	const MAX_PEEKLINE = 512
	static as zstring * (MAX_PEEKLINE + 1) ln

	dim as ubyte ptr r      = linebegin
	dim as ubyte ptr rlimit = file.limit
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
private sub print_oops_line(byval token as ubyte ptr, byval line as LexLine ptr)
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

private sub file_xoops(byref message as string)
	print file.name & "(" & file.linenum & "): oops, " & message
	end 1
end sub

'' Reads a-z, A-Z, 0-9, _, $ sequences (identifiers, keywords).
private sub read_id()
	file.tk = TK_ID

	do
		file.i += 1

		select case as const (file.i[0])
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

private sub read_number()
	file.tk = TK_NUMLIT

	dim as integer numbase = 10
	if (file.i[0] = CH_0) then '' 0
		if (file.i[0] = CH_L_X) then '' 0x
			numbase = 16
			file.i += 1
		else
			numbase = 8
		end if
	end if

	file.i += 1

	dim as integer found_dot = FALSE
	do
		dim as integer digit = file.i[0]

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

		file.i += 1
	loop

	'' Exponent? (can be used even without fractional part, e.g. '1e1')
	select case (file.i[0])
	case CH_E, CH_L_E   '' 'E', 'e'
		file.i += 1

		'' ['+' | '-']
		select case (file.i[0])
		case CH_PLUS, CH_MINUS
			file.i += 1
		end select

		'' ['0'-'9']
		while ((file.i[0] >= CH_0) and (file.i[0] <= CH_9))
			file.i += 1
		wend

	end select

	'' Type suffixes
	'' TODO: are all possible combinations covered?
	if (found_dot) then
		select case (file.i[0])
		case CH_F, CH_L_F, _    '' 'F' | 'f'
		     CH_D, CH_L_D       '' 'D' | 'd'
			file.i += 1
		end select
	else
		select case (file.i[0])
		case CH_U, CH_L_U       '' 'U' | 'u'
			file.i += 1
		end select

		select case (file.i[0])
		case CH_L, CH_L_L       '' 'L' | 'l'
			file.i += 1
			select case (file.i[0])
			case CH_L, CH_L_L       '' 'L' | 'l'
				file.i += 1
			end select
		end select
	end if
end sub

'' String/char literal parser
private sub read_string()
	dim as integer quotechar = file.i[0]
	if (quotechar = CH_QUOTE) then
		file.tk = TK_CHRLIT
	else
		file.tk = TK_STRLIT
	end if

	do
		file.i += 1

		select case (file.i[0])
		case quotechar
			file.i += 1
			exit do

		case CH_LF, CH_CR, CH_EOF
			file_xoops("string/char literal is left open")

		case CH_BACKSLASH	'' \
			select case (file.i[0])
			case CH_BACKSLASH, _ '' \\
			     CH_DQUOTE   , _ '' \"
			     CH_QUOTE        '' \'
				file.i += 1
			end select

		end select
	loop
end sub

private sub read_one(byval tk as integer)
	file.tk = tk
	file.i += 1
end sub

private sub skip_line_comment()
	file.i += 1
	do
		file.i += 1
		select case (file.i[0])
		case CH_LF, CH_CR, 0
			exit do
		end select
	loop
end sub

private sub skip_multi_comment()
	file.i += 1
	do
		file.i += 1
		select case (file.i[0])
		case 0
			file_xoops("multi-line comment is left open")
		case CH_MUL		'' *
			if (file.i[1] = CH_SLASH) then	'' */
				file.i += 2
				exit do
			end if
		end select
	loop
end sub

'' Parses the next token.
'' Note: must fill in the token structure correctly!
private sub lex_tokenize()
	'' Skip spaces in front of next token
	do
		select case (file.i[0])
		case CH_TAB, CH_SPACE

		case CH_SLASH		'' /
			select case (file.i[1])
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

		file.i += 1
	loop

	'' Identify the next token
	select case as const (file.i[0])
	case CH_LF, CH_CR
		file.tk = TK_EOL

		'' CRLF?
		if (file.i[0] = CH_CR) then
			if (file.i[1] = CH_LF) then
				'' CR
				file.i += 1
			end if
		end if

		'' CR | LF
		file.i += 1

		'' After skipping EOL, update the current line
		file.linenum += 1
		file.linebegin = file.i

	case CH_EXCL		'' !
		read_one(TK_LOGNOT)
		if (file.i[0] = CH_EQ) then	'' !=
			read_one(TK_NE)
		end if

	case CH_DQUOTE		'' "
		read_string()

	case CH_HASH		'' #
		read_one(TK_HASH)
		if (file.i[0] = CH_HASH) then	'' ##
			read_one(TK_MERGE)
		end if

	case CH_DOLLAR		'' $
		read_id()

	case CH_PERCENT		'' %
		read_one(TK_MOD)
		if (file.i[0] = CH_EQ) then	'' %=
			read_one(TK_SELFMOD)
		end if

	case CH_AMP		'' &
		read_one(TK_BITAND)
		select case (file.i[0])
		case CH_AMP	'' &&
			read_one(TK_LOGAND)
		case CH_EQ	'' &=
			read_one(TK_SELFBITAND)
		end select

	case CH_QUOTE		'' '
		read_string()

	case CH_LPAREN		'' (
		read_one(TK_LPAREN)

	case CH_RPAREN		'' )
		read_one(TK_RPAREN)

	case CH_MUL		'' *
		read_one(TK_MUL)
		if (file.i[0] = CH_EQ) then	'' *=
			read_one(TK_SELFMUL)
		end if

	case CH_PLUS		'' +
		read_one(TK_ADD)
		select case (file.i[0])
		case CH_PLUS	'' ++
			read_one(TK_INCREMENT)
		case CH_EQ	'' +=
			read_one(TK_SELFADD)
		end select

	case CH_COMMA		'' ,
		read_one(TK_COMMA)

	case CH_MINUS		'' -
		read_one(TK_SUB)
		select case (file.i[0])
		case CH_GT	'' ->
			read_one(TK_FIELDDEREF)
		case CH_MINUS	'' --
			read_one(TK_DECREMENT)
		case CH_EQ	'' -=
			read_one(TK_SELFSUB)
		end select

	case CH_DOT		'' .
		select case (file.i[1])
		case CH_0 to CH_9   '' 0-9 (Decimal float beginning with '.')
			read_number()
		case CH_DOT
			if (file.i[2] = CH_DOT) then	'' ...
				read_one(TK_ELLIPSIS)
				file.i += 2
			else
				read_one(TK_DOT)
			end if
		case else
			read_one(TK_DOT)
		end select

	case CH_SLASH		'' /
		read_one(TK_DIV)
		if (file.i[0] = CH_EQ) then	'' /=
			read_one(TK_SELFDIV)
		end if

	case CH_0 to CH_9	'' 0 - 9
		read_number()

	case CH_COLON		'' :
		read_one(TK_COLON)

	case CH_SEMI		'' ;
		read_one(TK_SEMI)

	case CH_LT		'' <
		read_one(TK_LT)
		select case (file.i[0])
		case CH_LT	'' <<
			read_one(TK_SHL)
			if (file.i[0] = CH_EQ) then	'' <<=
				read_one(TK_SELFSHL)
			end if
		case CH_EQ	'' <=
			read_one(TK_LE)
		end select

	case CH_EQ		'' =
		read_one(TK_ASSIGN)
		if (file.i[0] = CH_EQ) then	'' ==
			read_one(TK_EQ)
		end if

	case CH_GT		'' >
		read_one(TK_GT)
		select case (file.i[0])
		case CH_GT	'' >>
			read_one(TK_SHR)
			if (file.i[0] = CH_EQ) then	'' >>=
				read_one(TK_SELFSHR)
			end if
		case CH_EQ	'' >=
			read_one(TK_GE)
		end select

	case CH_QUEST		'' ?
		read_one(TK_IIF)

	case CH_A       to (CH_L - 1), _	'' A-Z except L
	     (CH_L + 1) to CH_Z
		read_id()

	case CH_L		'' L
		if (file.i[1] = CH_DQUOTE) then
			file.i += 1
			read_string()
		else
			read_id()
		end if

	case CH_LBRACKET	'' [
		read_one(TK_LBRACKET)

	case CH_BACKSLASH	'' \
		read_one(TK_BACKSLASH)

	case CH_RBRACKET	'' ]
		read_one(TK_RBRACKET)

	case CH_CIRCUMFLEX	'' ^
		read_one(TK_BITXOR)
		if (file.i[0] = CH_EQ) then	'' ^=
			read_one(TK_SELFBITXOR)
		end if

	case CH_UNDERLINE	'' _
		read_id()

	case CH_L_A to CH_L_Z	'' a-z
		read_id()

	case CH_LBRACE		'' {
		read_one(TK_LBRACE)

	case CH_PIPE		'' |
		read_one(TK_BITOR)
		select case (file.i[0])
		case CH_PIPE	'' ||
			read_one(TK_LOGOR)
		case CH_EQ	'' |=
			read_one(TK_SELFBITOR)
		end select

	case CH_RBRACE		'' }
		read_one(TK_RBRACE)

	case CH_TILDE		'' ~
		read_one(TK_BITNOT)

	case CH_EOF

	case else
		file_xoops("unexpected character: &h" + hex(file.i[0], 2))

	end select

	'' The token ends here
	lex.length = culng(lex.i) - culng(lex.i)

	if (file.tk = TK_ID) then
		'' Is this a keyword?
		dim as HashItem ptr item = _
		        hash_lookup(@lex.kwhash, _
		                    lex.i, _
		                    lex.length, _
		                    hash_hash(lex.i, lex.length))
		if (item->s) then
			file.tk = item->data
		end if
	end if
end sub

const IOBUFFER_SIZE = 1024 * 8
static shared as zstring * IOBUFFER_SIZE iobuffer

sub lex_load(byref filename as string)
	map_file(filename)

	'' Read in the whole file content
	dim as integer f = freefile()
	if (open(filename, for binary, access read, as #f)) then
		xoops("could not open this file; does it exist?")
	end if

	do
		dim as integer size = 0
		if (get(#f, , *cptr(ubyte ptr, @iobuffer), _
		        IOBUFFER_SIZE, size)) then
			xoops("my file I/Os are failing me, how sad")
		end if

		'' EOF?
		if (size = 0) then
			exit do
		end if

		gap_in(@lex.f->gap, @iobuffer, size)
	loop

	close #f
end sub

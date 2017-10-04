#include once "c-common.bi"
#include once "chars.bi"

using tktokens

type IntTypeSuffixes
	as integer have_u, have_l, have_ll
end type

''
'' Check whether the number literal token (TK_NUMBER) is a valid number literal,
'' and build a CONSTI/CONSTF AstNode representing its value (the literal saved
'' as-is in textual form, without type suffixes) and data type.
''
'' As a special case, in CPP expressions, number literals are always treated as
'' 64bit signed or unsigned, no matter what dtype was given as suffix and
'' ignoring the "int" default.
''
'' These are covered:
''    123        (decimal)
''    .123       (decimal float)
''    123.123    (decimal float)
''    0xAABBCCDD (hexadecimal)
''    010        (octal)
''    010.0      (decimal float, not octal float)
''    0b10101    (binary; non-standard but supported by gcc/clang)
''    0          (we treat this as decimal, even though it's octal,
''               for prettier FB code: 0 vs &o0)
'' There also is some simple float exponent and type suffix parsing.
''
'' We have to parse the number literal (but without type suffix) first before we
'' can decide whether it's an integer or float. This decides whether a leading
'' zero indicates octal or not.
''
type CNumLitParser
private:
	p as ubyte ptr '' null-terminated literal text
	as integer is_cpp, clong32, is_float, have_nonoct_digit
	as integer numbase
	ast as AstNode ptr

public:
	errmsg as string

	declare constructor _
		( _
			byval text as zstring ptr, _
			byval is_cpp as integer, _
			byval clong32 as integer _
		)
	declare destructor()
	declare function takeAst() as AstNode ptr
	declare sub parseNonDecimalPrefix()
	declare sub parseBody()
	declare sub parseExponent()
	declare function parseFloatTypeSuffixes() as integer
	declare function parseIntTypeSuffixes() as IntTypeSuffixes
	declare function getIntNumLitType(byval suffixes as IntTypeSuffixes) as integer
	declare sub parse()
end type

constructor CNumLitParser _
	( _
		byval text as zstring ptr, _
		byval is_cpp as integer, _
		byval clong32 as integer _
	)

	p = text
	this.is_cpp = is_cpp
	this.clong32 = clong32
	numbase = 10
	is_float = FALSE
	have_nonoct_digit = FALSE

	parse()

	p = NULL
end constructor

destructor CNumLitParser()
	delete ast
end destructor

function CNumLitParser.takeAst() as AstNode ptr
	function = ast
	ast = NULL
end function

sub CNumLitParser.parseNonDecimalPrefix()
	'' 0, 0x, 0X, 0b, 0B
	if p[0] = CH_0 then '' 0
		select case p[1]
		case CH_L_B, CH_B  '' 0b, 0B
			p += 2
			numbase = 2
		case CH_L_X, CH_X  '' 0x, 0X
			p += 2
			numbase = 16
		case CH_0 to CH_9
			p += 1
			numbase = 8
		end select
	end if
end sub

'' Body (integer part + fractional part, if any)
sub CNumLitParser.parseBody()
	select case numbase
	case 2
		while (p[0] = CH_0) or (p[0] = CH_1)
			p += 1
		wend

	case 16
		do
			select case as const p[0]
			case CH_0 to CH_9, CH_A to CH_F, CH_L_A to CH_L_F
			case else
				exit do
			end select
			p += 1
		loop

	case else
		do
			select case as const p[0]
			case CH_0 to CH_7
				'' These digits are allowed in both dec/oct literals
			case CH_8, CH_9
				'' These digits are only allowed in dec literals, not
				'' oct, but we don't know which it is yet.
				have_nonoct_digit = TRUE
			case CH_DOT
				'' Only one dot allowed
				if is_float then exit do
				is_float = TRUE
			case else
				exit do
			end select
			p += 1
		loop
	end select
end sub

sub CNumLitParser.parseExponent()
	'' Exponent? (can be used even without fractional part, e.g. '1e1')
	select case p[0]
	case CH_E, CH_L_E   '' 'E', 'e'
		is_float = TRUE
		p += 1

		'' ['+' | '-']
		select case p[0]
		case CH_PLUS, CH_MINUS
			p += 1
		end select

		'' ['0'-'9']*
		while (p[0] >= CH_0) and (p[0] <= CH_9)
			p += 1
		wend
	end select
end sub

function CNumLitParser.parseFloatTypeSuffixes() as integer
	'' Float type suffixes
	select case p[0]
	case CH_F, CH_L_F    '' 'F' | 'f'
		p += 1
		return TYPE_SINGLE
	case CH_D, CH_L_D    '' 'D' | 'd'
		p += 1
		return TYPE_DOUBLE
	end select
	return TYPE_NONE
end function

'' Integer type suffixes:
''  l, ll, ul, ull, lu, llu
'' MSVC-specific ones:
''  [u]i{8|16|32|64}
'' All letters can also be upper-case.
function CNumLitParser.parseIntTypeSuffixes() as IntTypeSuffixes
	dim suffixes as IntTypeSuffixes = (FALSE, FALSE, FALSE)

	select case p[0]
	case CH_U, CH_L_U       '' u
		p += 1
		suffixes.have_u = TRUE
	end select

	select case p[0]
	case CH_L, CH_L_L       '' l, [u]l
		p += 1
		select case p[0]
		case CH_L, CH_L_L       '' l, [u]ll
			p += 1
			suffixes.have_ll = TRUE
		case else
			suffixes.have_l = TRUE
		end select

		if suffixes.have_u = FALSE then
			select case p[0]
			case CH_U, CH_L_U       '' u, llu
				p += 1
				suffixes.have_u = TRUE
			end select
		end if

	case CH_I, CH_L_I       '' i, [u]i
		select case p[1]
		case CH_8 : p += 2 '' i8
		case CH_1 : if p[2] = CH_6 then : p += 3 : end if  '' i16
		case CH_3 : if p[2] = CH_2 then : p += 3 : end if  '' i32
		case CH_6 : if p[2] = CH_4 then : p += 3 : suffixes.have_ll = TRUE : end if  '' i64
		end select
	end select

	return suffixes
end function

function CNumLitParser.getIntNumLitType(byval suffixes as IntTypeSuffixes) as integer
	'' In CPP mode, all integer literals are 64bit, the 'l' suffix is ignored
	if is_cpp then
		return iif(suffixes.have_u, TYPE_ULONGINT, TYPE_LONGINT)
	end if

	'' In C mode, integer literals default to 'int', and suffixes are respected
	if suffixes.have_ll then
		return iif(suffixes.have_u, TYPE_ULONGINT, TYPE_LONGINT)
	end if

	if suffixes.have_l then
		return typeGetCLong(suffixes.have_u, clong32)
	end if

	return iif(suffixes.have_u, TYPE_ULONG, TYPE_LONG)
end function

sub CNumLitParser.parse()
	parseNonDecimalPrefix()
	var begin = p
	parseBody()
	parseExponent()

	'' Floats with leading zeroes are decimal, not octal.
	if is_float and (numbase = 8) then
		numbase = 10
	end if

	if have_nonoct_digit and (numbase = 8) then
		errmsg = "invalid digit in octal number literal"
		return
	end if

	'' Save the number literal body (we don't want to include type suffixes here)
	var old = p[0]
	p[0] = 0  '' temporary null terminator
	ast = astNew(ASTKIND_CONSTI, cptr(zstring ptr, begin))
	p[0] = old

	ast->dtype = parseFloatTypeSuffixes()
	is_float or= (ast->dtype <> TYPE_NONE)

	if is_float then
		ast->kind = ASTKIND_CONSTF
		'' No float type suffix? Then default to double.
		if ast->dtype = TYPE_NONE then
			ast->dtype = TYPE_DOUBLE
		end if
	else
		var suffixes = parseIntTypeSuffixes()
		ast->dtype = getIntNumLitType(suffixes)
		select case numbase
		case 16 : ast->attrib or= ASTATTRIB_HEX
		case  8 : ast->attrib or= ASTATTRIB_OCT
		case  2 : ast->attrib or= ASTATTRIB_BIN
		end select
	end if

	'' Show error if we didn't reach the end of the number literal
	if p[0] <> 0 then
		errmsg = "invalid suffix on number literal: '" + *cptr(zstring ptr, p) + "'"
	end if
end sub

function hNumberLiteral _
	( _
		byref tk as TokenBuffer, _
		byval x as integer, _
		byval is_cpp as integer, _
		byref errmsg as string, _
		byval clong32 as integer _
	) as AstNode ptr
	dim parser as CNumLitParser = CNumLitParser(tk.getText(x), is_cpp, clong32)
	if len(parser.errmsg) > 0 then
		errmsg = parser.errmsg
		return NULL
	end if
	return parser.takeAst()
end function

private function hReadEscapeSequence(byref p as ubyte ptr, byref errmsg as string) as ulongint
	select case p[0]
	case CH_DQUOTE    : p += 1 : function = CH_DQUOTE     '' \"
	case CH_QUOTE     : p += 1 : function = CH_QUOTE      '' \'
	case CH_QUEST     : p += 1 : function = CH_QUEST      '' \?
	case CH_BACKSLASH : p += 1 : function = CH_BACKSLASH  '' \\
	case CH_L_A       : p += 1 : function = CH_BELL       '' \a
	case CH_L_B       : p += 1 : function = CH_BACKSPACE  '' \b
	case CH_L_F       : p += 1 : function = CH_FORMFEED   '' \f
	case CH_L_N       : p += 1 : function = CH_LF         '' \n
	case CH_L_R       : p += 1 : function = CH_CR         '' \r
	case CH_L_T       : p += 1 : function = CH_TAB        '' \t
	case CH_L_V       : p += 1 : function = CH_VTAB       '' \v

	'' \NNN (octal, max 3 digits)
	case CH_0 to CH_7
		dim as uinteger value
		var length = 0
		do
			value = (value shl 3) + (p[0] - asc("0"))
			length += 1
			p += 1
		loop while (p[0] >= CH_0) and (p[0] <= CH_7) and (length < 3)

		function = value

	'' \xNNNN... (hexadecimal, as many digits as possible)
	case CH_L_X
		dim as ulongint value
		var length = 0
		p += 1
		do
			dim as uinteger digit = p[0]

			select case digit
			case CH_0 to CH_9
				digit -= CH_0
			case CH_A to CH_F
				digit -= (CH_A - 10)
			case CH_L_A to CH_L_F
				digit -= (CH_L_A - 10)
			case else
				exit do
			end select

			value = (value shl 4) + digit

			length += 1
			p += 1
		loop

		function = value

	case else
		errmsg = "unknown escape sequence"
	end select
end function

'' C string literal parsing
''   "..."
''   L"..."
''   '.'
''   L'.'
'' String literals can contain escape sequences
function hStringLiteral _
	( _
		byref tk as TokenBuffer, _
		byval x as integer, _
		byval eval_escapes as integer, _
		byref errmsg as string _
	) as AstNode ptr

	assert((tk.get(x) = TK_STRING) or (tk.get(x) = TK_WSTRING) or _
	       (tk.get(x) = TK_CHAR  ) or (tk.get(x) = TK_WCHAR  ))
	dim as ubyte ptr p = tk.getText(x)

	var dtype = TYPE_ZSTRING
	if p[0] = CH_L then
		dtype = TYPE_WSTRING
		p += 1
	end if

	var astkind = ASTKIND_STRING
	var quotechar = p[0]
	if quotechar = CH_QUOTE then
		astkind = ASTKIND_CHAR
		dtype = iif(dtype = TYPE_ZSTRING, TYPE_BYTE, TYPE_WCHAR_T)
	end if
	p += 1

	'' Evaluate escape sequences and store the string literal's text into a buffer
	const MAXTEXTLEN = 1 shl 12
	'' +2 extra room to allow for some "overflowing" to reduce the amount
	'' of checking needed, +1 for null terminator.
	static text as zstring * MAXTEXTLEN+2+1 = any
	var j = 0  '' current write position in text buffer

	do
		select case p[0]
		case quotechar
			exit do

		case 0
			assert(FALSE)

		case CH_BACKSLASH '' \
			if eval_escapes then
				p += 1

				var value = hReadEscapeSequence(p, errmsg)
				if len(errmsg) > 0 then
					exit function
				end if

				select case value
				case is > &hFFu
					errmsg = "escape sequence value bigger than " & &hFFu & " (&hFF): " & value & " (&h" & hex(value) & ")"
					exit function

				'' Encode embedded nulls as "\0", and then also backslashes
				'' as "\\" to prevent ambiguity with the backslash in "\0".
				'' This allows the string literal content to still be
				'' represented as null-terminated string.
				case 0
					text[j] = CH_BACKSLASH : j += 1
					text[j] = CH_0         : j += 1
				case CH_BACKSLASH
					text[j] = CH_BACKSLASH : j += 1
					text[j] = CH_BACKSLASH : j += 1

				case else
					text[j] = value : j += 1
				end select
			else
				'' Store backslash as-is, not resolving escape sequences
				text[j] = CH_BACKSLASH : j += 1
				p += 1

				select case p[0]
				case CH_BACKSLASH '' \\
					text[j] = CH_BACKSLASH : j += 1
					p += 1
				case quotechar    '' \" or \'
					text[j] = quotechar : j += 1
					p += 1
				end select
			end if

		case else
			text[j] = p[0] : j += 1
			p += 1
		end select

		if j > MAXTEXTLEN then
			errmsg = "string literal too long, MAXTEXTLEN=" & MAXTEXTLEN
			exit function
		end if
	loop

	'' closing quote
	p += 1

	'' null-terminator
	text[j] = 0

	var n = astNew(astkind, text)
	n->dtype = dtype
	function = n
end function

'' MacroParameter =
''      Identifier         (named parameter)
''    | Identifier '...'   (named + variadic)
''    | '...'              (variadic, using __VA_ARGS__)
'' ('...' can only appear on the last parameter)
private sub hMacroParam(byref tk as TokenBuffer, byref x as integer, byval macro as AstNode ptr)
	'' Identifier?
	dim id as zstring ptr
	if tk.get(x) >= TK_ID then
		id = tk.spellId(x)
		x += 1
	end if

	'' Shouldn't have seen a '...' yet
	assert((macro->attrib and ASTATTRIB_VARIADIC) = 0)
	var maybevariadic = 0

	'' '...'?
	if tk.get(x) = TK_ELLIPSIS then
		x += 1
		maybevariadic = ASTATTRIB_VARIADIC
		if id = NULL then
			''    #define m(a, ...)
			'' must become
			''    #define m(a, __VA_ARGS__...)
			'' in FB, because in FB, the '...' parameter of variadic macros must always have a name,
			'' and using __VA_ARGS__ for that is the easiest solution, because then we don't need to
			'' go replacing that in the macro body.
			id = @"__VA_ARGS__"
		end if
	elseif id = NULL then
		tk.oopsExpected(x, "macro parameter (identifier or '...')")
	end if

	var param = astNew(ASTKIND_MACROPARAM, id)
	param->attrib or= maybevariadic
	macro->append(param)
	macro->attrib or= maybevariadic
	macro->paramcount += 1
end sub

'' <no whitespace> '(' MacroParameters ')'
private sub hMacroParamList(byref tk as TokenBuffer, byref x as integer, byval macro as AstNode ptr)
	assert(macro->paramcount = -1)

	'' '(' directly behind #define identifier, no spaces in between?
	if (tk.get(x) = TK_LPAREN) and ((tk.getFlags(x) and TKFLAG_BEHINDSPACE) = 0) then
		x += 1

		macro->paramcount = 0

		'' Not just '()'?
		if tk.get(x) <> TK_RPAREN then
			'' MacroParam (',' MacroParam)*
			do
				hMacroParam(tk, x, macro)

				'' ','?
				if tk.get(x) <> TK_COMMA then
					exit do
				end if
				x += 1
			loop
		end if

		'' ')'?
		tk.expect(x, TK_RPAREN, "to close the parameter list in this macro declaration")
		x += 1
	end if
end sub

function hDefineHead(byref tk as TokenBuffer, byref x as integer) as AstNode ptr
	'' Identifier? (keywords should be allowed to, so anything >= TK_ID)
	select case tk.get(x)
	case is < TK_ID
		tk.expect(x, TK_ID, "behind #define")
	case KW_DEFINED
		tk.showErrorAndAbort(x, "'defined' cannot be used as macro name")
	end select
	var macro = astNewPPDEFINE(tk.spellId(x))
	x += 1

	hMacroParamList(tk, x, macro)

	function = macro
end function

'' 1st field: C operator precedence, starting at 1, higher value = higher precedence
'' 2nd field: is it right associative? (assignments/iif)
dim shared copinfo(ASTKIND_CLOGOR to ASTKIND_IIF) as COperatorInfo = _
{ _
	( 4, FALSE), _ '' ASTKIND_CLOGOR
	( 5, FALSE), _ '' ASTKIND_CLOGAND
	( 0, FALSE), _ '' ASTKIND_LOGOR (unused)
	( 0, FALSE), _ '' ASTKIND_LOGAND (unused)
	( 6, FALSE), _ '' ASTKIND_OR
	( 7, FALSE), _ '' ASTKIND_XOR
	( 8, FALSE), _ '' ASTKIND_AND
	( 1, FALSE), _ '' ASTKIND_CCOMMA
	( 2, TRUE ), _ '' ASTKIND_CASSIGN
	( 2, TRUE ), _ '' ASTKIND_CSELFOR
	( 2, TRUE ), _ '' ASTKIND_CSELFXOR
	( 2, TRUE ), _ '' ASTKIND_CSELFAND
	( 2, TRUE ), _ '' ASTKIND_CSELFSHL
	( 2, TRUE ), _ '' ASTKIND_CSELFSHR
	( 2, TRUE ), _ '' ASTKIND_CSELFADD
	( 2, TRUE ), _ '' ASTKIND_CSELFSUB
	( 2, TRUE ), _ '' ASTKIND_CSELFMUL
	( 2, TRUE ), _ '' ASTKIND_CSELFDIV
	( 2, TRUE ), _ '' ASTKIND_CSELFMOD
	( 9, FALSE), _ '' ASTKIND_CEQ
	( 9, FALSE), _ '' ASTKIND_CNE
	(10, FALSE), _ '' ASTKIND_CLT
	(10, FALSE), _ '' ASTKIND_CLE
	(10, FALSE), _ '' ASTKIND_CGT
	(10, FALSE), _ '' ASTKIND_CGE
	( 0, FALSE), _ '' ASTKIND_EQ (unused)
	( 0, FALSE), _ '' ASTKIND_NE (unused)
	( 0, FALSE), _ '' ASTKIND_LT (unused)
	( 0, FALSE), _ '' ASTKIND_LE (unused)
	( 0, FALSE), _ '' ASTKIND_GT (unused)
	( 0, FALSE), _ '' ASTKIND_GE (unused)
	(11, FALSE), _ '' ASTKIND_SHL
	(11, FALSE), _ '' ASTKIND_SHR
	(12, FALSE), _ '' ASTKIND_ADD
	(12, FALSE), _ '' ASTKIND_SUB
	(13, FALSE), _ '' ASTKIND_MUL
	(13, FALSE), _ '' ASTKIND_DIV
	(13, FALSE), _ '' ASTKIND_MOD
	(15, FALSE), _ '' ASTKIND_INDEX
	(15, FALSE), _ '' ASTKIND_MEMBER
	(15, FALSE), _ '' ASTKIND_MEMBERDEREF
	( 0, FALSE), _ '' ASTKIND_STRCAT (unused)
	(14, FALSE), _ '' ASTKIND_CLOGNOT
	(14, FALSE), _ '' ASTKIND_NOT
	(14, FALSE), _ '' ASTKIND_NEGATE
	(14, FALSE), _ '' ASTKIND_UNARYPLUS
	( 0, FALSE), _ '' ASTKIND_CDEFINED (unused)
	( 0, FALSE), _ '' ASTKIND_DEFINED (unused)
	(14, FALSE), _ '' ASTKIND_ADDROF
	(14, FALSE), _ '' ASTKIND_DEREF
	( 0, FALSE), _ '' ASTKIND_STRINGIFY (unused)
	(14, FALSE), _ '' ASTKIND_SIZEOF
	(14, FALSE), _ '' ASTKIND_CAST
	( 3, TRUE )  _ '' ASTKIND_IIF
}

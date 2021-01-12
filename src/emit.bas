'' FB code generation, from AST

#include once "emit.bi"

#include once "chars.bi"
#include once "emit-fbkeywords.bi"
#include once "fbfrog.bi"
#include once "util.bi"

namespace emit

dim shared tokentext(0 to TK__COUNT-1) as zstring ptr => { _
	@"TK_EOF", _
	@"TK_EOL", _
	@!"\t", _
	@" ", _
	@"TK_NUMLIT", _
	@"TK_STRLIT", _
	@"TK_TEXT", _
	@"''", _
	_
	@":"  , _
	@","  , _
	@"...", _
	@"."  , _
	@"->" , _
	@"##" , _
	@"#"  , _
	@"@"  , _
	_
	@"+", _
	@"-", _
	@"*", _
	@"/", _
	_
	@"=" , _
	@"<>", _
	@"<" , _
	@"<=", _
	@">" , _
	@">=", _
	_
	@"(", _
	@")", _
	@"[", _
	@"]", _
	@"{", _
	@"}", _
	_
	@"TK_ID", _
	_
	@"alias"   , _
	@"and"     , _
	@"andalso" , _
	@"any"     , _
	@"as"      , _
	@"asc"     , _
	@"byte"    , _
	@"byref"   , _
	@"byval"   , _
	@"cast"    , _
	@"cbyte"   , _
	@"cdbl"    , _
	@"cdecl"   , _
	@"cint"    , _
	@"clng"    , _
	@"clngint" , _
	@"clong"   , _
	@"clongdouble", _
	@"const"   , _
	@"cptr"    , _
	@"cshort"  , _
	@"csng"    , _
	@"cubyte"  , _
	@"cuint"   , _
	@"culng"   , _
	@"culngint", _
	@"culong"  , _
	@"cushort" , _
	@"declare" , _
	@"define"  , _
	@"defined" , _
	@"dim"     , _
	@"do"      , _
	@"double"  , _
	@"else"    , _
	@"elseif"  , _
	@"end"     , _
	@"endif"   , _
	@"endmacro", _
	@"enum"    , _
	@"extern"  , _
	@"field"   , _
	@"function", _
	@"if"      , _
	@"ifdef"   , _
	@"ifndef"  , _
	@"iif"     , _
	@"import"  , _
	@"inclib"  , _
	@"include" , _
	@"integer" , _
	@"long"    , _
	@"longint" , _
	@"loop"    , _
	@"macro"   , _
	@"mod"     , _
	@"not"     , _
	@"once"    , _
	@"or"      , _
	@"orelse"  , _
	@"pragma"  , _
	@"private" , _
	@"ptr"     , _
	@"return"  , _
	@"scope"   , _
	@"shared"  , _
	@"shl"     , _
	@"short"   , _
	@"shr"     , _
	@"single"  , _
	@"sizeof"  , _
	@"static"  , _
	@"stdcall" , _
	@"sub"     , _
	@"then"    , _
	@"to"      , _
	@"type"    , _
	@"typeof"  , _
	@"ubyte"   , _
	@"uinteger", _
	@"ulong"   , _
	@"ulongint", _
	@"undef"   , _
	@"union"   , _
	@"ushort"  , _
	@"wchar_t" , _
	@"wend"    , _
	@"while"   , _
	@"wstr"    , _
	@"wstring" , _
	@"xor"     , _
	@"zstring"   _
}

destructor TokenBuffer()
	deallocate(p)
end destructor

function TokenBuffer.storePayload(byval payload as const zstring ptr) as ulong
	var i = strings.store(payload)
	if i > MaxPayload then
		oops("too many TokenBuffer payloads")
	end if
	return i
end function

function TokenBuffer.storePayload(byval payload as const ubyte ptr, byval size as uinteger) as ulong
	var i = strings.store(payload, size)
	if i > MaxPayload then
		oops("too many TokenBuffer payloads")
	end if
	return i
end function

sub TokenBuffer.add(byval tk as Token)
	if count = room then
		if room = 0 then
			room = 32
		else
			room *= 2
		end if
		p = reallocate(p, sizeof(*p) * room)
	end if
	p[count] = tk
	count += 1
end sub

sub TokenBuffer.add(byval typ as ulong, byval payload as const zstring ptr)
	add(type<Token>(typ, storePayload(payload)))
end sub

sub TokenBuffer.add(byval typ as ulong, byval payload as const ubyte ptr, byval size as uinteger)
	add(type<Token>(typ, storePayload(payload, size)))
end sub

sub TokenBuffer.removeLast()
	count -= 1
end sub

const function TokenBuffer.get(byval i as integer) as ulong
	if i >= 0 and i < count then
		return p[i].typ
	end if
	return TK_EOF
end function

sub CodeGen.add(byval typ as ulong, byval payload as const zstring ptr)
	tokens.add(typ, payload)
end sub

sub CodeGen.add(byval typ as ulong, byval payload as const ubyte ptr, byval size as uinteger)
	tokens.add(typ, payload, size)
end sub

dim shared as ushort typeToKw(0 to TYPE__COUNT-1) = { _
	TK_TEXT, KW_ANY, _
	KW_BYTE, KW_UBYTE, _
	KW_SHORT, KW_USHORT, _
	KW_LONG, KW_ULONG, _
	KW_CLONG, KW_CULONG, _
	KW_INTEGER, KW_UINTEGER, _
	KW_LONGINT, KW_ULONGINT, _
	KW_SINGLE, KW_DOUBLE, KW_CLONGDOUBLE, _
	TK_TEXT, TK_TEXT, _
	KW_ZSTRING, KW_WSTRING, KW_WCHAR_T _
}

sub CodeGen.emitType(byval dtype as integer, byval subtype as AstNode ptr)
	var dt = typeGetDt(dtype)
	assert(dt <> TYPE_NONE)
	var ptrcount = typeGetPtrCount(dtype)

	if typeIsConstAt(dtype, ptrcount) then
		add(KW_CONST)
		add(TK_SPACE)
	end if

	'' If it's a pointer to a function pointer, wrap it inside
	'' a typeof() to prevent the additional PTRs from being seen
	'' as part of the function pointer's result type:
	''    int (**p)(void)
	''    p as function() as integer ptr
	''    p as typeof(function() as integer) ptr
	'' (alternatively a typedef could be used)
	var add_typeof = (dt = TYPE_PROC) and (ptrcount >= 2)
	if add_typeof then
		add(KW_TYPEOF)
		add(TK_LPAREN)
	end if

	select case dt
	case TYPE_UDT
		assert(astIsTEXT(subtype))
		add(TK_ID, subtype->text)

	case TYPE_PROC
		if ptrcount >= 1 then
			'' The inner-most PTR on function pointers will be
			'' ignored below, but we still should preserve its CONST
			if typeIsConstAt(dtype, ptrcount - 1) then
				add(KW_CONST)
				add(TK_SPACE)
			end if
		else
			'' proc type but no pointers -- this is not supported in
			'' place of data types in FB, so here we add a DECLARE to
			'' indicate that it's not supposed to be a procptr type,
			'' but a plain proc type.
			add(KW_DECLARE)
			add(TK_SPACE)
		end if
		emitExpr(subtype)

	case TYPE_ZSTRING, TYPE_WSTRING
		add(typeToKw(dt))
		if subtype then
			add(TK_SPACE)
			add(TK_STAR)
			add(TK_SPACE)
			emitExpr(subtype)
		end if

	case else
		add(typeToKw(dt))
	end select

	if add_typeof then
		add(TK_RPAREN)
	end if

	'' Ignore most-inner PTR on function pointers -- in FB it's already
	'' implied by writing AS SUB|FUNCTION(...).
	if dt = TYPE_PROC then
		if ptrcount >= 1 then
			ptrcount -= 1
		end if
	end if

	for i as integer = (ptrcount - 1) to 0 step -1
		if typeIsConstAt(dtype, i) then
			add(TK_SPACE)
			add(KW_CONST)
		end if
		add(TK_SPACE)
		add(KW_PTR)
	next
end sub

sub CodeGen.emitType(byval n as AstNode ptr)
	emitType(n->dtype, n->subtype)
end sub

'' Normally we'll emit Extern blocks, making it unnecessary to worry about
'' case-preserving aliases, but symbols can still have an explicit alias set due
'' to symbol renaming.
sub CodeGen.emitAlias(byval n as AstNode ptr)
	if n->alias_ then
		add(TK_SPACE)
		add(KW_ALIAS)
		add(TK_SPACE)
		add(TK_STRLIT, """" + *n->alias_ + """")
	end if
end sub

sub CodeGen.emitIdAndArray(byval n as AstNode ptr, byval allow_alias as integer)
	add(TK_ID, n->text)
	if n->array then
		emitExpr(n->array)
	end if
	if allow_alias then
		emitAlias(n)
	end if
	if n->bits then
		add(TK_SPACE)
		add(TK_COLON)
		add(TK_SPACE)
		emitExpr(n->bits)
	end if
end sub

sub CodeGen.emitSeparatedList(byval n as AstNode ptr, byval skip_head as integer)
	var count = 0
	var i = n->head
	if (i <> NULL) and skip_head then
		i = i->nxt
	end if
	while i
		if count > 0 then
			add(TK_COMMA)
			add(TK_SPACE)
		end if
		emitExpr(i)
		count += 1
		i = i->nxt
	wend
end sub

sub CodeGen.emitParamList(byval n as AstNode ptr, byval skip_head as integer)
	add(TK_LPAREN)
	emitSeparatedList(n, skip_head)
	add(TK_RPAREN)
end sub

sub CodeGen.emitInitializer(byval n as AstNode ptr)
	if n->expr then
		add(TK_SPACE)
		add(TK_EQ)
		add(TK_SPACE)
		emitExpr(n->expr)
	end if
end sub

sub CodeGen.emitProcHeader(byval n as AstNode ptr, byval is_expr as integer)
	assert(n->array = NULL)

	'' SUB|FUNCTION [<id>]
	add(iif(n->dtype = TYPE_ANY, KW_SUB, KW_FUNCTION))
	if is_expr = FALSE then
		add(TK_SPACE)
		add(TK_ID, n->text)
	end if

	'' Calling convention not covered by Extern block?
	if (n->attrib and ASTATTRIB_HIDECALLCONV) = 0 then
		if n->attrib and ASTATTRIB_CDECL then
			assert((n->attrib and ASTATTRIB_STDCALL) = 0) '' can't have both
			add(TK_SPACE)
			add(KW_CDECL)
		elseif n->attrib and ASTATTRIB_STDCALL then
			add(TK_SPACE)
			add(KW_STDCALL)
		end if
	end if

	if is_expr = FALSE then
		emitAlias(n)
	end if

	'' '(' Parameters... ')'
	emitParamList(n, FALSE)

	'' Function result type
	if n->dtype <> TYPE_ANY then
		add(TK_SPACE)
		add(KW_AS)
		add(TK_SPACE)
		emitType(n)
	end if
end sub

'' Warn about types named after FB quirk keywords, e.g. winapi's INPUT, to avoid
'' fbc bug #730 (Using quirk keywords as identifier leads to parsing problems later)
sub CodeGen.emitTodoForQuirkKeywordType(byval id as zstring ptr)
	if id andalso (fbkeywords.lookup(id) = FBKW_QUIRK) then
		add(TK_SPACE)
		add(TK_TEXT, "'' TODO")
	end if
end sub

sub CodeGen.emitMacroHeader(byval n as AstNode ptr, byval macrokw as ulong)
	bol()
	add(TK_HASH)
	add(macrokw)
	add(TK_SPACE)
	add(TK_ID, n->text)
	if n->paramcount >= 0 then
		emitParamList(n, FALSE)
	end if
	add(TK_SPACE)
end sub

private function renderStrLit(byval payload as const zstring ptr) as string
	var s = """"

	'' Turn the string literal from the internal format into
	'' something nice for FB code
	var has_escapes = FALSE
	dim as const ubyte ptr i = payload
	do
		select case i[0]
		case 0
			exit do

		'' Internal format: can contain \\ and \0 escape
		'' sequences to encode embedded null chars
		case CH_BACKSLASH
			i += 1
			if i[0] = CH_0 then
				'' If a digit is following, then ensure it doesn't
				'' become part of this escape sequence
				'' (FB's \NNN decimal escape sequence is limited to 3 chars)
				select case i[1]
				case CH_0 to CH_9
					s += $"\000"
				case else
					s += $"\0"
				end select
			else
				assert(i[0] = CH_BACKSLASH)
				s += $"\\"
			end if
			has_escapes = TRUE

		case CH_BELL      : s += $"\a"  : has_escapes = TRUE
		case CH_BACKSPACE : s += $"\b"  : has_escapes = TRUE
		case CH_TAB       : s += $"\t"  : has_escapes = TRUE
		case CH_LF        : s += $"\n"  : has_escapes = TRUE
		case CH_VTAB      : s += $"\v"  : has_escapes = TRUE
		case CH_FORMFEED  : s += $"\f"  : has_escapes = TRUE
		case CH_CR        : s += $"\r"  : has_escapes = TRUE
		case CH_DQUOTE    : s += $""""""

		case is < 32, is >= 127
			var n = str(i[0])

			'' If a digit is following, then ensure it doesn't
			'' become part of this escape sequence
			'' (FB's \NNN decimal escape sequence is limited to 3 chars)
			select case i[1]
			case CH_0 to CH_9
				n = string(3 - len(n), "0") + n
			end select
			s += $"\" + n
			has_escapes = TRUE

		case else
			s += chr(i[0])
		end select

		i += 1
	loop

	s += """"

	if has_escapes then
		s = "!" + s
	end if

	return s
end function

sub CodeGen.emitExpr(byval n as AstNode ptr, byval need_parens as integer, byval need_macroparam_parens as integer)
	var consider_parens = FALSE
	select case as const n->kind
	case ASTKIND_VEROR, ASTKIND_VERAND, ASTKIND_VERNUMCHECK, _
	     ASTKIND_LOGOR, ASTKIND_LOGAND, _
	     ASTKIND_OR, ASTKIND_XOR, ASTKIND_AND, _
	     ASTKIND_EQ, ASTKIND_NE, ASTKIND_LT, _
	     ASTKIND_LE, ASTKIND_GT, ASTKIND_GE, _
	     ASTKIND_SHL, ASTKIND_SHR, _
	     ASTKIND_ADD, ASTKIND_SUB, ASTKIND_MUL, ASTKIND_DIV, ASTKIND_MOD, _
	     ASTKIND_NOT, ASTKIND_NEGATE, ASTKIND_UNARYPLUS, ASTKIND_ADDROF, ASTKIND_DEREF
		consider_parens = TRUE
	case ASTKIND_TEXT
		need_parens = need_macroparam_parens and ((n->attrib and ASTATTRIB_PARENTHESIZEDMACROPARAM) <> 0)
		consider_parens = TRUE
	end select
	var add_parens = consider_parens and need_parens

	if add_parens then
		add(TK_LPAREN)
	end if

	select case as const n->kind
	case ASTKIND_VEROR, ASTKIND_VERAND
		var i = n->head
		while i
			emitExpr(i, TRUE)
			if i->nxt then
				add(TK_SPACE)
				if astIsVEROR(n) then
					add(KW_OR)
				else
					add(KW_AND)
				end if
				add(TK_SPACE)
			end if
			i = i->nxt
		wend

	case ASTKIND_VERNUMCHECK
		add(TK_ID, frog.versiondefine)
		add(TK_SPACE)
		add(TK_EQ)
		add(TK_SPACE)
		add(TK_NUMLIT, frog.vernums(n->vernum))

	case ASTKIND_PROC
		emitProcHeader(n, TRUE)

	case ASTKIND_PARAM
		'' should have been solved out by hExpandArrayTypedef()
		assert(n->array = NULL)

		'' vararg?
		if n->dtype = TYPE_NONE then
			add(TK_ELLIPSIS)
		else
			if typeIsRef(n->dtype) then
				add(KW_BYREF)
			else
				add(KW_BYVAL)
			end if
			if n->text then
				add(TK_SPACE)
				add(TK_ID, n->text)
			end if
			add(TK_SPACE)
			add(KW_AS)
			add(TK_SPACE)
			emitType(n)
			emitInitializer(n)
		end if

	case ASTKIND_ARRAY
		emitParamList(n, FALSE)

	case ASTKIND_MACROPARAM
		add(TK_ID, n->text)
		if n->attrib and ASTATTRIB_VARIADIC then
			add(TK_ELLIPSIS)
		end if

	case ASTKIND_CONSTI, ASTKIND_CONSTF
		var s = hGetFbNumberLiteralPrefix(n->attrib)
		s += *n->text

		var need_rparen = FALSE
		select case typeGetDtAndPtr(n->dtype)
		case TYPE_CLONG, TYPE_CULONG
			add(KW_CAST)
			add(TK_LPAREN)
			add(iif(typeGetDtAndPtr(n->dtype) = TYPE_CLONG, KW_CLONG, KW_CULONG))
			add(TK_COMMA)
			add(TK_SPACE)
			need_rparen = TRUE
		case TYPE_LONGINT  : s += "ll"
		case TYPE_ULONGINT : s += "ull"
		case TYPE_ULONG, TYPE_UINTEGER : s += "u"
		case TYPE_SINGLE
			'' Always add suffix on SINGLEs, to ensure it's using the intended precision.
			s += "f"
		case TYPE_DOUBLE
			'' FB defaults to DOUBLE as long as there is a fractional part or exponent,
			'' so no type suffix is needed for that, as in C.
			if (instr(s, ".") = 0) andalso _
			   (instr(s, "e") = 0) andalso _
			   (instr(s, "E") = 0) then
				s += "d"
			end if
		end select

		add(TK_NUMLIT, s)

		if need_rparen then
			add(TK_RPAREN)
		end if

	case ASTKIND_TEXT
		add(iif(strIsValidSymbolId(n->text), TK_ID, TK_TEXT), n->text)

	case ASTKIND_STRING, ASTKIND_CHAR
		if n->kind = ASTKIND_CHAR then
			add(KW_ASC)
			add(TK_LPAREN)
		end if

		select case typeGetDtAndPtr(n->dtype)
		case TYPE_WSTRING, TYPE_WCHAR_T
			add(KW_WSTR)
			add(TK_LPAREN)
		end select

		add(TK_STRLIT, renderStrLit(n->text))

		select case typeGetDtAndPtr(n->dtype)
		case TYPE_WSTRING, TYPE_WCHAR_T
			add(TK_RPAREN)
		end select

		if n->kind = ASTKIND_CHAR then
			add(TK_RPAREN)
		end if

	case ASTKIND_LOGOR       : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(KW_ORELSE  ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_LOGAND      : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(KW_ANDALSO ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_OR          : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(KW_OR      ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_XOR         : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(KW_XOR     ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_AND         : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(KW_AND     ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_EQ          : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(TK_EQ      ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_NE          : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(TK_NE      ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_LT          : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(TK_LT      ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_LE          : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(TK_LE      ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_GT          : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(TK_GT      ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_GE          : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(TK_GE      ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_SHL         : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(KW_SHL     ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_SHR         : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(KW_SHR     ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_ADD         : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(TK_PLUS    ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_SUB         : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(TK_MINUS   ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_MUL         : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(TK_STAR    ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_DIV         : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(TK_SLASH   ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_MOD         : emitExpr(n->head, TRUE) : add(TK_SPACE) : add(KW_MOD     ) : add(TK_SPACE) : emitExpr(n->tail, TRUE)
	case ASTKIND_INDEX       : emitExpr(n->head, TRUE) :               : add(TK_LBRACKET) :               : emitExpr(n->tail, TRUE) : add(TK_RBRACKET)
	case ASTKIND_MEMBER      : emitExpr(n->head, TRUE) :               : add(TK_DOT     ) :               : emitExpr(n->tail, TRUE)
	case ASTKIND_MEMBERDEREF : emitExpr(n->head, TRUE) :               : add(TK_ARROW   ) :               : emitExpr(n->tail, TRUE)
	case ASTKIND_NOT       : add(KW_NOT  ) : add(TK_SPACE) : emitExpr(n->head, TRUE)
	case ASTKIND_NEGATE    : add(TK_MINUS) : emitExpr(n->head, TRUE)
	case ASTKIND_UNARYPLUS : add(TK_PLUS ) : emitExpr(n->head, TRUE)
	case ASTKIND_ADDROF    : add(TK_AT   ) : emitExpr(n->head, TRUE)
	case ASTKIND_DEREF     : add(TK_STAR ) : emitExpr(n->head, TRUE)
	case ASTKIND_STRINGIFY : add(TK_HASH ) : emitExpr(n->head)

	case ASTKIND_SIZEOF
		add(KW_SIZEOF)
		add(TK_LPAREN)
		emitExpr(n->head, FALSE, FALSE)
		add(TK_RPAREN)

	case ASTKIND_DEFINED
		add(KW_DEFINED)
		add(TK_LPAREN)
		add(TK_ID, n->text)
		add(TK_RPAREN)

	case ASTKIND_CAST
		var is_comma_list = FALSE
		select case n->dtype
		case TYPE_BYTE     : add(KW_CBYTE   ) : add(TK_LPAREN)
		case TYPE_UBYTE    : add(KW_CUBYTE  ) : add(TK_LPAREN)
		case TYPE_SHORT    : add(KW_CSHORT  ) : add(TK_LPAREN)
		case TYPE_USHORT   : add(KW_CUSHORT ) : add(TK_LPAREN)
		case TYPE_LONG     : add(KW_CLNG    ) : add(TK_LPAREN)
		case TYPE_ULONG    : add(KW_CULNG   ) : add(TK_LPAREN)
		case TYPE_INTEGER  : add(KW_CINT    ) : add(TK_LPAREN)
		case TYPE_UINTEGER : add(KW_CUINT   ) : add(TK_LPAREN)
		case TYPE_LONGINT  : add(KW_CLNGINT ) : add(TK_LPAREN)
		case TYPE_ULONGINT : add(KW_CULNGINT) : add(TK_LPAREN)
		case TYPE_SINGLE   : add(KW_CSNG    ) : add(TK_LPAREN)
		case TYPE_DOUBLE   : add(KW_CDBL    ) : add(TK_LPAREN)
		case else
			is_comma_list = TRUE
			add(iif(typeGetPtrCount(n->dtype) > 0, KW_CPTR, KW_CAST))
			add(TK_LPAREN)
			emitType(n)
			add(TK_COMMA)
			add(TK_SPACE)
		end select
		emitExpr(n->head, FALSE, is_comma_list)
		add(TK_RPAREN)

	case ASTKIND_IIF
		add(KW_IIF)
		add(TK_LPAREN)
		emitExpr(n->expr)
		add(TK_COMMA)
		add(TK_SPACE)
		emitExpr(n->head)
		add(TK_COMMA)
		add(TK_SPACE)
		emitExpr(n->tail)
		add(TK_RPAREN)

	case ASTKIND_STRCAT
		var i = n->head
		while i
			if i <> n->head then
				add(TK_SPACE)
			end if
			emitExpr(i)
			i = i->nxt
		wend

	case ASTKIND_PPMERGE
		var i = n->head
		while i
			if i <> n->head then
				add(TK_HASHHASH)
			end if
			emitExpr(i)
			i = i->nxt
		wend

	case ASTKIND_CALL
		emitExpr(n->head, TRUE, FALSE)
		emitParamList(n, TRUE)

	case ASTKIND_STRUCTINIT
		emitParamList(n, FALSE)

	case ASTKIND_ARRAYINIT
		add(TK_LBRACE)
		emitSeparatedList(n, FALSE)
		add(TK_RBRACE)

	case ASTKIND_DIMENSION
		add(TK_NUMLIT, "0")
		add(TK_SPACE)
		add(KW_TO)
		add(TK_SPACE)
		select case n->expr->kind
		case ASTKIND_ELLIPSIS
			add(TK_ELLIPSIS)
		case ASTKIND_CONSTI
			add(TK_NUMLIT, str(n->expr->evalConstiAsInt64() - 1))
		case else
			emitExpr(n->expr, TRUE)
			add(TK_SPACE)
			add(TK_MINUS)
			add(TK_SPACE)
			add(TK_NUMLIT, "1")
		end select

	case ASTKIND_DATATYPE
		emitType(n)

	case ASTKIND_ELLIPSIS
		add(TK_ELLIPSIS)

	case else
		n->dump()
		assert(FALSE)
	end select

	if add_parens then
		add(TK_RPAREN)
	end if
end sub

sub CodeGen.bol()
	assert(not have_bol)
	have_bol = TRUE
	if singleline > 0 then
		if singlelinebols > 0 then
			add(TK_SPACE)
			add(TK_COLON)
			add(TK_SPACE)
		end if
		singlelinebols += 1
		if comment > 0 then
			add(TK_TEXT, "/' ")
		end if
	else
		for i as integer = 1 to indent
			add(TK_TAB)
		next
		if comment > 0 then
			add(TK_COMMENT)
			for i as integer = 1 to commentspaces
				add(TK_SPACE)
			next
		end if
	end if
end sub

sub CodeGen.eol()
	assert(have_bol)
	have_bol = FALSE
	if singleline > 0 then
		if comment > 0 then
			add(TK_TEXT, " '/")
		end if
	else
		'' strip tabs/spaces at EOL
		'' (in case we added indentation in an otherwise empty line)
		do
			select case tokens.get(tokens.count - 1)
			case TK_TAB, TK_SPACE
			case else
				exit do
			end select
			tokens.removeLast()
		loop
		add(TK_EOL)
	end if
end sub

sub CodeGen.eolSingleLineBegin()
	assert(singlelinebols = 0)
	if singleline = 0 then
		assert(have_bol)
		have_bol = FALSE
	end if
	singleline += 1
end sub

sub CodeGen.eolSingleLineEnd()
	singleline -= 1
	if singleline = 0 then
		assert(not have_bol)
		have_bol = TRUE
		eol()
	end if
	singlelinebols = 0
end sub

sub CodeGen.emitLine(byval begin as const zstring ptr, byval p as const ubyte ptr)
	if begin <= p then
		bol()
		if begin < p then
			add(TK_TEXT, begin, cuint(p) - cuint(begin))
		end if
		eol()
	end if
end sub

'' Given text that contains newlines, emit every line individually
sub CodeGen.emitLines(byval lines as const zstring ptr)
	dim as const ubyte ptr p = lines
	dim as const zstring ptr begin = p

	do
		select case p[0]
		case 0
			'' EOF not behind EOL? Treat as EOL.
			emitLine(begin, p)
			exit do

		case CH_LF, CH_CR
			emitLine(begin, p)

			'' CRLF?
			if (p[0] = CH_CR) and (p[1] = CH_LF) then
				p += 1
			end if

			p += 1

			'' EOF after EOL? Don't emit another empty line.
			if p[0] = 0 then
				exit do
			end if

			begin = p

		case else
			p += 1
		end select
	loop
end sub

sub CodeGen.emitIndentedChildren(byval n as AstNode ptr, byval parentkind as integer = -1)
	if comment > 0 then
		commentspaces += 4
	else
		indent += 1
	end if

	var i = n->head
	while i
		emitCode(i, parentkind)
		i = i->nxt
	wend

	if comment > 0 then
		commentspaces -= 4
	else
		indent -= 1
	end if
end sub

sub CodeGen.emitVarDecl _
	( _
		byval kw1 as integer, _
		byval kw2 as integer, _
		byval spaces as integer, _
		byval n as AstNode ptr, _
		byval is_extern as integer _
	)

	bol()
	add(kw1)
	add(TK_SPACE)
	if kw2 >= 0 then
		add(kw2)
		add(TK_SPACE)
	end if
	for i as integer = 1 to spaces
		add(TK_SPACE)
	next
	if is_extern then
		if ((n->attrib and ASTATTRIB_EXTERN) <> 0) and _
		   ((n->attrib and ASTATTRIB_DLLIMPORT) <> 0) then
			add(KW_IMPORT)
			add(TK_SPACE)
		end if
	end if

	if typeIsRef(n->dtype) then
		add(KW_BYREF)
		add(TK_SPACE)
	end if

	emitIdAndArray(n, is_extern)
	add(TK_SPACE)
	add(KW_AS)
	add(TK_SPACE)
	emitType(n)

	if not is_extern then
		emitInitializer(n)
	end if
	eol()
end sub

sub CodeGen.emitSelfBop(byval n as AstNode ptr, byval op as ulong)
	bol()
	emitExpr(n->head, TRUE)
	add(TK_SPACE)
	add(op)
	add(TK_EQ)
	add(TK_SPACE)
	emitExpr(n->tail, FALSE)
	eol()
end sub

sub CodeGen.emitCode(byval n as AstNode ptr, byval parentkind as integer)
	var wrap_in_ifndef = ((n->attrib and ASTATTRIB_IFNDEFDECL) <> 0)

	if wrap_in_ifndef then
		assert(n->text)
		bol()
		add(TK_HASH)
		add(KW_IFNDEF)
		add(TK_SPACE)
		add(TK_ID, n->text)
		eol()
		indent += 1
	end if

	select case as const n->kind
	case ASTKIND_GROUP
		var i = n->head
		while i
			emitCode(i)
			i = i->nxt
		wend

	case ASTKIND_DIVIDER
		if (n->prev <> NULL) and (n->nxt <> NULL) then
			bol()
			eol()
		end if

	case ASTKIND_SCOPEBLOCK
		bol()
		add(KW_SCOPE)
		eol()

		emitIndentedChildren(n)

		bol()
		add(KW_END)
		add(TK_SPACE)
		add(KW_SCOPE)
		eol()

	case ASTKIND_UNKNOWN
		comment += 1
		commentspaces += 1
		emitLines("TODO: " + *n->text)
		commentspaces -= 1
		comment -= 1

	case ASTKIND_FBCODE
		emitLines(n->text)

	case ASTKIND_RENAMELIST
		var added_indent = FALSE
		if comment = 0 then
			added_indent = TRUE
			comment += 1
			commentspaces += 1
		end if
		bol() : add(TK_TEXT, n->text) : eol()
		emitIndentedChildren(n)
		if added_indent then
			commentspaces -= 1
			comment -= 1
		end if

	case ASTKIND_INCLIB
		bol()
		add(TK_HASH)
		add(KW_INCLIB)
		add(TK_SPACE)
		add(TK_STRLIT, """" + *n->text + """")
		eol()

	case ASTKIND_UNDEF
		bol()
		add(TK_HASH)
		add(KW_UNDEF)
		add(TK_SPACE)
		add(TK_ID, n->text)
		eol()

	case ASTKIND_PRAGMAONCE
		bol()
		add(TK_HASH)
		add(KW_PRAGMA)
		add(TK_SPACE)
		add(KW_ONCE)
		eol()

	case ASTKIND_PPINCLUDE
		bol()
		add(TK_HASH)
		add(KW_INCLUDE)
		add(TK_SPACE)
		add(KW_ONCE)
		add(TK_SPACE)
		add(TK_STRLIT, """" + *n->text + """")
		eol()

	case ASTKIND_PPDEFINE
		if n->expr then
			if n->expr->isCodeScopeBlock() then
				if n->expr->isScopeBlockWith1Stmt() then
					'' Emit macro body with scope block in single-line #define
					emitMacroHeader(n, KW_DEFINE)
					eolSingleLineBegin()
					emitCode(n->expr)
					eolSingleLineEnd()
				else
					emitMacroHeader(n, KW_MACRO)
					eol()
					indent += 1
					emitCode(n->expr)
					indent -= 1
					bol()
					add(TK_HASH)
					add(KW_ENDMACRO)
					eol()
				end if
			else
				emitMacroHeader(n, KW_DEFINE)
				emitExpr(n->expr, TRUE)
				eol()
			end if
		else
			emitMacroHeader(n, KW_DEFINE)
			eol()
		end if

	case ASTKIND_PPIF
		bol()
		add(TK_HASH)

		assert(n->expr)
		select case n->expr->kind
		'' #if defined(id)        ->    #ifdef id
		case ASTKIND_DEFINED
			add(KW_IFDEF)
			add(TK_SPACE)
			add(TK_ID, n->expr->text)

		'' #if not defined(id)    ->    #ifndef id
		case ASTKIND_NOT
			if n->expr->head->kind = ASTKIND_DEFINED then
				add(KW_IFNDEF)
				add(TK_SPACE)
				add(TK_ID, n->expr->head->text)
			end if
		end select

		'' Not handled above? Then emit the default '#if expr'
		if tokens.get(tokens.count - 1) = TK_HASH then
			add(KW_IF)
			add(TK_SPACE)
			emitExpr(n->expr)
		end if

		eol()

		emitIndentedChildren(n)

	case ASTKIND_PPELSEIF
		bol()
		add(TK_HASH)
		add(KW_ELSEIF)
		add(TK_SPACE)
		emitExpr(n->expr)
		eol()
		emitIndentedChildren(n)

	case ASTKIND_PPELSE
		bol()
		add(TK_HASH)
		add(KW_ELSE)
		eol()
		emitIndentedChildren(n)

	case ASTKIND_PPENDIF
		bol()
		add(TK_HASH)
		add(KW_ENDIF)
		eol()

	case ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM
		if (n->kind = ASTKIND_ENUM) and (n->text <> NULL) then
			bol()
			add(KW_TYPE)
			add(TK_SPACE)
			add(TK_ID, n->text)
			add(TK_SPACE)
			add(KW_AS)
			add(TK_SPACE)
			add(KW_LONG)
			emitTodoForQuirkKeywordType(n->text)
			eol()
		end if

		'' If it's a struct inside a struct, or union inside union,
		'' insert a union/struct in between respectively, to make it
		'' FB-compatible. FB only allows alternating types/unions when
		'' nesting.
		var opposite = iif(n->kind = ASTKIND_STRUCT, KW_UNION, KW_TYPE)
		if n->kind = parentkind then
			assert(parentkind <> ASTKIND_ENUM)
			bol()
			add(opposite)
			eol()
			indent += 1
		end if

		var compound = KW_TYPE
		select case n->kind
		case ASTKIND_UNION : compound = KW_UNION
		case ASTKIND_ENUM  : compound = KW_ENUM
		end select

		bol()
		add(compound)
		if (n->kind <> ASTKIND_ENUM) and (n->text <> NULL) then
			add(TK_SPACE)
			add(TK_ID, n->text)
		end if
		var fieldalign = 0
		if n->attrib and ASTATTRIB_PACKED then
			fieldalign = 1
		elseif n->maxalign > 0 then
			fieldalign = n->maxalign
		end if
		if fieldalign > 0 then
			add(TK_SPACE)
			add(KW_FIELD)
			add(TK_SPACE)
			add(TK_EQ)
			add(TK_SPACE)
			add(TK_NUMLIT, str(fieldalign))
		end if
		if (n->kind <> ASTKIND_ENUM) and (n->text <> NULL) then
			emitTodoForQuirkKeywordType(n->text)
		end if
		eol()

		emitIndentedChildren(n, n->kind)

		bol()
		add(KW_END)
		add(TK_SPACE)
		add(compound)
		eol()

		if n->kind = parentkind then
			indent -= 1
			bol()
			add(KW_END)
			add(TK_SPACE)
			add(opposite)
			eol()
		end if

	case ASTKIND_TYPEDEF
		assert(n->array = NULL)
		bol()
		add(KW_TYPE)
		add(TK_SPACE)
		add(TK_ID, n->text)
		add(TK_SPACE)
		add(KW_AS)
		add(TK_SPACE)
		emitType(n)
		emitTodoForQuirkKeywordType(n->text)
		eol()

	case ASTKIND_CONST
		bol()
		if (n->attrib and ASTATTRIB_ENUMCONST) = 0 then
			add(KW_CONST)
			add(TK_SPACE)
		end if
		add(TK_ID, n->text)
		emitInitializer(n)
		eol()

	case ASTKIND_VAR
		if n->attrib and ASTATTRIB_LOCAL then
			if n->attrib and ASTATTRIB_STATIC then
				emitVarDecl(KW_STATIC, -1, 0, n, FALSE)
			else
				emitVarDecl(KW_DIM, -1, 0, n, FALSE)
			end if
		else
			if n->attrib and ASTATTRIB_EXTERN then
				emitVarDecl(KW_EXTERN, -1, 0, n, TRUE)
			elseif n->attrib and ASTATTRIB_STATIC then
				emitVarDecl(KW_DIM, KW_SHARED, 0, n, FALSE)
			else
				emitVarDecl(KW_EXTERN, -1, len("dim shared") - len("extern"), n, TRUE)
				emitVarDecl(KW_DIM, KW_SHARED, 0, n, FALSE)
			end if
		end if

	case ASTKIND_FIELD
		'' Fields can be named after keywords, but we have to do
		''     as type foo
		'' instead of
		''     foo as type
		'' if foo has special meaning at the beginning of a statement in
		'' a TYPE block.
		var use_multdecl = FALSE

		select case lcase(*n->text, 1)
		case "as", "static", "dim", "redim", "const", "declare", _
		     "end", "type", "union", "enum", "rem", _
		     "public", "private", "protected"
			use_multdecl = TRUE
		end select

		bol()
		if not use_multdecl then
			emitIdAndArray(n, FALSE)
			add(TK_SPACE)
		end if
		add(KW_AS)
		add(TK_SPACE)
		emitType(n)
		if use_multdecl then
			add(TK_SPACE)
			emitIdAndArray(n, FALSE)
		end if
		eol()

	case ASTKIND_PROC
		bol()
		if n->expr then
			add(KW_PRIVATE) '' procedure bodies in headers should really be private
		else
			add(KW_DECLARE)
		end if
		add(TK_SPACE)
		emitProcHeader(n, FALSE)
		eol()

		'' Body
		if n->expr then
			assert(n->expr->kind = ASTKIND_SCOPEBLOCK)
			emitIndentedChildren(n->expr)
			bol()
			add(KW_END)
			add(TK_SPACE)
			add(iif(n->dtype = TYPE_ANY, KW_SUB, KW_FUNCTION))
			eol()
		end if

	case ASTKIND_EXTERNBLOCKBEGIN
		bol()
		add(KW_EXTERN)
		add(TK_SPACE)
		add(TK_STRLIT, """" + *n->text + """")
		eol()

	case ASTKIND_EXTERNBLOCKEND
		bol()
		add(KW_END)
		add(TK_SPACE)
		add(KW_EXTERN)
		eol()

	case ASTKIND_RETURN
		bol()
		add(KW_RETURN)
		if n->head then
			add(TK_SPACE)
			emitExpr(n->head)
		end if
		eol()

	case ASTKIND_ASSIGN
		bol()
		emitExpr(n->head, TRUE)
		add(TK_SPACE)
		add(TK_EQ)
		add(TK_SPACE)
		emitExpr(n->tail, FALSE)
		eol()
	case ASTKIND_SELFOR  : emitSelfBop(n, KW_OR)
	case ASTKIND_SELFXOR : emitSelfBop(n, KW_XOR)
	case ASTKIND_SELFAND : emitSelfBop(n, KW_AND)
	case ASTKIND_SELFSHL : emitSelfBop(n, KW_SHL)
	case ASTKIND_SELFSHR : emitSelfBop(n, KW_SHR)
	case ASTKIND_SELFADD : emitSelfBop(n, TK_PLUS)
	case ASTKIND_SELFSUB : emitSelfBop(n, TK_MINUS)
	case ASTKIND_SELFMUL : emitSelfBop(n, TK_STAR)
	case ASTKIND_SELFDIV : emitSelfBop(n, TK_SLASH)
	case ASTKIND_SELFMOD : emitSelfBop(n, KW_MOD)

	case ASTKIND_IFBLOCK
		var i = n->head

		assert(i->kind = ASTKIND_IFPART)
		bol()
		add(KW_IF)
		add(TK_SPACE)
		emitExpr(i->expr)
		add(TK_SPACE)
		add(KW_THEN)
		eol()
		emitIndentedChildren(i)

		do
			i = i->nxt
			if i = NULL then exit do

			if i->kind = ASTKIND_ELSEIFPART then
				bol()
				add(KW_ELSEIF)
				add(TK_SPACE)
				emitExpr(i->expr)
				add(TK_SPACE)
				add(KW_THEN)
				eol()
				emitIndentedChildren(i)
			else
				assert(i->kind = ASTKIND_ELSEPART)
				bol()
				add(KW_ELSE)
				eol()
				emitIndentedChildren(i)
			end if
		loop

		bol()
		add(KW_END)
		add(TK_SPACE)
		add(KW_IF)
		eol()

	case ASTKIND_DOWHILE
		bol()
		add(KW_DO)
		eol()

		emitIndentedChildren(n)

		bol()
		add(KW_LOOP)
		add(TK_SPACE)
		add(KW_WHILE)
		add(TK_SPACE)
		emitExpr(n->expr)
		eol()

	case ASTKIND_WHILE
		bol()
		add(KW_WHILE)
		add(TK_SPACE)
		emitExpr(n->expr)
		eol()

		emitIndentedChildren(n)

		bol()
		add(KW_WEND)
		eol()

	case else
		bol()
		emitExpr(n)
		eol()
	end select

	if wrap_in_ifndef then
		indent -= 1
		bol()
		add(TK_HASH)
		add(KW_ENDIF)
		eol()
	end if
end sub

sub CodeGen.emitHeader(byref header as HeaderInfo)
	comment += 1
	commentspaces += 1
	bol() : add(TK_TEXT, "FreeBASIC binding for " + header.title) : eol()
	bol() : eol()
	bol() : add(TK_TEXT, "based on the C header files:") : eol()
	commentspaces += 2
	emitLines(header.licensefile->text)
	commentspaces -= 2
	bol() : eol()
	bol() : add(TK_TEXT, "translated to FreeBASIC by:") : eol()
	commentspaces += 2
	emitLines(header.translatorsfile->text)
	commentspaces -= 2
	commentspaces -= 1
	comment -= 1
	bol() : eol()
end sub

sub TokenRenderer.render(byref tokens as const TokenBuffer)
	dim ln as string
	for i as integer = 0 to tokens.count - 1
		var tk = tokens.get(i)
		select case tk
		case TK_EOL
			emitLine(ln)
			ln = ""
		case TK_TEXT, TK_ID, TK_STRLIT, TK_NUMLIT
			ln += *tokens.strings.p[tokens.p[i].payload]
		case else
			ln += *tokentext(tk)
		end select
	next
	if (tokens.count > 0) andalso (tokens.get(tokens.count - 1) <> TK_EOL) then
		emitLine(ln)
	end if
end sub

constructor FileWriter(byref filename as const string)
	fo = freefile()
	if open(filename, for output, as #fo) then
		oops("could not open output file: '" + filename + "'")
	end if
end constructor

destructor FileWriter()
	close #fo
end destructor

sub FileWriter.emitLine(byref ln as const string)
	print #fo, ln
end sub

sub StdoutWriter.emitLine(byref ln as const string)
	print ln
end sub

sub StringWriter.emitLine(byref ln as const string)
	s += ln
end sub

end namespace

function emitFbType(byval dtype as integer, byval subtype as AstNode ptr) as string
	dim fbcode as emit.CodeGen
	fbcode.emitType(dtype, subtype)
	dim w as emit.StringWriter
	w.render(fbcode.tokens)
	return w.s
end function

function emitFbExpr(byval n as AstNode ptr) as string
	dim fbcode as emit.CodeGen
	fbcode.emitExpr(n)
	dim w as emit.StringWriter
	w.render(fbcode.tokens)
	return w.s
end function

sub emitFbFile(byref filename as string, byval header as HeaderInfo ptr, byval ast as AstNode ptr)
	dim fbcode as emit.CodeGen
	if header then
		fbcode.emitHeader(*header)
	end if
	fbcode.emitCode(ast)
	dim w as emit.FileWriter = emit.FileWriter(filename)
	w.render(fbcode.tokens)
end sub

sub emitFbStdout(byval ast as AstNode ptr, byval baseindent as integer)
	dim fbcode as emit.CodeGen
	fbcode.indent += baseindent
	fbcode.emitCode(ast)
	dim w as emit.StdoutWriter
	w.render(fbcode.tokens)
end sub

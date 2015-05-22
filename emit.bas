''
'' "AST to FB .bi file" emitter
''

#include once "fbfrog.bi"

declare sub emitCode(byval n as ASTNODE ptr, byval parentclass as integer = -1)

function emitType overload _
	( _
		byval dtype as integer, _
		byval subtype as ASTNODE ptr, _
		byval debugdump as integer _
	) as string

	static as zstring ptr datatypenames(0 to TYPE__COUNT-1) = _
	{ _
		@"none"    , _
		@"any"     , _
		@"byte"    , _
		@"ubyte"   , _
		@"short"   , _
		@"ushort"  , _
		@"long"    , _
		@"ulong"   , _
		@"clong"   , _
		@"culong"  , _
		@"integer" , _
		@"uinteger", _
		@"longint" , _
		@"ulongint", _
		@"single"  , _
		@"double"  , _
		@"clongdouble", _
		@"udt"     , _
		@"proc"    , _
		@"zstring" , _
		@"wstring" , _
		@"wchar_t"   _
	}

	dim as string s

	var dt = typeGetDt(dtype)
	var ptrcount = typeGetPtrCount(dtype)

	if typeIsConstAt(dtype, ptrcount) then
		s += "const "
	end if

	if debugdump then
		s += *datatypenames(dt)
	else
		'' If it's a pointer to a function pointer, wrap it inside
		'' a typeof() to prevent the additional PTRs from being seen
		'' as part of the function pointer's result type:
		''    int (**p)(void)
		''    p as function() as integer ptr
		''    p as typeof(function() as integer) ptr
		'' (alternatively a typedef could be used)
		var add_typeof = (dt = TYPE_PROC) and (ptrcount >= 2)
		if add_typeof then
			s += "typeof("
		end if

		select case dt
		case TYPE_UDT
			assert(astIsTEXT(subtype))
			s += *subtype->text

		case TYPE_PROC
			if ptrcount >= 1 then
				'' The inner-most PTR on function pointers will be
				'' ignored below, but we still should preserve its CONST
				if typeIsConstAt(dtype, ptrcount - 1) then
					s += "const "
				end if
			else
				'' proc type but no pointers -- this is not supported in
				'' place of data types in FB, so here we add a DECLARE to
				'' indicate that it's not supposed to be a procptr type,
				'' but a plain proc type.
				s += "declare "
			end if
			s += emitExpr(subtype)

		case TYPE_ZSTRING, TYPE_WSTRING
			s += *datatypenames(dt)
			if subtype then
				s += " * " + emitExpr(subtype)
			end if

		case else
			s += *datatypenames(dt)
		end select

		if add_typeof then
			s += ")"
		end if

		'' Ignore most-inner PTR on function pointers -- in FB it's already
		'' implied by writing AS SUB|FUNCTION(...).
		if dt = TYPE_PROC then
			if ptrcount >= 1 then
				ptrcount -= 1
			end if
		end if
	end if

	for i as integer = (ptrcount - 1) to 0 step -1
		if typeIsConstAt(dtype, i) then
			s += " const"
		end if

		s += " ptr"
	next

	function = s
end function

function emitType overload(byval n as ASTNODE ptr) as string
	function = emitType(n->dtype, n->subtype)
end function

namespace emit
	dim shared as integer fo, indent, comment, commentspaces, singleline, at_bol
end namespace

private sub emitLine(byref ln as string)
	dim s as string

	if emit.at_bol then
		'' Only add indentation if the line will contain more than that
		if (len(ln) > 0) or (emit.comment > 0) then
			s += string(emit.indent, !"\t")
		end if
	else
		if emit.singleline > 0 then
			s += " : "
		end if
	end if

	if emit.comment > 0 then
		if emit.singleline > 0 then
			s += "/'"
		else
			s += "''"
		end if
		if len(ln) > 0 then
			s += space(emit.commentspaces)
		end if
	end if

	s += ln

	if emit.comment > 0 then
		if emit.singleline > 0 then
			s += " '/"
		end if
	end if

	if emit.singleline > 0 then
		print #emit.fo, s;
		emit.at_bol = FALSE
	else
		print #emit.fo, s
		emit.at_bol = TRUE
	end if
end sub

private sub emitSingleLineBegin()
	emit.singleline += 1
end sub

private sub emitSingleLineEnd()
	emit.singleline -= 1
	if emit.singleline = 0 then
		print #emit.fo, ""
		emit.at_bol = TRUE
	end if
end sub

private sub hFlushLine(byval begin as zstring ptr, byval p as ubyte ptr)
	if begin > p then
		exit sub
	end if

	'' Insert a null terminator at EOL temporarily, so the current line
	'' text can be read from the string directly, instead of having to be
	'' copied out char-by-char.
	dim as integer old = p[0]
	p[0] = 0
	emitLine(*begin)
	p[0] = old
end sub

'' Given text that contains newlines, emit every line individually and prepend
'' indentation and/or <'' > if it's a comment.
private sub emitLines(byval lines as zstring ptr)
	dim as ubyte ptr p = lines
	dim as zstring ptr begin = p

	do
		select case p[0]
		case 0
			'' EOF not behind EOL? Treat as EOL.
			hFlushLine(begin, p)
			exit do

		case CH_LF, CH_CR
			hFlushLine(begin, p)

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

'' Normally we'll emit Extern blocks, making it unnecessary to worry about
'' case-preserving aliases, but symbols can still have an explicit alias set due
'' to symbol renaming.
private function hEmitAlias(byval n as ASTNODE ptr) as string
	if n->alias then
		function = " alias """ + *n->alias + """"
	end if
end function

private function hIdAndArray(byval n as ASTNODE ptr, byval allow_alias as integer) as string
	var s = *n->text
	if allow_alias then
		s += hEmitAlias(n)
	end if
	if n->array then
		s += emitExpr(n->array)
	end if
	if n->bits then
		s += " : " + emitExpr(n->bits)
	end if
	function = s
end function

private function hSeparatedList(byval n as ASTNODE ptr, byval separator as zstring ptr) as string
	dim s as string

	var count = 0
	var i = n->head
	while i
		if count > 0 then
			s += *separator
		end if

		s += emitExpr(i, FALSE)

		count += 1
		i = i->next
	wend

	function = s
end function

private function hParamList(byval n as ASTNODE ptr) as string
	function = "(" + hSeparatedList(n, ", ") + ")"
end function

private sub hEmitIndentedChildren(byval n as ASTNODE ptr, byval parentclass as integer = -1)
	if emit.comment > 0 then
		emit.commentspaces += 4
	else
		emit.indent += 1
	end if

	var i = n->head
	while i
		emitCode(i, parentclass)
		i = i->next
	wend

	if emit.comment > 0 then
		emit.commentspaces -= 4
	else
		emit.indent -= 1
	end if
end sub

private function hInitializer(byval n as ASTNODE ptr) as string
	if n->expr then
		function = " = " + emitExpr(n->expr)
	end if
end function

private sub emitVarDecl(byref prefix as string, byval n as ASTNODE ptr, byval is_extern as integer)
	var s = prefix

	if is_extern then
		if ((n->attrib and ASTATTRIB_EXTERN) <> 0) and _
		   ((n->attrib and ASTATTRIB_DLLIMPORT) <> 0) then
			s += "import "
		end if
	end if

	s += hIdAndArray(n, is_extern) + " as " + emitType(n)
	if is_extern = FALSE then s += hInitializer(n)

	emitLine(s)
end sub

private sub emitProc(byref s as string, byval n as ASTNODE ptr, byval is_expr as integer)
	assert(n->array = NULL)

	'' SUB|FUNCTION [<id>]
	s += iif(n->dtype = TYPE_ANY, "sub", "function")
	if is_expr = FALSE then
		s += " " + *n->text + hEmitAlias(n)
	end if

	'' Calling convention not covered by Extern block?
	if (n->attrib and ASTATTRIB_HIDECALLCONV) = 0 then
		if n->attrib and ASTATTRIB_CDECL then
			assert((n->attrib and ASTATTRIB_STDCALL) = 0) '' can't have both
			s += " cdecl"
		elseif n->attrib and ASTATTRIB_STDCALL then
			s += " stdcall"
		end if
	end if

	'' '(' Parameters... ')'
	s += hParamList(n)

	'' Function result type
	if n->dtype <> TYPE_ANY then
		s += " as " + emitType(n)
	end if
end sub

'' Warn about types named after FB quirk keywords, e.g. winapi's INPUT, to avoid
'' fbc bug #730 (Using quirk keywords as identifier leads to parsing problems later)
private function hCheckForQuirkKeywordType(byval id as zstring ptr) as string
	if id andalso (fbkeywordsLookup(id) = FBKW_QUIRK) then
		function = " '' TODO"
	end if
end function

private function hMacroBodyIsCodeBlock(byval n as ASTNODE ptr) as integer
	if n->expr then
		select case n->expr->class
		case ASTCLASS_SCOPEBLOCK, ASTCLASS_IFBLOCK, ASTCLASS_DOWHILE, _
		     ASTCLASS_WHILE
			function = TRUE
		end select
	end if
end function

private function hMacroBodyIsScopeBlockWith1Stmt(byval n as ASTNODE ptr) as integer
	function = (n->expr->class = ASTCLASS_SCOPEBLOCK) andalso _
	           astHas1Child(n->expr) andalso (not astIsCodeBlock(n->expr->head))
end function

private sub hMacroParamList(byref s as string, byval n as ASTNODE ptr)
	if n->paramcount >= 0 then
		s += hParamList(n)
	end if
end sub

private sub emitAssign(byval n as ASTNODE ptr, byval assignop as zstring ptr)
	emitLine(emitExpr(n->head, TRUE) + " " + *assignop + " " + emitExpr(n->tail, FALSE))
end sub

private sub emitCode(byval n as ASTNODE ptr, byval parentclass as integer)
	select case as const n->class
	case ASTCLASS_GROUP
		var i = n->head
		while i
			emitCode(i)
			i = i->next
		wend

	case ASTCLASS_DIVIDER
		if (n->prev <> NULL) and (n->next <> NULL) then
			emitLine("")
		end if

	case ASTCLASS_SCOPEBLOCK
		emitLine("scope")
		hEmitIndentedChildren(n)
		emitLine("end scope")

	case ASTCLASS_UNKNOWN
		emit.comment += 1
		emit.commentspaces += 1
		emitLines("TODO: " + *n->text)
		emit.commentspaces -= 1
		emit.comment -= 1

	case ASTCLASS_FBCODE
		emitLines(n->text)

	case ASTCLASS_RENAMELIST
		var added_indent = FALSE
		if emit.comment = 0 then
			added_indent = TRUE
			emit.comment += 1
			emit.commentspaces += 1
		end if
		emitLine(*n->text)
		hEmitIndentedChildren(n)
		if added_indent then
			emit.commentspaces -= 1
			emit.comment -= 1
		end if

	case ASTCLASS_INCLIB
		emitLine("#inclib """ + *n->text + """")

	case ASTCLASS_UNDEF
		emitLine("#undef " + *n->text)

	case ASTCLASS_PRAGMAONCE
		emitLine("#pragma once")

	case ASTCLASS_PPINCLUDE
		emitLine("#include once """ + *n->text + """")

	case ASTCLASS_PPDEFINE
		if hMacroBodyIsCodeBlock(n) then
			if hMacroBodyIsScopeBlockWith1Stmt(n) then
				emitSingleLineBegin()
				var s = "#define " + *n->text
				hMacroParamList(s, n)
				s += " scope"
				emitLine(s)
				emitCode(n->expr->head)
				emitLine("end scope")
				emitSingleLineEnd()
			else
				var s = "#macro " + *n->text
				hMacroParamList(s, n)
				emitLine(s)

				emit.indent += 1
				emitCode(n->expr)
				emit.indent -= 1

				emitLine("#endmacro")
			end if
		else
			var s = "#define " + *n->text
			hMacroParamList(s, n)
			if n->expr then
				s += " " + emitExpr(n->expr, TRUE)
			end if
			emitLine(s)
		end if

	case ASTCLASS_PPIF
		dim s as string
		assert(n->expr)
		select case n->expr->class
		'' #if defined(id)        ->    #ifdef id
		case ASTCLASS_DEFINED
			s = "#ifdef " + *n->expr->text

		'' #if not defined(id)    ->    #ifndef id
		case ASTCLASS_NOT
			if n->expr->head->class = ASTCLASS_DEFINED then
				s = "#ifndef " + *n->expr->head->text
			end if
		end select
		if len(s) = 0 then
			s = "#if " + emitExpr(n->expr)
		end if
		emitLine(s)

		hEmitIndentedChildren(n)

	case ASTCLASS_PPELSEIF
		emitLine("#elseif " + emitExpr(n->expr))
		hEmitIndentedChildren(n)

	case ASTCLASS_PPELSE
		emitLine("#else")
		hEmitIndentedChildren(n)

	case ASTCLASS_PPENDIF
		emitLine("#endif")

	case ASTCLASS_PPERROR
		emitLine("#error " + emitExpr(n->expr))

	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		var tail = hCheckForQuirkKeywordType(n->text)

		if (n->class = ASTCLASS_ENUM) and (n->text <> NULL) then
			emitLine("type " + *n->text + " as long" + tail)
		end if

		'' If it's a struct inside a struct, or union inside union,
		'' insert a union/struct in between respectively, to make it
		'' FB-compatible. FB only allows alternating types/unions when
		'' nesting.
		var opposite = iif(n->class = ASTCLASS_STRUCT, "union", "type")
		if n->class = parentclass then
			assert(parentclass <> ASTCLASS_ENUM)
			emitLine(opposite)
			emit.indent += 1
		end if

		dim as string compound
		select case n->class
		case ASTCLASS_UNION : compound = "union"
		case ASTCLASS_ENUM  : compound = "enum"
		case else           : compound = "type"
		end select

		var s = compound
		if (n->class <> ASTCLASS_ENUM) and (n->text <> NULL) then
			s += " " + *n->text
		end if
		if n->attrib and ASTATTRIB_PACKED then
			s += " field = 1"
		elseif n->maxalign > 0 then
			s += " field = " & n->maxalign
		end if
		if (n->class <> ASTCLASS_ENUM) and (n->text <> NULL) then
			s += tail
		end if
		emitLine(s)

		hEmitIndentedChildren(n, n->class)

		emitLine("end " + compound)

		if n->class = parentclass then
			emit.indent -= 1
			emitLine("end " + opposite)
		end if

	case ASTCLASS_TYPEDEF
		assert(n->array = NULL)
		emitLine("type " + *n->text + " as " + emitType(n) + hCheckForQuirkKeywordType(n->text))

	case ASTCLASS_CONST
		dim s as string
		if (n->attrib and ASTATTRIB_ENUMCONST) = 0 then
			s = "const "
		end if
		emitLine(s + *n->text + hInitializer(n))

	case ASTCLASS_VAR
		if n->attrib and ASTATTRIB_LOCAL then
			if n->attrib and ASTATTRIB_STATIC then
				emitVarDecl("static ", n, FALSE)
			else
				emitVarDecl("dim ", n, FALSE)
			end if
		else
			if n->attrib and ASTATTRIB_EXTERN then
				emitVarDecl("extern ", n, TRUE)
			elseif n->attrib and ASTATTRIB_STATIC then
				emitVarDecl("dim shared ", n, FALSE)
			else
				emitVarDecl("extern     ", n, TRUE)
				emitVarDecl("dim shared ", n, FALSE)
			end if
		end if

	case ASTCLASS_FIELD
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

		if use_multdecl then
			emitLine("as " + emitType(n) + " " + hIdAndArray(n, FALSE))
		else
			emitLine(hIdAndArray(n, FALSE) + " as " + emitType(n))
		end if

	case ASTCLASS_PROC
		dim s as string
		if n->expr then
			s += "private "  '' procedure bodies in headers should really be private
		else
			s += "declare "
		end if
		emitProc(s, n, FALSE)
		emitLine(s)

		'' Body
		if n->expr then
			assert(n->expr->class = ASTCLASS_SCOPEBLOCK)
			hEmitIndentedChildren(n->expr)
			emitLine("end " + iif(n->dtype = TYPE_ANY, "sub", "function"))
		end if

	case ASTCLASS_EXTERNBLOCKBEGIN
		emitLine("extern """ + *n->text + """")

	case ASTCLASS_EXTERNBLOCKEND
		emitLine("end extern")

	case ASTCLASS_RETURN
		var ln = "return"
		if n->head then
			ln += " " + emitExpr(n->head)
		end if
		emitLine(ln)

	case ASTCLASS_ASSIGN  : emitAssign(n, "=")
	case ASTCLASS_SELFOR  : emitAssign(n, "or=")
	case ASTCLASS_SELFXOR : emitAssign(n, "xor=")
	case ASTCLASS_SELFAND : emitAssign(n, "and=")
	case ASTCLASS_SELFSHL : emitAssign(n, "shl=")
	case ASTCLASS_SELFSHR : emitAssign(n, "shr=")
	case ASTCLASS_SELFADD : emitAssign(n, "+=")
	case ASTCLASS_SELFSUB : emitAssign(n, "-=")
	case ASTCLASS_SELFMUL : emitAssign(n, "*=")
	case ASTCLASS_SELFDIV : emitAssign(n, "/=")
	case ASTCLASS_SELFMOD : emitAssign(n, "mod=")

	case ASTCLASS_IFBLOCK
		var i = n->head

		assert(i->class = ASTCLASS_IFPART)
		emitLine("if " + emitExpr(i->expr) + " then")
		hEmitIndentedChildren(i)

		do
			i = i->next
			if i = NULL then exit do

			if i->class = ASTCLASS_ELSEIFPART then
				emitLine("elseif " + emitExpr(i->expr) + " then")
				hEmitIndentedChildren(i)
			else
				assert(i->class = ASTCLASS_ELSEPART)
				emitLine("else")
				hEmitIndentedChildren(i)
			end if
		loop

		emitLine("end if")

	case ASTCLASS_DOWHILE
		emitLine("do")
		hEmitIndentedChildren(n)
		emitLine("loop while " + emitExpr(n->expr))

	case ASTCLASS_WHILE
		emitLine("while " + emitExpr(n->expr))
		hEmitIndentedChildren(n)
		emitLine("wend")

	case else
		emitLine(emitExpr(n))
	end select
end sub

private function hEmitOperands(byval n as ASTNODE ptr, byref separator as string) as string
	dim s as string
	var i = n->head
	while i
		if i <> n->head then s += separator
		s += emitExpr(i)
		i = i->next
	wend
	function = s
end function

function emitExpr(byval n as ASTNODE ptr, byval need_parens as integer, byval need_macroparam_parens as integer) as string
	dim as string s

	if n = NULL then
		exit function
	end if

	var consider_parens = FALSE

	select case as const n->class
	case ASTCLASS_VEROR, ASTCLASS_VERAND
		var i = n->head
		while i
			s += emitExpr(i, TRUE)
			if i->next then
				if astIsVEROR(n) then
					s += " or "
				else
					s += " and "
				end if
			end if
			i = i->next
		wend
		consider_parens = TRUE

	case ASTCLASS_PROC
		emitProc(s, n, TRUE)

	case ASTCLASS_PARAM
		'' should have been solved out by hExpandArrayTypedef()
		assert(n->array = NULL)

		'' vararg?
		if n->dtype = TYPE_NONE then
			s += "..."
		else
			s += "byval"
			if n->text then
				s += " " + *n->text
			end if
			s += " as " + emitType(n)
			s += hInitializer(n)
		end if

	case ASTCLASS_ARRAY
		s += hParamList(n)

	case ASTCLASS_MACROPARAM
		s += *n->text
		if n->attrib and ASTATTRIB_VARIADIC then
			s += "..."
		end if

	case ASTCLASS_CONSTI, ASTCLASS_CONSTF
		dim as string suffix
		var need_rparen = FALSE

		select case typeGetDtAndPtr(n->dtype)
		case TYPE_CLONG
			s += "cast(clong, "
			need_rparen = TRUE
		case TYPE_CULONG
			s += "cast(culong, "
			need_rparen = TRUE
		case TYPE_LONGINT
			suffix = "ll"
		case TYPE_ULONGINT
			suffix = "ull"
		case TYPE_ULONG, TYPE_UINTEGER
			suffix = "u"
		case TYPE_SINGLE
			'' Always add suffix on SINGLEs, to ensure it's using
			'' the intended precision.
			suffix = "f"
		case TYPE_DOUBLE
			'' FB defaults to DOUBLE as long as there is a fractional part
			'' or exponent, so no type suffix is needed for that, as in C.
			if (instr(*n->text, ".") = 0) andalso _
			   (instr(*n->text, "e") = 0) andalso _
			   (instr(*n->text, "E") = 0) then
				suffix = "d"
			end if
		end select

		s += hGetFbNumberLiteralPrefix(n->attrib) + *n->text + suffix

		if need_rparen then
			s += ")"
		end if

	case ASTCLASS_TEXT
		s = *n->text
		if need_macroparam_parens and ((n->attrib and ASTATTRIB_PARENTHESIZEDMACROPARAM) <> 0) then
			s = "(" + s + ")"
		end if

	case ASTCLASS_STRING, ASTCLASS_CHAR
		s = """"

		'' Turn the string literal from the internal format into
		'' something nice for FB code
		var has_escapes = FALSE
		dim as ubyte ptr i = n->text
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

		select case typeGetDtAndPtr(n->dtype)
		case TYPE_WSTRING, TYPE_WCHAR_T
			s = "wstr(" + s + ")"
		end select

		if n->class = ASTCLASS_CHAR then
			s = "asc(" + s + ")"
		end if

	case ASTCLASS_LOGOR       : s = emitExpr(n->head, TRUE) + " orelse "  + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_LOGAND      : s = emitExpr(n->head, TRUE) + " andalso " + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_OR          : s = emitExpr(n->head, TRUE) + " or "      + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_XOR         : s = emitExpr(n->head, TRUE) + " xor "     + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_AND         : s = emitExpr(n->head, TRUE) + " and "     + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_EQ          : s = emitExpr(n->head, TRUE) + " = "       + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_NE          : s = emitExpr(n->head, TRUE) + " <> "      + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_LT          : s = emitExpr(n->head, TRUE) + " < "       + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_LE          : s = emitExpr(n->head, TRUE) + " <= "      + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_GT          : s = emitExpr(n->head, TRUE) + " > "       + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_GE          : s = emitExpr(n->head, TRUE) + " >= "      + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_SHL         : s = emitExpr(n->head, TRUE) + " shl "     + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_SHR         : s = emitExpr(n->head, TRUE) + " shr "     + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_ADD         : s = emitExpr(n->head, TRUE) + " + "       + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_SUB         : s = emitExpr(n->head, TRUE) + " - "       + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_MUL         : s = emitExpr(n->head, TRUE) + " * "       + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_DIV         : s = emitExpr(n->head, TRUE) + " / "       + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_MOD         : s = emitExpr(n->head, TRUE) + " mod "     + emitExpr(n->tail, TRUE) : consider_parens = TRUE
	case ASTCLASS_INDEX       : s = emitExpr(n->head, TRUE) + "["         + emitExpr(n->tail, TRUE) + "]"
	case ASTCLASS_MEMBER      : s = emitExpr(n->head, TRUE) + "."         + emitExpr(n->tail, TRUE)
	case ASTCLASS_MEMBERDEREF : s = emitExpr(n->head, TRUE) + "->"        + emitExpr(n->tail, TRUE)
	case ASTCLASS_NOT       : s = "not "     + emitExpr(n->head, TRUE) : consider_parens = TRUE
	case ASTCLASS_NEGATE    : s = "-"        + emitExpr(n->head, TRUE) : consider_parens = TRUE
	case ASTCLASS_UNARYPLUS : s = "+"        + emitExpr(n->head, TRUE) : consider_parens = TRUE
	case ASTCLASS_ADDROF    : s = "@"        + emitExpr(n->head, TRUE) : consider_parens = TRUE
	case ASTCLASS_DEREF     : s = "*"        + emitExpr(n->head, TRUE) : consider_parens = TRUE
	case ASTCLASS_STRINGIFY : s = "#"        + emitExpr(n->head)
	case ASTCLASS_SIZEOF    : s = "sizeof("  + emitExpr(n->head, FALSE, FALSE) + ")"
	case ASTCLASS_DEFINED   : s = "defined(" + *n->text + ")"
	case ASTCLASS_CAST
		var is_comma_list = FALSE
		select case n->dtype
		case TYPE_BYTE     : s =    "cbyte("
		case TYPE_UBYTE    : s =   "cubyte("
		case TYPE_SHORT    : s =   "cshort("
		case TYPE_USHORT   : s =  "cushort("
		case TYPE_LONG     : s =     "clng("
		case TYPE_ULONG    : s =    "culng("
		case TYPE_INTEGER  : s =     "cint("
		case TYPE_UINTEGER : s =    "cuint("
		case TYPE_LONGINT  : s =  "clngint("
		case TYPE_ULONGINT : s = "culngint("
		case TYPE_SINGLE   : s =     "csng("
		case TYPE_DOUBLE   : s =     "cdbl("
		case else
			is_comma_list = TRUE
			if typeGetPtrCount(n->dtype) > 0 then
				s = "cptr("
			else
				s = "cast("
			end if
			s += emitType(n) + ", "
		end select
		s += emitExpr(n->head, FALSE, is_comma_list) + ")"

	case ASTCLASS_IIF
		s = "iif(" + emitExpr(n->expr) + ", " + emitExpr(n->head) + ", " + emitExpr(n->tail) + ")"

	case ASTCLASS_STRCAT
		s = hEmitOperands(n, " ")

	case ASTCLASS_PPMERGE
		s = hEmitOperands(n, "##")

	case ASTCLASS_CALL
		s = *n->text + hParamList(n)

	case ASTCLASS_STRUCTINIT
		s = hParamList(n)

	case ASTCLASS_ARRAYINIT
		s = "{" + hSeparatedList(n, ", ") + "}"

	case ASTCLASS_DIMENSION
		s = "0 to "
		select case n->expr->class
		case ASTCLASS_ELLIPSIS
			s += "..."
		case ASTCLASS_CONSTI
			s &= astEvalConstiAsInt64(n->expr) - 1
		case else
			s += emitExpr(n->expr, TRUE) + " - 1"
		end select

	case ASTCLASS_DATATYPE
		s = emitType(n)

	case ASTCLASS_ELLIPSIS
		s = "..."

	case else
		astDump(n)
		assert(FALSE)
	end select

	if consider_parens and need_parens then
		s = "(" + s + ")"
	end if

	function = s
end function

private sub emitHeader(byref header as HeaderInfo)
	emit.comment += 1
	emit.commentspaces += 1

	emitLine("FreeBASIC binding for " + header.title)
	emitLine("")
	emitLine("based on the C header files:")
	emit.commentspaces += 2
	emitLines(header.licensefile->buffer)
	emit.commentspaces -= 2
	emitLine("")
	emitLine("translated to FreeBASIC by:")
	emit.commentspaces += 2
	emitLines(header.translatorsfile->buffer)
	emit.commentspaces -= 2

	emit.commentspaces -= 1
	emit.comment -= 1

	emitLine("")
end sub

sub emitFile(byref filename as string, byval header as HeaderInfo ptr, byval ast as ASTNODE ptr)
	emit.indent = 0
	emit.comment = 0
	emit.commentspaces = 0
	emit.singleline = 0
	emit.at_bol = TRUE

	emit.fo = freefile()
	if open(filename, for output, as #emit.fo) then
		oops("could not open output file: '" + filename + "'")
	end if

	if header then
		emitHeader(*header)
	end if

	emitCode(ast)

	close #emit.fo
end sub

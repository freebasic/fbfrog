#include once "clang-parser.bi"
#include once "util-str.bi"
#include once "c-lex.bi"

type ClangString
	s as CXString
	declare constructor(byval source as CXString)
	declare constructor(byref rhs as const ClangString) '' unimplemented
	declare operator let(byref rhs as const ClangString) '' unimplemented
	declare destructor()
	declare function value() as string
end type

constructor ClangString(byval source as CXString)
	s = source
end constructor

destructor ClangString()
	clang_disposeString(s)
end destructor

function ClangString.value() as string
	return *clang_getCString(s)
end function

private function wrapClangStr(byref s as CXString) as string
	dim wrapped as ClangString = ClangString(s)
	clear(s, 0, sizeof(s))
	return wrapped.value()
end function

constructor ClangContext(byref sourcectx as SourceContext, byref api as ApiInfo)
	this.sourcectx = @sourcectx
	this.api = @api
	index = clang_createIndex(0, 0)

	for i as integer = tktokens.KW__C_FIRST to tktokens.KW__C_LAST
		ckeywords.addOverwrite(tkInfoText(i), cast(any ptr, i))
	next

	fbfrog_c_parser = new CParser(fbfrog_tk, api)
end constructor

destructor ClangContext()
	delete fbfrog_c_parser
	clang_disposeTranslationUnit(unit)
	clang_disposeIndex(index)
end destructor

sub ClangContext.addArg(byval arg as const zstring ptr)
	args.append(strDuplicate(arg))
end sub

sub ClangContext.dumpArgs()
	var s = "libclang:"
	for i as integer = 0 to args.count - 1
		s += " " + *args.p[i]
	next
	print s
end sub

sub ClangContext.parseTranslationUnit()
	var e = _
		clang_parseTranslationUnit2(index, _
			NULL, args.p, args.count, _
			NULL, 0, _
			CXTranslationUnit_Incomplete or CXTranslationUnit_DetailedPreprocessingRecord, _
			@unit)

	if e <> CXError_Success then
		oops("libclang parsing failed with error code " & e)
	end if

	var diagcount = clang_getNumDiagnostics(unit)
	if diagcount > 0 then
		for i as integer = 0 to diagcount - 1
			print wrapClangStr(clang_formatDiagnostic(clang_getDiagnostic(unit, i), clang_defaultDiagnosticDisplayOptions()))
		next
		end 1
	end if
end sub

private function dumpTokenKind(byval kind as CXTokenKind) as string
	select case kind
	case CXToken_Comment     : return "Comment"
	case CXToken_Identifier  : return "Identifier"
	case CXToken_Keyword     : return "Keyword"
	case CXToken_Literal     : return "Literal"
	case CXToken_Punctuation : return "Punctuation"
	case else : return "Unknown(" & kind & ")"
	end select
end function

function ClangContext.dumpToken(byval token as CXToken) as string
	return dumpTokenKind(clang_getTokenKind(token)) + "[" + wrapClangStr(clang_getTokenSpelling(unit, token)) + "]"
end function

function ClangContext.dumpCursorTokens(byval cursor as CXCursor) as string
	dim buffer as CXToken ptr
	dim count as ulong
	clang_tokenize(unit, clang_getCursorExtent(cursor), @buffer, @count)

	dim s as string
	if count > 0 then
		for i as integer = 0 to count - 1
			if i > 0 then
				s += " "
			end if
			s += dumpToken(buffer[i])
		next
	end if

	clang_disposeTokens(unit, buffer, count)
	return s
end function

function ClangContext.locationFromClang(byval location as CXSourceLocation) as TkLocation
	dim file as CXFile
	dim linenum as ulong
	clang_getSpellingLocation(location, @file, @linenum, NULL, NULL)

	dim astloc as TkLocation

	if file then
		var filename = clang_getFileName(file)
		var filenamecstr = clang_getCString(filename)
		assert(filenamecstr)
		astloc.source = sourcectx->lookupOrMakeSourceInfo(filenamecstr, TRUE)
		clang_disposeString(filename)
	else
		astloc.source = sourcectx->lookupOrMakeSourceInfo("<built-in>", FALSE)
	end if

	astloc.linenum = linenum

	return astloc
end function

function ClangContext.locationFromClang(byval cursor as CXCursor) as TkLocation
	return locationFromClang(clang_getCursorLocation(cursor))
end function

function ClangContext.isBuiltIn(byval cursor as CXCursor) as integer
	dim file as CXFile
	clang_getSpellingLocation(clang_getCursorLocation(cursor), @file, NULL, NULL, NULL)
	return (file = NULL)
end function

function ClangContext.parseEvalResult(byval eval as CXEvalResult) as ASTNODE ptr
	var text = ""
	var kind = ASTKIND_CONSTI
	var evalkind = clang_EvalResult_getKind(eval)

	select case evalkind
	case CXEval_Int
		if clang_EvalResult_isUnsignedInt(eval) then
			text = str(clang_EvalResult_getAsUnsigned(eval))
		else
			text = str(clang_EvalResult_getAsLongLong(eval))
		end if

	case CXEval_Float
		text = str(clang_EvalResult_getAsDouble(eval))
		kind = ASTKIND_CONSTF

	case CXEval_StrLiteral
		text = *clang_EvalResult_getAsStr(eval)
		kind = ASTKIND_STRING

	case CXEval_UnExposed
		return NULL

	case else
		oops("unhandled eval result kind " & evalkind)
	end select

	return astNew(kind, text)
end function

function ClangContext.evaluateInitializer(byval cursor as CXCursor) as ASTNODE ptr
	var eval = clang_Cursor_Evaluate(cursor)
	var n = parseEvalResult(eval)
	clang_EvalResult_dispose(eval)
	return n
end function

sub ClangContext.parseLinkage(byref n as ASTNODE, byval cursor as CXCursor)
	var linkage = clang_getCursorLinkage(cursor)
	select case linkage
	case CXLinkage_NoLinkage
		n.attrib or= ASTATTRIB_LOCAL
	case CXLinkage_Internal
		n.attrib or= ASTATTRIB_STATIC
	case CXLinkage_External
		n.attrib or= ASTATTRIB_EXTERN
	case else
		oops("unhandled linkage kind " & linkage)
	end select
end sub

private function dumpSourceLocation(byval location as CXSourceLocation) as string
	dim filename as CXString
	dim as ulong linenum, column
	clang_getPresumedLocation(location, @filename, @linenum, @column)
	function = *clang_getCString(filename) + ":" & linenum & ":" & column
	clang_disposeString(filename)
end function

private function dumpClangType(byval ty as CXType) as string
	return ty.kind & ": " & wrapClangStr(clang_getTypeSpelling(ty))
end function

type ClangAstVisitor extends object
	declare abstract function visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult
	declare static function staticVisitor(byval cursor as CXCursor, byval parent as CXCursor, byval client_data as CXClientData) as CXChildVisitResult
	declare sub visitChildrenOf(byval cursor as CXCursor)
end type

function ClangAstVisitor.staticVisitor(byval cursor as CXCursor, byval parent as CXCursor, byval client_data as CXClientData) as CXChildVisitResult
	dim self as ClangAstVisitor ptr = client_data
	return self->visitor(cursor, parent)
end function

sub ClangAstVisitor.visitChildrenOf(byval cursor as CXCursor)
	clang_visitChildren(cursor, @staticVisitor, @this)
end sub

type ClangAstDumper extends ClangAstVisitor
	ctx as ClangContext ptr
	nestinglevel as integer
	declare constructor(byref ctx as ClangContext)
	declare function visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult override
	declare static function dumpOne(byval cursor as CXCursor) as string
	declare sub dump(byval cursor as CXCursor)
end type

constructor ClangAstDumper(byref ctx as ClangContext)
	this.ctx = @ctx
end constructor

function ClangAstDumper.visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult
	dump(cursor)
	return CXChildVisit_Continue
end function

function ClangAstDumper.dumpOne(byval cursor as CXCursor) as string
	var kind = wrapClangStr(clang_getCursorKindSpelling(clang_getCursorKind(cursor)))
	var spelling = wrapClangStr(clang_getCursorSpelling(cursor))
	var ty = clang_getCursorType(cursor)

	var s = kind + " " + spelling
	s += ": type[" & dumpClangType(ty) & "]"
	's += " from " + dumpSourceLocation(clang_getCursorLocation(cursor))

	if clang_getCursorKind(cursor) = CXCursor_StructDecl then
		if clang_equalCursors(cursor, clang_getTypeDeclaration(clang_getCursorType(cursor))) then
			s += " StructDecl is decl"
		else
			s += " StructDecl is not decl"
		end if
	end if

	return s
end function

sub ClangAstDumper.dump(byval cursor as CXCursor)
	if ctx->isBuiltIn(cursor) = false then
		print space(nestinglevel * 3) + dumpOne(cursor)
	end if
	nestinglevel += 1
	visitChildrenOf(cursor)
	nestinglevel -= 1
end sub

sub ClangContext.parseVariadicProc(byref proc as ASTNODE, byval ty as CXType)
	if clang_isFunctionTypeVariadic(ty) then
		astAppend(@proc, astNew(ASTKIND_PARAM))
	end if
end sub

sub ClangContext.parseCallConv(byref proc as ASTNODE, byval ty as CXType)
	var callconv = clang_getFunctionTypeCallingConv(ty)
	select case callconv
	case CXCallingConv_C
		proc.attrib or= ASTATTRIB_CDECL
	case CXCallingConv_X86StdCall
		proc.attrib or= ASTATTRIB_STDCALL
	case else
		oops("Unsupported callconv " & callconv)
	end select
end sub

sub ClangContext.parseClangFunctionType(byval ty as CXType, byref dtype as integer, byref subtype as ASTNODE ptr)
	var proc = astNew(ASTKIND_PROC)

	var resultty = clang_getResultType(ty)
	assert(resultty.kind <> CXType_Invalid)
	parseClangType(resultty, *proc)

	var paramcount = clang_getNumArgTypes(ty)
	for i as integer = 0 to paramcount - 1
		var param = astNew(ASTKIND_PARAM)
		parseClangType(clang_getArgType(ty, i), *param)
		astAppend(proc, param)
	next

	parseVariadicProc(*proc, ty)
	parseCallConv(*proc, ty)

	dtype = TYPE_PROC
	subtype = proc
end sub

function ClangContext.makeSymbolFromCursor(byval kind as integer, byval cursor as CXCursor) as ASTNODE ptr
	var n = astNew(kind, wrapClangStr(clang_getCursorSpelling(cursor)))
	parseClangType(clang_getCursorType(cursor), *n)
	n->location = locationFromClang(cursor)
	return n
end function

type RecordFieldCollector
	ctx as ClangContext ptr
	record as ASTNODE ptr
	declare constructor(byref ctx as ClangContext, byval record as ASTNODE ptr)
	declare operator let(byref as const RecordFieldCollector) '' unimplemented
	declare static function staticVisitor(byval cursor as CXCursor, byval client_data as CXClientData) as CXVisitorResult
	declare function visitor(byval cursor as CXCursor) as CXVisitorResult
	declare sub collectFieldsOf(byval ty as CXType)
end type

constructor RecordFieldCollector(byref ctx as ClangContext, byval record as ASTNODE ptr)
	this.ctx = @ctx
	this.record = record
end constructor

function RecordFieldCollector.staticVisitor(byval cursor as CXCursor, byval client_data as CXClientData) as CXVisitorResult
	dim self as RecordFieldCollector ptr = client_data
	return self->visitor(cursor)
end function

function RecordFieldCollector.visitor(byval cursor as CXCursor) as CXVisitorResult
	assert(clang_getCursorKind(cursor) = CXCursor_FieldDecl)
	astAppend(record, ctx->makeSymbolFromCursor(ASTKIND_FIELD, cursor))
	return CXChildVisit_Continue
end function

sub RecordFieldCollector.collectFieldsOf(byval ty as CXType)
	clang_Type_visitFields(ty, @staticVisitor, @this)
end sub

type EnumConstVisitor extends ClangAstVisitor
	ctx as ClangContext ptr
	enumbody as ASTNODE ptr
	declare constructor(byref ctx as ClangContext, byval enumbody as ASTNODE ptr)
	declare operator let(byref as const EnumConstVisitor) '' unimplemented
	declare function visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult override
end type

constructor EnumConstVisitor(byref ctx as ClangContext, byval enumbody as ASTNODE ptr)
	this.ctx = @ctx
	this.enumbody = enumbody
end constructor

function EnumConstVisitor.visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult
	if clang_getCursorKind(cursor) = CXCursor_EnumConstantDecl then
		var enumconst = ctx->makeSymbolFromCursor(ASTKIND_CONST, cursor)
		enumconst->attrib or= ASTATTRIB_ENUMCONST
		astAppend(enumbody, enumconst)
	end if
	return CXChildVisit_Continue
end function

private function isTagDecl(byval cursor as CXCursor) as integer
	select case clang_getCursorKind(cursor)
	case CXCursor_StructDecl, CXCursor_UnionDecl, CXCursor_EnumDecl
		return true
	case CXCursor_TypedefDecl
	case else
		assert(false)
	end select
	return false
end function

sub ClangContext.parseClangType(byval ty as CXType, byref dtype as integer, byref subtype as ASTNODE ptr)
	'' TODO: check ABI to ensure it's correct
	select case as const ty.kind
	case CXType_Void       : dtype = TYPE_ANY
	case CXType_Bool       : dtype = TYPE_BYTE
	case CXType_Char_S     : dtype = TYPE_BYTE
	case CXType_Char_U     : dtype = TYPE_UBYTE
	case CXType_UChar      : dtype = TYPE_UBYTE
	case CXType_UShort     : dtype = TYPE_USHORT
	case CXType_UInt       : dtype = TYPE_ULONG
	case CXType_ULong      : dtype = TYPE_CULONG
	case CXType_ULongLong  : dtype = TYPE_ULONGINT
	case CXType_SChar      : dtype = TYPE_BYTE
	case CXType_WChar      : dtype = TYPE_WCHAR_T
	case CXType_Short      : dtype = TYPE_SHORT
	case CXType_Int        : dtype = TYPE_LONG
	case CXType_Long       : dtype = TYPE_CLONG
	case CXType_LongLong   : dtype = TYPE_LONGINT
	case CXType_Float      : dtype = TYPE_SINGLE
	case CXType_Double     : dtype = TYPE_DOUBLE
	case CXType_LongDouble : dtype = TYPE_CLONGDOUBLE

	case CXType_Pointer
		dim pointee_dtype as integer
		parseClangType(clang_getPointeeType(ty), pointee_dtype, subtype)
		dtype = typeAddrOf(pointee_dtype)

	case CXType_Elaborated, CXType_Typedef
		var decl = clang_getTypeDeclaration(ty)
		dtype = TYPE_UDT
		subtype = astNewTEXT(wrapClangStr(clang_getCursorSpelling(decl)))
		if isTagDecl(decl) then
			subtype->attrib or= ASTATTRIB_TAGID
		end if

	case CXType_Record, CXType_Enum
		var decl = clang_getTypeDeclaration(ty)
		var tagbody = astNew(iif(ty.kind = CXType_Enum, ASTKIND_ENUM, ASTKIND_STRUCT), wrapClangStr(clang_getCursorSpelling(decl)))
		if ty.kind = CXType_Record then
			RecordFieldCollector(this, tagbody).collectFieldsOf(ty)
		else
			EnumConstVisitor(this, tagbody).visitChildrenOf(decl)
		end if
		dtype = TYPE_UDT
		subtype = tagbody

	case CXType_ConstantArray, CXType_IncompleteArray
		var arraytype = astNew(ASTKIND_ARRAY)
		parseClangType(clang_getArrayElementType(ty), *arraytype)

		dim size as ASTNODE ptr
		if ty.kind = CXType_IncompleteArray then
			size = astNew(ASTKIND_ELLIPSIS)
		else
			size = astNew(ASTKIND_CONSTI, str(clang_getArraySize(ty)))
			size->dtype = TYPE_INTEGER
		end if

		if arraytype->array = NULL then
			arraytype->array = astNew(ASTKIND_ARRAY)
		end if
		var d = astNew(ASTKIND_DIMENSION)
		d->expr = size
		astPrepend(arraytype->array, d)

		dtype = TYPE_ARRAY
		subtype = arraytype

	case CXType_FunctionProto
		parseClangFunctionType(ty, dtype, subtype)

	case CXType_Unexposed
		var resultty = clang_getResultType(ty)
		if resultty.kind <> CXType_Invalid then
			parseClangFunctionType(ty, dtype, subtype)
		else
			oops("unhandled clang type " + dumpClangType(ty))
		end if

	case else
		oops("unhandled clang type " + dumpClangType(ty))
	end select

	if clang_isConstQualifiedType(ty) then
		dtype = typeSetIsConst(dtype)
	end if
end sub

sub ClangContext.parseClangType(byval ty as CXType, byref n as ASTNODE)
	parseClangType(ty, n.dtype, n.subtype)
	if typeGetDt(n.dtype) = TYPE_ARRAY then
		if typeGetPtrCount(n.dtype) > 0 then
			oops("pointer to array, not supported")
		end if

		var arraytype = n.subtype
		n.subtype = NULL

		astSetType(@n, typeExpand(n.dtype, arraytype->dtype), arraytype->subtype)

		assert(n.array = NULL)
		n.array = arraytype->array
		arraytype->array = NULL

		astDelete(arraytype)
	end if
end sub

sub ClangContext.appendFbfrogToken(byref token as const CXToken)
	var clangkind = clang_getTokenKind(token)
	var spelling = wrapClangStr(clang_getTokenSpelling(unit, token))

	select case clangkind
	case CXToken_Identifier
		fbfrog_tk.insert(fbfrog_tk.count(), tktokens.TK_ID, spelling)

	case CXToken_Keyword
		var item = ckeywords.lookup(spelling, hashHash(spelling))
		if item->s = NULL then
			oops("unknown keyword " + spelling)
		end if
		fbfrog_tk.insert(fbfrog_tk.count(), cint(item->data))

	case CXToken_Literal, CXToken_Punctuation
		var x = fbfrog_tk.count()
		var y = lexLoadC(fbfrog_tk, x, spelling, locationFromClang(clang_getTokenLocation(unit, token)).source)
		assert(y = x + 1)

	case else
		oops("unhandled token kind " & clangkind)
	end select
end sub

sub ClangContext.appendFbfrogTokens(byval cursor as CXCursor)
	dim tokens as CXToken ptr
	dim tokencount as ulong
	clang_tokenize(unit, clang_getCursorExtent(cursor), @tokens, @tokencount)
	if tokencount > 0 then
		for i as integer = 0 to tokencount - 1
			appendFbfrogToken(tokens[i])
		next
	end if
	clang_disposeTokens(unit, tokens, tokencount)
end sub

function ClangContext.parseMacro(byval cursor as CXCursor) as ASTNODE ptr
	fbfrog_tk.insert(0, tktokens.TK_ID, "define")
	appendFbfrogTokens(cursor)
	fbfrog_tk.insert(fbfrog_tk.count(), tktokens.TK_EOL)
	fbfrog_c_parser->x = 0
	fbfrog_c_parser->parseok = TRUE
	function = fbfrog_c_parser->parseDefine()
end function

type ParamVisitor extends ClangAstVisitor
	proc as ASTNODE ptr
	param as ASTNODE ptr
	declare constructor(byval proc as ASTNODE ptr)
	declare operator let(byref as const ParamVisitor) '' unimplemented
	declare function visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult override
end type

constructor ParamVisitor(byval proc as ASTNODE ptr)
	this.proc = proc
	this.param = proc->head
end constructor

function ParamVisitor.visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult
	if clang_getCursorKind(cursor) = CXCursor_ParmDecl then
		if param = NULL then
			oops("found ParmDecl, but no param in the function's type")
		end if
		var spelling = clang_getCursorSpelling(cursor)
		var id = clang_getCString(spelling)
		if id[0] then
			astSetText(param, id[0])
		end if
		clang_disposeString(spelling)
		param = param->nxt
	end if
	return CXChildVisit_Continue
end function

type TranslationUnitParser extends ClangAstVisitor
	ctx as ClangContext ptr
	ast as AstBuilder
	declare constructor(byref ctx as ClangContext)
	declare operator let(byref as const TranslationUnitParser) '' unimplemented
	declare function visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult override
	declare sub parse(byval unit as CXTranslationUnit)
end type

constructor TranslationUnitParser(byref ctx as ClangContext)
	this.ctx = @ctx
end constructor

function TranslationUnitParser.visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult
	select case clang_getCursorKind(cursor)
	case CXCursor_VarDecl
		var n = ctx->makeSymbolFromCursor(ASTKIND_VAR, cursor)

		'' FIXME: function pointer types don't save the names of their params, so we'd have to
		'' parse the ParmDecls when they are there. However it's difficult because the ParmDecls
		'' of function pointer and its result type are saved at the same level.
		'' See AST dump of
		''  static void (*(*(*PA)(int A1, int A2))(int B1, int B2))(int C1, int C2);
		'if typeGetDtAndPtr(n->dtype) = TYPE_PROC then
		'	dim v as ParamVisitor = ParamVisitor(n->subtype)
		'	v.visitChildrenOf(cursor)
		'end if

		n->expr = ctx->evaluateInitializer(cursor)
		ctx->parseLinkage(*n, cursor)

		ast.takeAppend(n)

	case CXCursor_FunctionDecl
		'' Parse the function type
		dim dtype as integer
		dim proc as ASTNODE ptr
		ctx->parseClangType(clang_getCursorType(cursor), dtype, proc)

		assert(dtype = TYPE_PROC)
		astSetText(proc, wrapClangStr(clang_getCursorSpelling(cursor)))
		proc->location = ctx->locationFromClang(cursor)

		'' Collect parameter names and location
		'' (information that's only available from cursors, not from the type)
		var paramcount = clang_Cursor_getNumArguments(cursor)
		if paramcount > 0 then
			var param = proc->head
			for i as integer = 0 to paramcount - 1
				assert(param)
				var paramcursor = clang_Cursor_getArgument(cursor, i)
				astSetText(param, wrapClangStr(clang_getCursorSpelling(paramcursor)))
				param->location = ctx->locationFromClang(paramcursor)
				param = param->nxt
			next
			assert(param = NULL)
		else
			assert(proc->head = NULL)
		end if

		ctx->parseLinkage(*proc, cursor)

		ast.takeAppend(proc)

	case CXCursor_StructDecl, CXCursor_UnionDecl, CXCursor_EnumDecl
		'' Really the declaration with body?
		if clang_equalCursors(cursor, clang_getTypeDeclaration(clang_getCursorType(cursor))) then
			dim dtype as integer
			dim tagbody as ASTNODE ptr
			ctx->parseClangType(clang_getCursorType(cursor), dtype, tagbody)

			assert(dtype = TYPE_UDT)
			assert(*tagbody->text = wrapClangStr(clang_getCursorSpelling(cursor)))

			select case clang_getCursorKind(cursor)
			case CXCursor_UnionDecl
				tagbody->kind = ASTKIND_UNION
			case CXCursor_EnumDecl
				assert(tagbody->kind = ASTKIND_ENUM)
			end select
			tagbody->location = ctx->locationFromClang(cursor)

			if tagbody->head then
				ast.takeAppend(tagbody)
			else
				'' No fields; must be a forward declaration
				ctx->api->idopt(tktokens.OPT_ADDFORWARDDECL).addPattern(tagbody->text)
				astDelete(tagbody)
			end if
		end if

	case CXCursor_TypedefDecl
		var n = astNew(ASTKIND_TYPEDEF, wrapClangStr(clang_getCursorSpelling(cursor)))
		ctx->parseClangType(clang_getTypedefDeclUnderlyingType(cursor), *n)
		n->location = ctx->locationFromClang(cursor)

		ctx->fbfrog_c_parser->addTypedef(n->text)
		ast.takeAppend(n)

	case CXCursor_MacroDefinition
		if ctx->isBuiltIn(cursor) = false then
			var n = ctx->parseMacro(cursor)
			n->location = ctx->locationFromClang(cursor)
			ast.takeAppend(n)
		end if

	case CXCursor_InclusionDirective, CXCursor_MacroExpansion
		'' Ignore (clang expands all #includes anyways)

	case else
		oops("unhandled cursor kind " & clang_getCursorKind(cursor))
	end select

	return CXChildVisit_Continue
end function

sub TranslationUnitParser.parse(byval unit as CXTranslationUnit)
	visitChildrenOf(clang_getTranslationUnitCursor(unit))
end sub

function ClangContext.parseAst() as ASTNODE ptr
	'ClangAstDumper(this).dump(clang_getTranslationUnitCursor(unit))

	dim unitparser as TranslationUnitParser = TranslationUnitParser(this)
	unitparser.parse(unit)

	var t = unitparser.ast.takeTree()
	fbfrog_c_parser->processQueuedDefBodies(t)

	return t
end function

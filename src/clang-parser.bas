#include once "clang-parser.bi"
#include once "util-str.bi"

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

constructor ClangContext(byref sourcectx as SourceContext)
	this.sourcectx = @sourcectx
	index = clang_createIndex(0, 0)
end constructor

destructor ClangContext()
	clang_disposeTranslationUnit(unit)
	clang_disposeIndex(index)
end destructor

sub ClangContext.addArg(byval arg as const zstring ptr)
	args.append(strDuplicate(arg))
end sub

private function getClangTokenKindSpelling(byval kind as CXTokenKind) as string
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
	dim tokenstr as ClangString = ClangString(clang_getTokenSpelling(unit, token))
	return getClangTokenKindSpelling(clang_getTokenKind(token)) + "[" + tokenstr.value() + "]"
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

	var filename = clang_getFileName(file)

	dim astloc as TkLocation
	astloc.source = sourcectx->lookupOrMakeSourceInfo(clang_getCString(filename), TRUE)
	astloc.linenum = linenum

	clang_disposeString(filename)
	return astloc
end function

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
	nestinglevel as integer
	declare function visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult override
	declare function dumpOne(byval cursor as CXCursor) as string
	declare sub dump(byval cursor as CXCursor)
end type

function ClangAstDumper.visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult
	dump(cursor)
	return CXChildVisit_Continue
end function

function ClangAstDumper.dumpOne(byval cursor as CXCursor) as string
	var kind = wrapClangStr(clang_getCursorKindSpelling(clang_getCursorKind(cursor)))
	var spelling = wrapClangStr(clang_getCursorSpelling(cursor))
	var ty = clang_getCursorType(cursor)
	return kind + " " + spelling + ": type[" & dumpClangType(ty) & "]"
end function

sub ClangAstDumper.dump(byval cursor as CXCursor)
	print space(nestinglevel * 3) + dumpOne(cursor)
	nestinglevel += 1
	visitChildrenOf(cursor)
	nestinglevel -= 1
end sub

declare sub parseClangType(byval ty as CXType, byref dtype as integer, byref subtype as ASTNODE ptr)

private sub parseClangFunctionType(byval ty as CXType, byref dtype as integer, byref subtype as ASTNODE ptr)
	var proc = astNew(ASTKIND_PROC)

	var resultty = clang_getResultType(ty)
	assert(resultty.kind <> CXType_Invalid)
	parseClangType(resultty, proc->dtype, proc->subtype)

	var paramcount = clang_getNumArgTypes(ty)
	for i as integer = 0 to paramcount - 1
		var param = astNew(ASTKIND_PARAM)
		parseClangType(clang_getArgType(ty, i), param->dtype, param->subtype)
		astAppend(proc, param)
	next

	if clang_isFunctionTypeVariadic(ty) then
		astAppend(proc, astNew(ASTKIND_PARAM))
	end if

	dtype = TYPE_PROC
	subtype = proc
end sub

type FieldCollector extends ClangAstVisitor
	record as ASTNODE ptr
	declare constructor(byval record as ASTNODE ptr)
	declare operator let(byref as const FieldCollector) '' unimplemented
	declare function visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult override
end type

constructor FieldCollector(byval record as ASTNODE ptr)
	this.record = record
end constructor

function FieldCollector.visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult
	if clang_getCursorKind(cursor) = CXCursor_FieldDecl then
		var fld = astNew(ASTKIND_FIELD, wrapClangStr(clang_getCursorSpelling(cursor)))
		parseClangType(clang_getCursorType(cursor), fld->dtype, fld->subtype)
		astAppend(record, fld)
	end if
	return CXChildVisit_Continue
end function

private sub parseClangType(byval ty as CXType, byref dtype as integer, byref subtype as ASTNODE ptr)
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

	case CXType_Elaborated
		dtype = TYPE_UDT
		subtype = astNewTEXT(wrapClangStr(clang_getTypeSpelling(ty)))

	case CXType_Record
		var struct = astNew(ASTKIND_STRUCT, wrapClangStr(clang_getTypeSpelling(ty)))
		FieldCollector(struct).visitChildrenOf(clang_getTypeDeclaration(ty))
		dtype = TYPE_UDT
		subtype = struct

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
		var n = astNew(ASTKIND_VAR, wrapClangStr(clang_getCursorSpelling(cursor)))
		parseClangType(clang_getCursorType(cursor), n->dtype, n->subtype)

		'' FIXME: function pointer types don't save the names of their params, so we'd have to
		'' parse the ParmDecls when they are there. However it's difficult because the ParmDecls
		'' of function pointer and its result type are saved at the same level.
		'' See AST dump of
		''  static void (*(*(*PA)(int A1, int A2))(int B1, int B2))(int C1, int C2);
		'if typeGetDtAndPtr(n->dtype) = TYPE_PROC then
		'	dim v as ParamVisitor = ParamVisitor(n->subtype)
		'	v.visitChildrenOf(cursor)
		'end if

		ast.takeAppend(n)

	case CXCursor_FunctionDecl
		var proc = astNew(ASTKIND_PROC, wrapClangStr(clang_getCursorSpelling(cursor)))
		parseClangType(clang_getCursorResultType(cursor), proc->dtype, proc->subtype)

		var paramcount = clang_Cursor_getNumArguments(cursor)
		if paramcount > 0 then
			for i as integer = 0 to paramcount - 1
				var paramcursor = clang_Cursor_getArgument(cursor, i)
				var param = astNew(ASTKIND_PARAM, wrapClangStr(clang_getCursorSpelling(paramcursor)))
				parseClangType(clang_getCursorType(paramcursor), param->dtype, param->subtype)
				astAppend(proc, param)
			next
		end if

		ast.takeAppend(proc)

	case CXCursor_StructDecl, CXCursor_UnionDecl
		dim dtype as integer
		dim struct as ASTNODE ptr
		parseClangType(clang_getCursorType(cursor), dtype, struct)
		assert(dtype = TYPE_UDT)
		astSetText(struct, wrapClangStr(clang_getCursorSpelling(cursor)))
		if clang_getCursorKind(cursor) = CXCursor_UnionDecl then
			struct->kind = ASTKIND_UNION
		end if
		struct->location = ctx->locationFromClang(clang_getCursorLocation(cursor))
		ast.takeAppend(struct)

	end select

	return CXChildVisit_Continue
end function

sub TranslationUnitParser.parse(byval unit as CXTranslationUnit)
	visitChildrenOf(clang_getTranslationUnitCursor(unit))
end sub

function ClangContext.parseAst() as ASTNODE ptr
	print "libclang invocation args:";
	for i as integer = 0 to args.count - 1
		print " ";*args.p[i];
	next
	print

	var e = _
		clang_parseTranslationUnit2(index, _
			NULL, args.p, args.count, _
			NULL, 0, _
			CXTranslationUnit_Incomplete, _
			@unit)

	if e <> CXError_Success then
		oops("libclang parsing failed with error code " & e)
	end if

	var diagcount = clang_getNumDiagnostics(unit)
	if diagcount > 0 then
		for i as integer = 0 to diagcount - 1
			print wrapClangStr(clang_formatDiagnostic(clang_getDiagnostic(unit, i), clang_defaultDiagnosticDisplayOptions()))
		next
	end if

	ClangAstDumper().dump(clang_getTranslationUnitCursor(unit))

	dim unitparser as TranslationUnitParser = TranslationUnitParser(this)
	unitparser.parse(unit)
	return unitparser.ast.takeTree()
end function

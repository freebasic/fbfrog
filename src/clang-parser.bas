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

constructor ClangParser()
	index = clang_createIndex(0, 0)
end constructor

destructor ClangParser()
	clang_disposeTranslationUnit(unit)
	clang_disposeIndex(index)
end destructor

sub ClangParser.addArg(byval arg as const zstring ptr)
	args.append(strDuplicate(arg))
end sub

sub ClangParser.parseTranslationUnit()
	print "libclang invocation args:";
	for i as integer = 0 to args.count - 1
		print " ";*args.p[i];
	next
	print

	dim e as CXErrorCode = _
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

function ClangParser.dumpToken(byval token as CXToken) as string
	dim tokenstr as ClangString = ClangString(clang_getTokenSpelling(unit, token))
	return getClangTokenKindSpelling(clang_getTokenKind(token)) + "[" + tokenstr.value() + "]"
end function

function ClangParser.dumpCursorTokens(byval cursor as CXCursor) as string
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

private function functionDeclVisitor(byval cursor as CXCursor, byval parent as CXCursor, byval client_data as CXClientData) as CXChildVisitResult
	var kind = clang_getCursorKind(cursor)
	var ty = clang_getCursorType(cursor)

	select case kind
	case CXCursor_ParmDecl
		var id = wrapClangStr(clang_getCursorSpelling(cursor))
		print "param: " & id & ", type " & ty.kind & " " & wrapClangStr(clang_getTypeSpelling(ty))
		dim pparamcount as integer ptr = client_data
		*pparamcount += 1
	end select

	return CXChildVisit_Continue
end function

private function cursorVisitor(byval cursor as CXCursor, byval parent as CXCursor, byval client_data as CXClientData) as CXChildVisitResult
	var kind = clang_getCursorKind(cursor)
	var spelling = wrapClangStr(clang_getCursorSpelling(cursor))

	select case kind
	case CXCursor_FunctionDecl
		print "FunctionDecl " & spelling

		dim paramcount as integer
		clang_visitChildren(cursor, @functionDeclVisitor, @paramcount)
		print paramcount & " params"
		print "source: " & dumpSourceLocation(clang_getCursorLocation(cursor))
		function = CXChildVisit_Continue

	case else
		function = CXChildVisit_Recurse
	end select
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

	case else
		oops("unhandled clang type " + dumpClangType(ty))
	end select

	if clang_isConstQualifiedType(ty) then
		dtype = typeSetIsConst(dtype)
	end if
end sub

type TranslationUnitParser extends ClangAstVisitor
	ast as AstBuilder
	declare operator let(byref as const TranslationUnitParser) '' unimplemented
	declare function visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult override
	declare sub parse(byval unit as CXTranslationUnit)
end type

function TranslationUnitParser.visitor(byval cursor as CXCursor, byval parent as CXCursor) as CXChildVisitResult
	select case clang_getCursorKind(cursor)
	case CXCursor_VarDecl
		var n = astNew(ASTKIND_VAR, wrapClangStr(clang_getCursorSpelling(cursor)))
		parseClangType(clang_getCursorType(cursor), n->dtype, n->subtype)
		ast.takeAppend(n)
	end select
	return CXChildVisit_Continue
end function

sub TranslationUnitParser.parse(byval unit as CXTranslationUnit)
	visitChildrenOf(clang_getTranslationUnitCursor(unit))
end sub

function ClangParser.parseAst() as ASTNODE ptr
	ClangAstDumper().dump(clang_getTranslationUnitCursor(unit))
	dim unitparser as TranslationUnitParser
	unitparser.parse(unit)
	return unitparser.ast.takeTree()
end function

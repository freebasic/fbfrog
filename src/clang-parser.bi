#include once "ast.bi"
#include once "util.bi"

#include once "clang-c.bi"

type ClangContext
	index as CXIndex
	unit as CXTranslationUnit
	args as DynamicArray(const_zstring_ptr)

	sourcectx as SourceContext ptr

	declare constructor(byref sourcectx as SourceContext)
	declare destructor()
	declare operator let(byref as const ClangContext) '' unimplemented

	declare sub addArg(byval arg as const zstring ptr)

	declare function dumpToken(byval token as CXToken) as string
	declare function dumpCursorTokens(byval cursor as CXCursor) as string

	declare function locationFromClang(byval location as CXSourceLocation) as TkLocation
	declare function locationFromClang(byval cursor as CXCursor) as TkLocation
	declare function isBuiltIn(byval cursor as CXCursor) as integer
	declare function parseEvalResult(byval eval as CXEvalResult) as ASTNODE ptr
	declare function evaluateInitializer(byval cursor as CXCursor) as ASTNODE ptr
	declare sub parseLinkage(byref n as ASTNODE, byval cursor as CXCursor)
	declare sub parseVariadicProc(byref proc as ASTNODE, byval ty as CXType)
	declare sub parseCallConv(byref proc as ASTNODE, byval ty as CXType)
	declare sub parseClangFunctionType(byval ty as CXType, byref dtype as integer, byref subtype as ASTNODE ptr)
	declare function makeSymbolFromCursor(byval kind as integer, byval cursor as CXCursor) as ASTNODE ptr
	declare sub parseClangType(byval ty as CXType, byref dtype as integer, byref subtype as ASTNODE ptr)

	declare function parseAst() as ASTNODE ptr
end type

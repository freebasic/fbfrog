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
	declare function parseAst() as ASTNODE ptr
end type

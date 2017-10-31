#include once "util.bi"
#include once "clang-c.bi"

type ClangParser
	index as CXIndex
	unit as CXTranslationUnit
	args as DynamicArray(const_zstring_ptr)

	declare constructor()
	declare destructor()
	declare operator let(byref as const ClangParser) '' unimplemented
	declare sub addArg(byval arg as const zstring ptr)
	declare sub parseTranslationUnit()
end type

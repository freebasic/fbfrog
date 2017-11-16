#include once "ast.bi"
#include once "util.bi"
#include once "tk.bi"
#include once "options.bi"
#include once "c-parser.bi"

#include once "clang-c.bi"

type TempIdManager
private:
	'' Counter for producing temporary identifiers, for non-nested anonymous structs/unions
	count as integer

	'' Map clang type spelling => temporary identifier, for non-nested anonymous structs/unions
	'' The clang type spelling seems to be unique, for example "struct (anonymous at a.h:6:8)",
	'' so it should be safe to use it as key.
	'' This map is needed because CXType_Record and CXType_Elaborated appear separately even for
	'' anonymous ones. The temp id is generated for the CXType_Record, and must be looked up for the CXType_Elaborated.
	anontagtable as THash = THash(10, true) '' key = zstring owned by anontagtable, data = FB string owned by tempidlist
	tempidlist as DynamicArray(string)

public:
	declare operator let(byref as const TempIdManager) '' unimplemented
	declare sub add(byref typespelling as const string, byref tempid as const string)
	declare function makeNext(byref typespelling as const string) as string
	declare function lookup(byref typespelling as const string) as const string ptr
end type

type ClangContext
	index as CXIndex
	unit as CXTranslationUnit
	args as DynamicArray(const_zstring_ptr)

	sourcectx as SourceContext ptr
	options as BindingOptions ptr

	ckeywords as THash = THash(12)
	fbfrog_tk as TokenBuffer
	fbfrog_c_parser as CParser ptr

	tempids as TempIdManager

	declare constructor(byref sourcectx as SourceContext, byref options as BindingOptions)
	declare destructor()
	declare operator let(byref as const ClangContext) '' unimplemented

	declare sub addArg(byval arg as const zstring ptr)
	declare sub dumpArgs()

	declare sub parseTranslationUnit()

	declare function dumpToken(byval token as CXToken) as string
	declare function dumpCursorTokens(byval cursor as CXCursor) as string

	declare function locationFromClang(byval location as CXSourceLocation) as TkLocation
	declare function locationFromClang(byval cursor as CXCursor) as TkLocation
	declare function isBuiltIn(byval cursor as CXCursor) as integer
	declare function parseEvalResult(byval eval as CXEvalResult) as ASTNODE ptr
	declare function evaluateInitializer(byval cursor as CXCursor) as ASTNODE ptr
	declare function parseEnumConstValue(byval cursor as CXCursor, byval parent as CXCursor) as ASTNODE ptr
	declare sub parseLinkage(byref n as ASTNODE, byval cursor as CXCursor)
	declare sub parseVariadicProc(byref proc as ASTNODE, byval ty as CXType)
	declare sub parseCallConv(byref proc as ASTNODE, byval ty as CXType)
	declare sub parseClangFunctionType(byval ty as CXType, byref dtype as integer, byref subtype as ASTNODE ptr)
	declare function makeSymbolFromCursor(byval kind as integer, byval cursor as CXCursor) as ASTNODE ptr
	declare sub parseClangType(byval ty as CXType, byref dtype as integer, byref subtype as ASTNODE ptr)
	declare sub parseClangType(byval ty as CXType, byref n as ASTNODE)
	declare sub appendFbfrogToken(byref token as const CXToken)
	declare sub appendFbfrogTokens(byval cursor as CXCursor)
	declare function parseMacro(byval cursor as CXCursor) as ASTNODE ptr

	declare function parseAst() as ASTNODE ptr

	declare sub inclusionVisitor(byval incfile as CXFile, byval stack as CXSourceLocation ptr, byval stackcount as ulong)
	declare static sub staticInclusionVisitor _
		( _
			byval incfile as CXFile, _
			byval stack as CXSourceLocation ptr, _
			byval stackcount as ulong, _
			byval client_data as CXClientData _
		)
	declare sub parseInclusions()
end type

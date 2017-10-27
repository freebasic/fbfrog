#include once "ast.bi"
#include once "util-str.bi"

type ParentChildPattern
	parentpattern as string
	childpattern as string
	declare function matches(byval parent as AstNode ptr, byval child as AstNode ptr) as integer
end type

type IndexPattern
	parentpattern as string
	childindex as integer
	declare function matches(byval parentid as zstring ptr, byval childindex as integer) as integer
end type

type DeclPatterns
	'' id patterns
	ids as StringMatcher

	'' parent.child patterns
	pcParents as StringMatcher
	pcChildren as StringMatcher
	pcs as ParentChildPattern ptr
	pccount as integer

	'' index patterns
	indexParents as StringMatcher
	index as IndexPattern ptr
	indexcount as integer

	declare sub parseAndAdd(byref s as string)
	declare destructor()
	declare function matches _
		( _
			byval parentparent as AstNode ptr, _
			byval parent as AstNode ptr, _
			byval child as AstNode ptr, _
			byval childindex as integer _
		) as integer
	declare operator let(byref as const DeclPatterns) '' unimplemented
end type

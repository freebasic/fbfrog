#include once "util.bi"
#include once "util-hash.bi"
#include once "tk.bi"

type LexContext
	sourcectx as SourceContext ptr
	tk as TokenBuffer ptr

	i		as ubyte ptr  '' Current char, will always be <= limit
	x		as integer
	location	as DecodedLocation
	behindspace	as integer

	keywords as THash = THash(12)

	declare operator let(byref as const LexContext) '' unimplemented
	declare sub initKeywords(byval first as integer, byval last as integer)
	declare function lookupKeyword(byval id as zstring ptr, byval defaulttk as integer) as integer
	declare sub showErrorAndAbort(byref message as string)
	declare sub setLocation(byval flags as integer = 0)
	declare sub newLine()
end type

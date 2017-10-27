#include once "common.bi"

declare function hTrim(byref s as string) as string
declare function hLTrim(byref s as string) as string
declare function strStartsWith(byref s as string, byref lookfor as string) as integer
declare function strDuplicate(byval s as const zstring ptr) as zstring ptr
declare sub strSplit(byref s as string, byref delimiter as string, byref l as string, byref r as string)
declare function strReplace _
	( _
		byref text as string, _
		byref a as string, _
		byref b as string _
	) as string
declare function strReplaceNonIdChars(byref orig as string, byval replacement as integer) as string
declare function strMakePrintable(byref a as string) as string
declare function strIsValidSymbolId(byval s as const zstring ptr) as integer
declare function strIsNumber(byref s as string) as integer
declare function strIsReservedIdInC(byval id as zstring ptr) as integer
declare function strMatch(byref s as const string, byref pattern as const string) as integer

'' TODO: are EOL nodes really necessary? Maybe it's enough to store an EOL flag
'' on all possible terminal nodes.
type StringMatcher
	enum
		MatchRoot = 0 '' StringMatcher.root will automatically use this (zero-initialization)
		MatchString
		MatchWildcard
		MatchEol
	end enum

	nodekind as integer

	'' string for MatchString nodes
	text as zstring ptr
	textlength as integer

	payload as any ptr '' MatchEol

	nonEmpty as integer '' MatchRoot only

	'' array of child nodes
	children as StringMatcher ptr
	childcount as integer

	declare destructor()
	declare sub addChild(byval nodekind as integer, byval text as const ubyte ptr, byval textlength as integer)
	declare sub addChildHoldingPreviousChildren(byval nodekind as integer, byval text as const ubyte ptr, byval textlength as integer)
	declare sub truncateTextToOnly(byval newlength as integer)
	declare sub addPattern(byval pattern as const zstring ptr, byval payload as any ptr = NULL)
	declare function matches(byval s as const zstring ptr, byref payload as any ptr = NULL) as integer
	declare function dump1() as string
	declare sub dump()
	declare operator let(byref as const StringMatcher) '' unimplemented
end type

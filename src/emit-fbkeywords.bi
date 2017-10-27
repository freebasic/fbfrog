#include once "util-hash.bi"

enum
	FBKW_OP
	FBKW_CORE
	FBKW_QUIRK
	FBKW_RTL
	FBKW_PP
end enum

type FBKeywordTable
	tb as THash = THash(8, FALSE)
	declare constructor()
	declare operator let(byref as const FBKeywordTable) '' unimplemented
	declare function lookup(byval id as zstring ptr) as integer
end type

extern fbkeywords as FBKeywordTable

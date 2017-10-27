'' The hash table is an array of items,
'' which associate a string to some user data.
type THashItem
	s as const zstring ptr
	hash as ulong     '' hash value for quick comparison
	data as any ptr   '' user data
end type

type THash
	items		as THashItem ptr
	count		as integer  '' number of used items
	room		as integer  '' number of allocated items

	'' Whether this hash table should strDuplicate() when storing strings
	'' and free them on hashEnd(). If FALSE, the caller is responsible for
	'' ensuring that strings passed to hashAdd*() stay valid as long as
	'' hashLookup()'s may be done, i.e. typically until hashEnd().
	duplicate_strings	as integer

	declare constructor(byval exponent as integer, byval duplicate_strings as integer = FALSE)
	declare destructor()
	declare operator let(byref as const THash) '' unimplemented

	declare function lookup(byval s as const zstring ptr, byval hash as ulong) as THashItem ptr
	declare function lookupDataOrNull(byval id as const zstring ptr) as any ptr
	declare function contains(byval s as const zstring ptr, byval hash as ulong) as integer
	declare sub add(byval item as THashItem ptr, byval hash as ulong, byval s as const zstring ptr, byval dat as any ptr)
	declare function addOverwrite(byval s as const zstring ptr, byval dat as any ptr) as THashItem ptr
	#if __FB_DEBUG__
		declare sub dump()
	#endif

	private:
		declare sub allocTable()
		declare sub growTable()
end type

declare function hashHash(byval s as const zstring ptr) as ulong

type StrBuffer
	p as zstring ptr ptr
	count as integer
private:
	hashtb as THash = THash(8, FALSE)
	room as integer
public:
	declare destructor()
	declare operator let(byref as const StrBuffer) '' unimplemented
	declare function store(byval payload as const ubyte ptr, byval size as uinteger) as integer
	declare function store(byval payload as const zstring ptr) as integer
end type

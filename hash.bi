#define DEBUG_HASH

'' The hash table is an array of these hash items, which associate a string to
'' some user data (always an array index in our case).
type HashItem
	as ubyte ptr s
	as integer length
	as uinteger hash        '' Hash value for quick comparison
	as integer data         '' User data
end type

type HashTable
	as HashItem ptr items  '' The table
	as integer count        '' Used
	as integer room         '' Allocated
	#ifdef DEBUG_HASH
		as integer initialroom
		as integer lookups
		as integer collisions
	#endif
end type

declare function hash_hash(byval s as ubyte ptr, byval length as integer) as uinteger
declare function hash_lookup _
	( _
		byval h as HashTable ptr, _
		byval s as ubyte ptr, _
		byval length as integer, _
		byval hash as uinteger _
	) as HashItem ptr
declare sub hash_init(byval h as HashTable ptr, byval exponent as integer)

#ifdef DEBUG_HASH
	declare sub _hash_stats(byval h as HashTable ptr)
	#define hash_stats(h) print "[hash] " & #h : _hash_stats(h)
#else
	#define hash_stats(h) _hash_stats(h)
#endif

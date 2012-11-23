'' Global token text storage (no-deletions-buffer + hash table)
'' Note: The lexer uses the hash items' data fields to store the KW_* values
'' for C keywords; all other entries use -1.

#include once "fbfrog.bi"
#include once "crt.bi"

type TEXTSTORAGE
	hash		as HASHTABLE

	p		as any ptr  '' Begin of free room in last allocated block
	free		as integer  '' Remaining free room

	hits		as integer  '' How often existing entries were reused
	misses		as integer  '' Number of entries
	used		as integer  '' Amount of stored bytes
	wasted		as integer  '' Unused/unreachable/wasted bytes
	blockcount	as integer  '' Number of allocated blocks
	allocated	as integer  '' Full allocated size
end type

dim shared as TEXTSTORAGE storage

private function storageAllocate( byval length as integer ) as ubyte ptr
	'' storage: Stores away bytes, no reallocations, no deletions
	'' Purpose: to be faster than malloc(), and to avoid having to
	'' deallocate() token text every single time a token is removed.

	const STORAGE_ROOM = (1 shl 13)

	'' Not enough room left in the current block?
	if( storage.free < length ) then
		'' The remaining free room in the previous block will be lost
		storage.wasted += storage.free
		storage.blockcount += 1

		storage.free = STORAGE_ROOM
		if( storage.free < length ) then
			storage.free = length
		end if

		storage.allocated += storage.free

		'' Get new block
		storage.p = allocate( storage.free )
	end if

	function = storage.p

	storage.p += length
	storage.free -= length
	storage.used += length
end function

function storageStore _
	( _
		byval text as ubyte ptr, _
		byval length as integer, _
		byval pdat as integer ptr _
	) as ubyte ptr

	dim as uinteger hash = any
	dim as HashItem ptr item = any
	dim as zstring ptr s = any

	'' Prevent empty strings from getting into the hash, allowing that
	'' would be pointless.
	if( length = 0 ) then
		storage.hits += 1
		return @""
	end if

	hash = hashHash( text, length )
	item = hashLookup( @storage.hash, text, length, hash )

	'' Already exists?
	if( item->s ) then
		storage.hits += 1
		*pdat = cint( item->data )
		return item->s
	end if

	'' Allocate new string, and add a null terminator
	storage.misses += 1
	s = storageAllocate( length + 1 )
	memcpy( s, text, length )
	s[length] = 0

	'' Add the new entry
	hashAdd( @storage.hash, item, hash, s, length, cptr( any ptr, *pdat ) )

	function = s
end function

sub storageInit( )
	'' The hash table must be big to avoid collisions...
	'' 2^10 works well for the C keywords currently (see also hashLookup()
	'' and enable the debug code there), however this is supposed to hold
	'' much more than just the keywords.
	hashInit( @storage.hash, 12 )

	storage.p = NULL
	storage.free = 0

	storage.hits = 0
	storage.misses = 0
	storage.used = 0
	storage.wasted = 0
	storage.blockcount = 0
end sub

sub storageStats( )
	print using "  storage: &/& hits (&%), &/& used (&%)"; _
		storage.hits; storage.hits + storage.misses; _
		cint( (100 / (storage.hits + storage.misses)) * storage.hits ); _
		storage.used; storage.allocated; _
		cint( (100 / storage.allocated) * storage.used )

	print using "              & blocks, average stored length: &, & wasted (&%)"; _
		storage.blockcount; _
		iif( storage.misses > 0, storage.used \ storage.misses, 0 ); _
		storage.wasted; cint( (100 / storage.allocated) * storage.wasted )

	hashStats( @storage.hash, "storage" )
end sub

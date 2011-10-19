'' Generic hash table (open addressing/closed hashing),
'' based on GCC's libcpp's hash table.

#include once "fbfrog.bi"
#include once "crt.bi"

function hash_hash _
	( _
		byval s as ubyte ptr, _
		byval length as integer _
	) as uinteger

	dim as uinteger hash = 5381

	dim as ubyte ptr limit = s + length
	while (s < limit)
		hash = *s + (hash shl 5) - hash
		s += 1
	wend

	return hash
end function

private function hash_hash2 _
	( _
		byval s as ubyte ptr, _
		byval length as integer _
	) as uinteger

	dim as uinteger hash = 0

	dim as ubyte ptr limit = s + length
	while (s < limit)
		hash = *s + (hash shl 6) + (hash shl 16) - hash
		s += 1
	wend

	return hash
end function

private sub allocate_table(byval h as HashTable ptr)
	'' They must be zeroed, because NULL instead of a string indicates
	'' unused items
	h->items = xcallocate(h->room * sizeof(HashItem))
end sub

private sub grow_table(byval h as HashTable ptr)
	dim as HashItem ptr old = h->items
	dim as integer oldroom = h->room

	h->resizes += 1
	h->room shl= 1
	allocate_table(h)

	'' Insert all used items from the old table into the new one.
	'' This will redistribute everything using the new h->room.
	for item as HashItem ptr = old to (old + (oldroom - 1))
		if (item->s) then
			'' Yep, this is recursive, but since the table is
			'' larger by now, we won't recurse in here again.
			*hash_lookup(h, item->s, item->length, item->hash) = *item
		end if
	next

	deallocate(old)
end sub

function hash_lookup _
	( _
		byval h as HashTable ptr, _
		byval s as ubyte ptr, _
		byval length as integer, _
		byval hash as uinteger _
	) as HashItem ptr

	ASSUMING(s)
	ASSUMING(length > 0)

	'' Enlarge the hash map when >= 75% is used up, for better lookup
	'' performance (it's easier to find free items if there are many; i.e.
	'' less collisions), and besides there always must be free slots,
	'' otherwise a lookup could end up in an infinite loop.
	if ((h->count * 4) >= (h->room * 3)) then
		grow_table(h)
	end if

	h->lookups += 1

	dim as uinteger roommask = h->room - 1

	'' First probe
	dim as uinteger i = hash and roommask
	dim as HashItem ptr item = h->items + i

	'' Found unused item with first probe?
	if (item->s = NULL) then
		h->perfects += 1
		return item
	end if

	'' Item is used. Is it the correct string?
	if (item->hash = hash) then
		if (item->length = length) then
			if (memcmp(item->s, s, length) = 0) then
				h->perfects += 1
				return item
			end if
		end if
	end if

	'' The first probe reached an item containing the wrong string.
	'' The collision is resolved by stepping through items until a free item or
	'' the look-for string is found.
	''
	'' The step size is calculated based on a 2nd hash value. It is or'ed with 1
	'' to make sure it is odd, so all items will eventually be reached, because
	'' h->room always is a power of 2.
	dim as uinteger stepsize = (hash_hash2(s, length) and roommask) or 1

	do
#if 0
		'' Collisions happen when both hashes are equal mod table size
		print "** COLLISION at " & i & ": " & _
			*str_duplicate(s, length) & _
			" with existing " & _
			*str_duplicate(item->s, item->length)
#endif
		h->collisions += 1
		i = (i + stepsize) and roommask
		item = h->items + i

		'' Found unused item?
		'' The string is not in the hash, or it would have been found before.
		if (item->s = NULL) then
			exit do
		end if

		'' Item is used. Is it the correct string?
		if (item->hash = hash) then
			if (item->length = length) then
				if (memcmp(item->s, s, length) = 0) then
					exit do
				end if
			end if
		end if
	loop

	return item
end function

sub hash_init(byval h as HashTable ptr, byval exponent as integer)
	h->count = 0
	h->room = 1 shl exponent
	h->resizes = 0
	h->lookups = 0
	h->perfects = 0
	h->collisions = 0
	allocate_table(h)
end sub

#if 0
sub hash_end(byval h as HashTable ptr)
	deallocate(h->items)
end sub
#endif

sub hash_stats(byval h as HashTable ptr, byref prefix as string)
	print using "  " & prefix & " hash: " & _
		"&/& hits (&%), &/& used (&%), & resizes"; _
		h->perfects, h->lookups, _
		cint((100 / h->lookups) * h->perfects), _
		h->count, h->room, _
		cint((100 / h->room) * h->count), _
		h->resizes
end sub

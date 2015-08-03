''
'' Generic hash table (open addressing/closed hashing), based on GCC's libcpp's
'' hash table.
''
'' Note: No deletions possible due to the collision resolution adding duplicates
'' to other entries in the table, instead of adding to the same bucket.
'' Each duplicate can be reached by following through the chain of steps
'' indicated by hashHash2(), the first free entry reached indicates the end of
'' the chain -- that's where duplicates are inserted. Removing an entry from
'' this chain would cause the following entries to become unreachable/lost,
'' as the free item in the middle would appear as the end of the chain now.
''
'' If the hash table owns the strings then hashAdd() will duplicate them and
'' then never change them again until hashEnd() which frees them.
''

#include once "fbfrog.bi"

function hashHash(byval s as zstring ptr) as ulong
	dim as long hash = 5381
	while (*s)[0]
		hash = (*s)[0] + (hash shl 5) - hash
		s += 1
	wend
	function = hash
end function

private function hashHash2(byval s as zstring ptr) as ulong
	dim as ulong hash = 0
	while (*s)[0]
		hash = (*s)[0] + (hash shl 6) + (hash shl 16) - hash
		s += 1
	wend
	function = hash
end function

private sub hAllocTable(byval h as THASH ptr)
	'' They must be zeroed, because NULL instead of a string indicates
	'' unused items
	h->items = callocate(h->room * sizeof(THASHITEM))
end sub

private sub hGrowTable(byval h as THASH ptr)
	var old = h->items
	var oldroom = h->room

	h->room shl= 1
	hAllocTable(h)

	'' Insert all used items from the old table into the new one.
	'' This will redistribute everything using the new h->room.
	for item as THASHITEM ptr = old to (old + (oldroom - 1))
		if item->s then
			'' Yep, this is recursive, but since the table is
			'' larger by now, we won't recurse in here again.
			*hashLookup(h, item->s, item->hash) = *item
		end if
	next

	deallocate(old)
end sub

function hashLookup _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval hash as ulong _
	) as THASHITEM ptr

	'' Enlarge the hash map when >= 75% is used up, for better lookup
	'' performance (it's easier to find free items if there are many; i.e.
	'' less collisions), and besides there always must be free slots,
	'' otherwise a lookup could end up in an infinite loop.
	if (h->count * 4) >= (h->room * 3) then
		hGrowTable(h)
	end if

	dim as uinteger roommask = h->room - 1

	'' First probe
	var i = hash and roommask
	var item = h->items + i

	'' Found unused item with first probe?
	if item->s = NULL then
		return item
	end if

	'' Item is used. Is it the correct string?
	if item->hash = hash then
		if *item->s = *s then
			return item
		end if
	end if

	'' The first probe reached an item containing the wrong string.
	'' The collision is resolved by stepping through items until a
	'' free item or the look-for string is found.
	''
	'' The step size is calculated based on a 2nd hash value. It is or'ed
	'' with 1 to make sure it's odd, so all items will eventually be
	'' reached, because h->room always is a power of 2.
	var stepsize = (hashHash2(s) and roommask) or 1

	do
		i = (i + stepsize) and roommask
		item = h->items + i

		'' Found unused item?
		'' The string is not in the hash, or it would have been found before.
		if item->s = NULL then
			exit do
		end if

		'' Item is used. Is it the correct string?
		if item->hash = hash then
			if *item->s = *s then
				exit do
			end if
		end if
	loop

	function = item
end function

function hashLookupDataOrNull(byval h as THASH ptr, byval id as zstring ptr) as any ptr
	var item = hashLookup(h, id, hashHash(id))
	if item->s then
		function = item->data
	end if
end function

function hashContains _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval hash as ulong _
	) as integer
	function = (hashLookup(h, s, hash)->s <> NULL)
end function

sub hashAdd _
	( _
		byval h as THASH ptr, _
		byval item as THASHITEM ptr, _
		byval hash as ulong, _
		byval s as zstring ptr, _
		byval dat as any ptr _
	)

	if h->duplicate_strings then
		s = strDuplicate(s)
	end if

	item->s = s
	item->hash = hash
	item->data = dat
	h->count += 1

end sub

'' Add entry, overwriting previous user data stored in that slot
function hashAddOverwrite _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval dat as any ptr _
	) as THASHITEM ptr

	var hash = hashHash(s)
	var item = hashLookup(h, s, hash)

	if item->s then
		'' Already exists
		assert(*item->s = *s)
		assert(item->hash = hash)
	else
		'' New entry
		if h->duplicate_strings then
			item->s = strDuplicate(s)
		else
			item->s = s
		end if
		item->hash = hash
		h->count += 1
	end if

	item->data = dat

	function = item
end function

sub hashInit _
	( _
		byval h as THASH ptr, _
		byval exponent as integer, _
		byval duplicate_strings as integer _
	)

	h->count = 0
	h->room = 1 shl exponent
	h->duplicate_strings = duplicate_strings
	hAllocTable(h)

end sub

sub hashEnd(byval h as THASH ptr)
	'' Free each item's string if they were duplicated
	if h->duplicate_strings then
		var i = h->items
		var limit = i + h->room
		while i < limit
			deallocate(i->s)
			i += 1
		wend
	end if

	deallocate(h->items)
end sub

#if __FB_DEBUG__
sub hashDump(byval h as THASH ptr)
	print "hash: " & h->count & "/" & h->room & " slots used"
	for i as integer = 0 to h->room-1
		with h->items[i]
			print "    " & i & ": ";
			if .s then
				print "hash=" + hex(.hash) + ", s=""" + *.s + """, data=" + hex(.data)
			else
				print "(free)"
			end if
		end with
	next
end sub
#endif

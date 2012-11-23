'' Generic hash table (open addressing/closed hashing),
'' based on GCC's libcpp's hash table.

#include once "fbfrog.bi"
#include once "crt.bi"

function hashHash _
	( _
		byval s as ubyte ptr, _
		byval length as integer _
	) as uinteger

	dim as uinteger hash = any
	dim as ubyte ptr limit = any

	hash = 5381
	limit = s + length

	while( s < limit )
		hash = *s + (hash shl 5) - hash
		s += 1
	wend

	function = hash
end function

private function hashHash2 _
	( _
		byval s as ubyte ptr, _
		byval length as integer _
	) as uinteger

	dim as uinteger hash = any
	dim as ubyte ptr limit = any

	hash = 0
	limit = s + length

	while( s < limit )
		hash = *s + (hash shl 6) + (hash shl 16) - hash
		s += 1
	wend

	function = hash
end function

private sub hAllocTable( byval h as HASHTABLE ptr )
	'' They must be zeroed, because NULL instead of a string indicates
	'' unused items
	h->items = callocate( h->room * sizeof( HASHITEM ) )
end sub

private sub hGrowTable( byval h as HASHTABLE ptr )
	dim as HASHITEM ptr old = any
	dim as integer oldroom = any

	old = h->items
	oldroom = h->room

	h->resizes += 1
	h->room shl= 1
	hAllocTable( h )

	'' Insert all used items from the old table into the new one.
	'' This will redistribute everything using the new h->room.
	for item as HASHITEM ptr = old to (old + (oldroom - 1))
		if( item->s ) then
			'' Yep, this is recursive, but since the table is
			'' larger by now, we won't recurse in here again.
			*hashLookup( h, item->s, item->length, item->hash ) = *item
		end if
	next

	deallocate( old )
end sub

function hashLookup _
	( _
		byval h as HASHTABLE ptr, _
		byval s as ubyte ptr, _
		byval length as integer, _
		byval hash as uinteger _
	) as HASHITEM ptr

	dim as uinteger roommask = any, i = any, stepsize = any
	dim as HASHITEM ptr item = any

	'' Enlarge the hash map when >= 75% is used up, for better lookup
	'' performance (it's easier to find free items if there are many; i.e.
	'' less collisions), and besides there always must be free slots,
	'' otherwise a lookup could end up in an infinite loop.
	if( (h->count * 4) >= (h->room * 3) ) then
		hGrowTable( h )
	end if

	h->lookups += 1

	roommask = h->room - 1

	'' First probe
	i = hash and roommask
	item = h->items + i

	'' Found unused item with first probe?
	if( item->s = NULL ) then
		h->perfects += 1
		return item
	end if

	'' Item is used. Is it the correct string?
	if( item->hash = hash ) then
		if( item->length = length ) then
			if( memcmp( item->s, s, length ) = 0 ) then
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
	stepsize = (hashHash2( s, length ) and roommask) or 1

	do
#if 0
		'' Collisions happen when both hashes are equal mod table size
		print "** COLLISION at " + hex( i ) + ": " + _
			*strDuplicate( s, length ) + _
			" with existing " + _
			*strDuplicate( item->s, item->length )
#endif
		h->collisions += 1

		i = (i + stepsize) and roommask
		item = h->items + i

		'' Found unused item?
		'' The string is not in the hash, or it would have been found before.
		if( item->s = NULL ) then
			exit do
		end if

		'' Item is used. Is it the correct string?
		if( item->hash = hash ) then
			if( item->length = length ) then
				if( memcmp( item->s, s, length ) = 0 ) then
					exit do
				end if
			end if
		end if
	loop

	function = item
end function

sub hashAdd _
	( _
		byval h as HASHTABLE ptr, _
		byval item as HASHITEM ptr, _
		byval hash as uinteger, _
		byval s as ubyte ptr, _
		byval length as integer, _
		byval dat as any ptr _
	)

	item->s = s
	item->length = length
	item->hash = hash
	item->data = dat
	h->count += 1

end sub

sub hashInit( byval h as HASHTABLE ptr, byval exponent as integer )
	h->count = 0
	h->room = 1 shl exponent
	h->resizes = 0
	h->lookups = 0
	h->perfects = 0
	h->collisions = 0
	hAllocTable( h )
end sub

sub hashEnd( byval h as HASHTABLE ptr )
	deallocate( h->items )
end sub

sub hashStats( byval h as HASHTABLE ptr, byref prefix as string )
	print using "  " + prefix + " hash: " + _
		"&/& hits (&%), &/& used (&%), & resizes"; _
		h->perfects; h->lookups; _
		cint( (100 / h->lookups) * h->perfects ); _
		h->count; h->room; _
		cint( (100 / h->room) * h->count ); _
		h->resizes
end sub

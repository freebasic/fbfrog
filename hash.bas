'' Generic hash table (open addressing/closed hashing),
'' based on GCC's libcpp's hash table.

#include once "fbfrog.bi"
#include once "crt.bi"

function hashHash( byval s as zstring ptr ) as uinteger
	dim as uinteger hash = any

	hash = 5381
	while( (*s)[0] )
		hash = (*s)[0] + (hash shl 5) - hash
		s += 1
	wend

	function = hash
end function

private function hashHash2( byval s as zstring ptr ) as uinteger
	dim as uinteger hash = any

	hash = 0
	while( (*s)[0] )
		hash = (*s)[0] + (hash shl 6) + (hash shl 16) - hash
		s += 1
	wend

	function = hash
end function

private sub hAllocTable( byval h as THASH ptr )
	'' They must be zeroed, because NULL instead of a string indicates
	'' unused items
	h->items = callocate( h->room * sizeof( THASHITEM ) )
end sub

private sub hGrowTable( byval h as THASH ptr )
	dim as THASHITEM ptr old = any
	dim as integer oldroom = any

	old = h->items
	oldroom = h->room

	h->resizes += 1
	h->room shl= 1
	hAllocTable( h )

	'' Insert all used items from the old table into the new one.
	'' This will redistribute everything using the new h->room.
	for item as THASHITEM ptr = old to (old + (oldroom - 1))
		if( item->s ) then
			'' Yep, this is recursive, but since the table is
			'' larger by now, we won't recurse in here again.
			*hashLookup( h, item->s, item->hash ) = *item
		end if
	next

	deallocate( old )
end sub

function hashLookup _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval hash as uinteger _
	) as THASHITEM ptr

	dim as uinteger roommask = any, i = any, stepsize = any
	dim as THASHITEM ptr item = any

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
		if( *item->s = *s ) then
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
	stepsize = (hashHash2( s ) and roommask) or 1

	do
#if 0
		'' Collisions happen when both hashes are equal mod table size
		print "** COLLISION at " + hex( i ) + ": " + *s + _
			" with existing " + *item->s
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
			if( *item->s = *s ) then
				exit do
			end if
		end if
	loop

	function = item
end function

sub hashAdd _
	( _
		byval h as THASH ptr, _
		byval item as THASHITEM ptr, _
		byval hash as uinteger, _
		byval s as zstring ptr, _
		byval dat as any ptr _
	)

	item->s = s
	item->hash = hash
	item->data = dat
	h->count += 1

end sub

sub hashAddOverwrite _
	( _
		byval h as THASH ptr, _
		byval s as zstring ptr, _
		byval dat as any ptr _
	)

	dim as uinteger hash = any
	dim as THASHITEM ptr item = any

	hash = hashHash( s )
	item = hashLookup( h, s, hash )

	'' Update count if entry doesn't exist yet
	if( item->s = NULL ) then
		h->count += 1
	end if

	'' Overwrite existing entry (if any)
	item->s = s
	item->hash = hash
	item->data = dat

end sub

sub hashInit( byval h as THASH ptr, byval exponent as integer )
	h->count = 0
	h->room = 1 shl exponent
	h->resizes = 0
	h->lookups = 0
	h->perfects = 0
	h->collisions = 0
	hAllocTable( h )
end sub

sub hashEnd( byval h as THASH ptr )
	deallocate( h->items )
end sub

sub hashStats( byval h as THASH ptr, byref prefix as string )
	print using "  " + prefix + " hash: " + _
		"&/& hits (&%), &/& used (&%), & resizes"; _
		h->perfects; h->lookups; _
		cint( (100 / h->lookups) * h->perfects ); _
		h->count; h->room; _
		cint( (100 / h->room) * h->count ); _
		h->resizes
end sub

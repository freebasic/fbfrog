'' Generic linked list

#include once "fbfrog.bi"

#define listGetPtr( node ) cptr( any ptr, cptr( ubyte ptr, node ) + sizeof( LISTNODE ) )
#define listGetNode( p ) cptr( LISTNODE ptr, cptr( ubyte ptr, p ) - sizeof( LISTNODE ) )

function listGetHead( byval l as TLIST ptr) as any ptr
	if( l->head ) then
		function = listGetPtr( l->head )
	end if
end function

function listGetTail( byval l as TLIST ptr) as any ptr
	if( l->tail ) then
		function = listGetPtr( l->tail )
	end if
end function

function listGetNext( byval p as any ptr ) as any ptr
	dim as LISTNODE ptr nxt = any
	nxt = listGetNode( p )->next
	if( nxt ) then
		function = listGetPtr( nxt )
	end if
end function

function listGetPrev( byval p as any ptr ) as any ptr
	dim as LISTNODE ptr prv = any
	prv = listGetNode( p )->prev
	if( prv ) then
		function = listGetPtr( prv )
	end if
end function

function listAppend( byval l as TLIST ptr ) as any ptr
	dim as LISTNODE ptr node = any

	node = callocate( l->nodesize )
	node->next = NULL
	node->prev = l->tail
	if( l->tail ) then
		l->tail->next = node
	else
		l->head = node
	end if
	l->tail = node

	function = listGetPtr( node )
end function

sub listDelete( byval l as TLIST ptr, byval p as any ptr )
	dim as LISTNODE ptr node = any, nxt = any, prv = any

	if( p = NULL ) then exit sub
	node = listGetNode( p )

	nxt = node->next
	prv = node->prev
	if( prv ) then
		prv->next = nxt
	else
		l->head = nxt
	end if
	if( nxt ) then
		nxt->prev = prv
	else
		l->tail = prv
	end if

	deallocate( node )
end sub

function listCount( byval l as TLIST ptr ) as integer
	dim as integer count = any
	dim as LISTNODE ptr node = any

	count = 0
	node = l->head
	while( node )
		count += 1
		node = node->next
	wend

	function = count
end function

sub listInit( byval l as TLIST ptr, byval unit as integer )
	l->head = NULL
	l->tail = NULL
	l->nodesize = sizeof( LISTNODE ) + unit
end sub

sub listEnd( byval l as TLIST ptr )
	dim as LISTNODE ptr node = any, nxt = any
	node = l->head
	while( node )
		nxt = node->next
		deallocate( node )
		node = nxt
	wend
end sub

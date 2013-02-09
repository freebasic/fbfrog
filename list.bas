'' Generic linked list

#include once "fbfrog.bi"

#define listGetPtr( node ) cptr( any ptr, cptr( ubyte ptr, node ) + sizeof( LISTNODE ) )
#define listGetNode( p ) cptr( LISTNODE ptr, cptr( ubyte ptr, p ) - sizeof( LISTNODE ) )

function listGetHead( byval l as TLIST ptr) as any ptr
	if( l->head ) then
		function = listGetPtr( l->head )
	end if
end function

function listGetNext( byval p as any ptr ) as any ptr
	dim as LISTNODE ptr nxt = any
	nxt = listGetNode( p )->next
	if( nxt ) then
		function = listGetPtr( nxt )
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

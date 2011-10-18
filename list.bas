'' Generic linked list

#include once "fbfrog.bi"

#define list_node(p)   cptr(ListNode ptr, cptr(ubyte ptr, p   ) - sizeof(ListNode))
#define list_ptr(node) cptr(any ptr     , cptr(ubyte ptr, node) + sizeof(ListNode))

function list_head(byval l as LinkedList ptr) as any ptr
	if (l->head = NULL) then
		return NULL
	end if
	return list_ptr(l->head)
end function

function list_next(byval p as any ptr) as any ptr
	dim as ListNode ptr nxt = list_node(p)->next
	if (nxt = NULL) then
		return NULL
	end if
	return list_ptr(nxt)
end function

function list_append(byval l as LinkedList ptr) as any ptr
	dim as ListNode ptr node = xallocate(l->nodesize)

	node->next = NULL
	node->prev = l->tail
	if (l->tail) then
		l->tail->next = node
	else
		l->head = node
	end if
	l->tail = node

	return list_ptr(node)
end function

sub list_init(byval l as LinkedList ptr, byval unit as integer)
	l->head = NULL
	l->tail = NULL
	l->nodesize = sizeof(ListNode) + unit
end sub

#if 0
sub list_end(byval l as LinkedList ptr)
	dim as ListNode ptr node = l->head
	while (node)
		dim as ListNode ptr nxt = node->next
		deallocate(node)
		node = nxt
	wend
end sub
#endif

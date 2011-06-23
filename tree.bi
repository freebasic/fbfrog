enum
	TREE_DEFINE = 0
end enum

type FbTree
	as integer type
	as zstring ptr id
	as FbTree ptr next
	as FbTree ptr prev
end type

type FbHeader
	as FbHeader ptr next
	as FbHeader ptr prev
	as FbTree ptr head
	as FbTree ptr tail
	as zstring ptr filename
end type

declare sub tree_global_init()
declare sub tree_begin_header(byval filename as zstring ptr)
declare sub tree_add(byval t as FbTree ptr)
declare function tree_new_define(byval id as zstring ptr) as FbTree ptr

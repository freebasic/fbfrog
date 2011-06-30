enum
	TREE_DEFINE = 0
	TREE_INCLUDE
end enum

type FbTree
	as integer type
	as zstring ptr text
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
declare function tree_get_first_header() as FbHeader ptr
declare sub tree_add(byval t as FbTree ptr)
declare sub tree_add_define(byval id as zstring ptr)
declare sub tree_add_include(byval is_computed as integer, byval text as zstring ptr)

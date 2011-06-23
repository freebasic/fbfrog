'' Tree representing all header files, their #define's, their UDTs, function
'' declarations, etc.
''
'' Identifiers and string/number literals are stored in a huge text buffer
'' and a hash table to avoid storing duplicates.
'' Node and text buffers: no reallocations, no deletions

#include once "tree.bi"
#include once "hash.bi"

type TreeStuff
	'' Free room in the current block of memory for tree nodes.
	'' As soon as it fills up, we allocate a new one.
	as FbTree ptr node
	as integer nodefree
	#ifdef ENABLE_STATS
		as integer nodecount      '' Overall number of nodes
		as integer nodeblockcount '' Allocated blocks
	#endif

	'' Same for text data
	as ubyte ptr text
	as integer textfree
	#ifdef ENABLE_STATS
		as integer textwasted '' Unused/unreachable/wasted bytes
		as integer textcount  '' Number of "entries"
		as integer textblockcount '' Allocated blocks
		as integer textstored '' Amount of stored bytes
	#endif
	as HashTable texthash '' Hash table of all strings, to avoid duplicates

	'' List of headers, each containing its tree
	as FbHeader ptr head
	as FbHeader ptr tail
end type

dim shared as TreeStuff tree

#ifdef ENABLE_STATS
private sub show_stats() destructor
	print "tree stats:"
	print !"\t" & tree.nodecount & " nodes allocated on " & tree.nodeblockcount & " blocks"
	print !"\t" & tree.textcount & " text strings on " & tree.textblockcount & " blocks"
	print !"\t" & tree.textstored & " text bytes stored, " & tree.textwasted & " bytes wasted"
	print !"\t" & "average string length: " & iif(tree.textcount > 0, tree.textstored \ tree.textcount, 0)
	hash_stats(@tree.texthash)
end sub
#endif

sub tree_global_init()
	hash_init(@tree.texthash, 13)
end sub

private function text_store(byval text as zstring ptr) as zstring ptr
	dim as integer length = len(*text)

	'' Already stored this string?
	dim as uinteger hash = hash_hash(text, length)
	dim as HashItem ptr item = hash_lookup(@tree.texthash, text, length, hash)
	if (item->s) then
		return item->s
	end if

	'' NULL terminator
	length += 1

	#ifdef ENABLE_STATS
		tree.textcount += 1
		tree.textstored += length
	#endif

	'' Not enough room left in the current block? Then get a new block.
	if (tree.textfree < length) then
		#ifdef ENABLE_STATS
			'' The remaining free room is lost
			tree.textwasted += tree.textfree
			tree.textblockcount += 1
		#endif

		const BLOCK_SIZE = (1 shl 14)
		tree.textfree = length
		if (tree.textfree < BLOCK_SIZE) then
			tree.textfree = BLOCK_SIZE
		end if
		tree.text = xallocate(tree.textfree)
	end if

	dim as ubyte ptr p = tree.text
	tree.text += length
	tree.textfree -= length

	fb_MemCopy(byval p, byval text, length)
	item->s = p
	item->hash = hash
	/'item->data = <unused>'/

	return p
end function

private function tree_alloc() as FbTree ptr
	#ifdef ENABLE_STATS
		tree.nodecount += 1
	#endif

	if (tree.nodefree = 0) then
		#ifdef ENABLE_STATS
			tree.nodeblockcount += 1
		#endif
		tree.nodefree = 256
		tree.node = xallocate(sizeof(FbTree) * tree.nodefree)
	end if

	dim as FbTree ptr node = tree.node
	tree.node += 1
	tree.nodefree -= 1
	return node
end function

sub tree_begin_header(byval filename as zstring ptr)
	dim as FbHeader ptr header = xallocate(sizeof(FbHeader))
	header->next = NULL
	header->prev = tree.tail
	tree.tail = header
	if (tree.head = NULL) then
		tree.head = header
	end if

	header->head = NULL
	header->tail = NULL

	header->filename = text_store(filename)
end sub

'' Append a node to the last header
sub tree_add(byval t as FbTree ptr)
	t->prev = tree.tail->tail
	t->next = NULL
	tree.tail->tail = t
	if (tree.tail->head = NULL) then
		tree.tail->head = t
	end if
end sub

function tree_new_define(byval id as zstring ptr) as FbTree ptr
	dim as FbTree ptr t = tree_alloc()
	t->type = TREE_DEFINE
	t->id = text_store(id)
	t->next = NULL
	t->prev = NULL
	return t
end function

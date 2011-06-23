#include once "emit.bi"
#include once "tree.bi"

type EmitterStuff
	as integer f
end type

dim shared as EmitterStuff emitter

private sub emit(byval what as zstring ptr)
	if (put(#emitter.f, , *what)) then
		xoops("file output failed!")
	end if
end sub

private sub emit_open(byref filename as string)
	emitter.f = freefile()
	if (open(filename, for binary, access write, as #emitter.f)) then
		xoops("could not open output file: '" & filename & "'")
	end if
end sub

private sub emit_close()
	close #emitter.f
	emitter.f = 0
end sub

private sub emit_tree(byval t as FbTree ptr)
	static as integer level = 0

	select case as const (t->type)
	case TREE_DEFINE
		emit("#define ")
		emit(t->id)

	end select
end sub

sub emit_header(byval header as FbHeader ptr)
	emit_open(*header->filename)

	dim as FbTree ptr t = header->head
	while (t)
		emit_tree(t)
		t = t->next
	wend

	emit_close()
end sub

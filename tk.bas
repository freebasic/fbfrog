#include once "tk.bi"
#include once "common.bi"
#include once "crt.bi"

type OneToken
	as integer id        '' TK_*
	as zstring ptr text  '' Identifiers and number/string literals, or NULL
	as integer stmt      '' STMT_*
end type

type AllTokens
	'' Gap buffer of tokens
	as OneToken ptr p   '' Buffer containing: front,gap,back
	as integer front    '' Front length; the gap's offset
	as integer gap      '' Gap length
	as integer size     '' Front + back

	'' Stats
	as ulongint inputsize  '' Sum of input file sizes, just for stats
	as integer inputtokens '' Count of input tokens
end type

dim shared as AllTokens tk

private function tk_ptr(byval x as integer) as OneToken ptr
	'' Inside end?
	if (x >= tk.front) then
		'' Invalid?
		if (x >= tk.size) then
			return NULL
		end if
		x += tk.gap
	else
		'' Invalid?
		if (x < 0) then
			return NULL
		end if
	end if
	return tk.p + x
end function

sub tk_move(byval delta as integer)
	tk_move_to(tk.front + delta)
end sub

sub tk_move_to(byval x as integer)
	if (x < 0) then
		x = 0
	elseif (x > tk.size) then
		x = tk.size
	end if

	dim as integer old = tk.front
	if (x < old) then
		'' Move gap left
		dim as OneToken ptr p = tk.p + x
		memmove(p + tk.gap, p, (old - x) * sizeof(OneToken))
	elseif (x > old) then
		'' Move gap right
		dim as OneToken ptr p = tk.p + old
		memmove(p, p + tk.gap, (x - old) * sizeof(OneToken))
	end if

	tk.front = x
end sub

sub tk_in(byval id as integer, byval text as zstring ptr)
	dim as integer length = any
	if (text) then
		length = len(*text)
	else
		length = 0
	end if
	tk_in_raw(id, text, length)
end sub

'' Insert token at current position, the current position moves forward.
sub tk_in_raw _
	( _
		byval id as integer, _
		byval text as ubyte ptr, _
		byval length as integer _
	)

	dim as OneToken ptr p = any

	'' Make room for the new data, if necessary
	if (tk.gap = 0) then
		const NEWGAP = 512

		tk.p = xreallocate(tk.p, (tk.size + NEWGAP) * sizeof(OneToken))
		p = tk.p + tk.front

		'' Move the back block to the end of the new buffer,
		'' so that the gap in the middle grows.
		if (tk.size > tk.front) then
			memmove(p + NEWGAP, p + tk.gap, _
			        (tk.size - tk.front) * sizeof(OneToken))
		end if

		tk.gap = NEWGAP
	else
		p = tk.p + tk.front
	end if

	p->id = id
	if (length > 0) then
		p->text = xallocate(length + 1)
		memcpy(p->text, text, length)
		p->text[length] = 0
	else
		p->text = NULL
	end if
	p->stmt = STMT_TOPLEVEL

	tk.front += 1
	tk.gap -= 1
	tk.size += 1
end sub

sub tk_insert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr _
	)
	tk_move_to(x)
	tk_in(id, text)
end sub

sub tk_insert_space(byval x as integer)
	tk_insert(x, TK_SPACE, " ")
end sub

'' Delete token in front of current position (backwards deletion)
sub tk_out()
	if (tk.front < 1) then return

	tk.front -= 1
	tk.gap += 1
	tk.size -= 1

	dim as OneToken ptr p = tk.p + tk.front
	deallocate(p->text)
end sub

sub tk_remove(byval x as integer)
	tk_move_to(x + 1)
	tk_out()
end sub

sub tk_drop_all()
	for i as integer = 0 to (tk.size - 1)
		deallocate(tk_ptr(i)->text)
	next
	tk.gap += tk.front + tk.size
	tk.front = 0
	tk.size = 0
end sub

sub tk_init()
	tk.p = NULL
	tk.front = 0
	tk.gap = 0
	tk.size = 0
end sub

sub tk_end()
	deallocate(tk.p)
end sub

function tk_get(byval x as integer) as integer
	dim as OneToken ptr p = tk_ptr(x)
	if (p = NULL) then
		return TK_EOF
	end if
	return p->id
end function

function tk_text(byval x as integer) as zstring ptr
	dim as OneToken ptr p = tk_ptr(x)
	if (p = NULL) then
		return NULL
	end if
	return p->text
end function

function tk_stmt(byval x as integer) as integer
	dim as OneToken ptr p = tk_ptr(x)
	if (p = NULL) then
		return STMT_TOPLEVEL
	end if
	return p->stmt
end function

function tk_count() as integer
	return tk.size
end function

sub tk_mark_stmt _
	( _
		byval stmt as integer, _
		byval first as integer, _
		byval xright as integer _
	)

	for i as integer = first to (xright - 1)
		dim as OneToken ptr p = tk_ptr(i)
		p->stmt = stmt
	next
end sub

sub tk_count_input_size(byval n as integer)
	tk.inputsize += n
end sub

sub tk_count_input_token()
	tk.inputtokens += 1
end sub

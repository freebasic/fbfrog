#include once "tk.bi"
#include once "common.bi"
#include once "crt.bi"

type OneToken
	as integer id       '' TK_*
	as zstring ptr text '' Identifiers and number/string literals, or NULL
end type

type AllTokens
	as OneToken ptr p
	as integer count
	as integer room
	as ulongint inputsize  '' Sum of input file sizes, just for stats
	as integer inputtokens '' Count of input tokens
end type

dim shared as AllTokens tk

sub tk_init()
end sub

sub tk_insert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr _
	)
	dim as integer length = any
	if (text) then
		length = len(*text)
	else
		length = 0
	end if
	tk_insert_raw(x, id, text, length)
end sub

sub tk_insert_raw _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as ubyte ptr, _
		byval length as integer _
	)

	xassert((x >= 0) and (x <= tk.count))

	if (tk.room = tk.count) then
		tk.room += 512
		tk.p = xreallocate(tk.p, tk.room * sizeof(OneToken))
	end if
	dim as OneToken ptr p = tk.p + x
	if (x < tk.count) then
		memmove(p + 1, p, (tk.count - x) * sizeof(OneToken))
	end if
	tk.count += 1

	p->id = id
	if (length > 0) then
		p->text = xallocate(length + 1)
		memcpy(p->text, text, length)
		p->text[length] = 0
	else
		''xassert((tokeninfo(id) and FLAG_TEXT) = 0)
		p->text = NULL
	end if
end sub

sub tk_remove(byval x as integer)
	xassert((x >= 0) and (x < tk.count))

	dim as OneToken ptr p = tk.p + x
	deallocate(p->text)

	tk.count -= 1
	if (x < tk.count) then
		memmove(p, p + 1, (tk.count - x) * sizeof(OneToken))
	end if
end sub

sub tk_drop_all()
	for i as integer = 0 to (tk.count - 1)
		deallocate(tk.p[i].text)
	next
	tk.count = 0
	tk.inputsize = 0
end sub

function tk_get(byval x as integer) as integer
	if (x < 0) then return TK_BOF
	if (x >= tk.count) then return TK_EOF
	return tk.p[x].id
end function

function tk_text(byval x as integer) as zstring ptr
	xassert((x >= 0) and (x < tk.count))
	''xassert(tokeninfo(tk.p[x].id) and FLAG_TEXT)
	return tk.p[x].text
end function

sub tk_count_input_size(byval n as integer)
	tk.inputsize += n
end sub

sub tk_count_input_token()
	tk.inputtokens += 1
end sub

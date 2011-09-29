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

sub tk_copy(byval target as integer, byval source as integer)
	tk_insert(target, tk_get(source), tk_text(source))
	tk_mark_stmt(tk_stmt(source), target, target)
end sub

sub tk_copy_range _
	( _
		byval x as integer, _
		byval first as integer, _
		byval last as integer _
	)
	for i as integer = 0 to (last - first)
		tk_copy(x + i, first + i)
	next
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

sub tk_remove_range(byval first as integer, byval last as integer)
	while (last >= first)
		tk_remove(first)
		last -= 1
	wend
end sub

sub tk_replace _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr _
	)
	tk_remove(x)
	tk_insert(x, id, text)
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
		byval last as integer _
	)

	for i as integer = first to last
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

dim shared as zstring ptr token_id_text(0 to (TK__COUNT-1)) = _
{ _
	@"eof", _
	@"todo", _
	@"byte", _
	@"eol", _
	@"space", _
	@"comment", _
	@"linecomment", _
	_
	@"decnum", _
	@"hexnum", _
	@"octnum", _
	_
	@"string", _
	@"char", _
	@"wstring", _
	@"wchar", _
	@"estring", _
	@"echar", _
	@"ewstring", _
	@"ewchar", _
	_
	@"lognot", _
	@"ne", _
	@"hash", _
	@"merge", _
	@"mod", _
	@"selfmod", _
	@"bitand", _
	@"selfbitand", _
	@"logand", _
	@"lparen", _
	@"rparen", _
	@"mul", _
	@"selfmul", _
	@"add", _
	@"selfadd", _
	@"increment", _
	@"comma", _
	@"sub", _
	@"selfsub", _
	@"decrement", _
	@"fieldderef", _
	@"dot", _
	@"ellipsis", _
	@"div", _
	@"selfdiv", _
	@"colon", _
	@"semi", _
	@"lt", _
	@"shl", _
	@"selfshl", _
	@"le", _
	@"assign", _
	@"eq", _
	@"gt", _
	@"shr", _
	@"selfshr", _
	@"ge", _
	@"question", _
	@"lbracket", _
	@"backslash", _
	@"rbracket", _
	@"bitxor", _
	@"selfbitxor", _
	@"underscore", _
	@"lbrace", _
	@"bitor", _
	@"selfbitor", _
	@"logor", _
	@"rbrace", _
	@"bitnot", _
	_
	@"id", _
	_
	@"AUTO", _
	@"BREAK", _
	@"CASE", _
	@"CHAR", _
	@"CONST", _
	@"CONTINUE", _
	@"DEFAULT", _
	@"DEFINE", _
	@"DEFINED", _
	@"DO", _
	@"DOUBLE", _
	@"ELIF", _
	@"ELSE", _
	@"ENDIF", _
	@"ENUM", _
	@"EXTERN", _
	@"FLOAT", _
	@"FOR", _
	@"GOTO", _
	@"IF", _
	@"IFDEF", _
	@"IFNDEF", _
	@"INCLUDE", _
	@"INLINE", _
	@"INT", _
	@"LONG", _
	@"PRAGMA", _
	@"REGISTER", _
	@"RESTRICT", _
	@"RETURN", _
	@"SHORT", _
	@"SIGNED", _
	@"SIZEOF", _
	@"STATIC", _
	@"STRUCT", _
	@"SWITCH", _
	@"TYPEDEF", _
	@"UNDEF", _
	@"UNION", _
	@"UNSIGNED", _
	@"VOID", _
	@"VOLATILE", _
	@"WHILE", _
	_
	@"ALIAS", _
	@"ANY", _
	@"AS", _
	@"BYTE", _
	@"BYVAL", _
	@"CAST", _
	@"CDECL", _
	@"CPTR", _
	@"DECLARE", _
	@"DIM", _
	@"ELSEIF", _
	@"END", _
	@"EXIT", _
	@"EXPORT", _
	@"FIELD", _
	@"FUNCTION", _
	@"IIF", _
	@"INTEGER", _
	@"LONGINT", _
	@"LOOP", _
	@"NEXT", _
	@"PASCAL", _
	@"PRIVATE", _
	@"PTR", _
	@"SCOPE", _
	@"SELECT", _
	@"SHARED", _
	@"SINGLE", _
	@"STDCALL", _
	@"SUB", _
	@"THEN", _
	@"TO", _
	@"TYPE", _
	@"UBYTE", _
	@"UINTEGER", _
	@"ULONG", _
	@"ULONGINT", _
	@"USHORT", _
	@"WEND", _
	@"WSTR", _
	@"WSTRING", _
	@"ZSTRING" _
}

dim shared as zstring ptr token_stmt_text(0 to (STMT__COUNT - 1)) = _
{ _
	@"", _
	@"pp", _
	@"extern", _
	@"endextern", _
	@"struct", _
	@"endstruct", _
	@"enum", _
	@"endenum", _
	@"enumfield", _
	@"field" _
}

function tk_debug(byval x as integer) as string
	return *token_stmt_text(tk_stmt(x)) & "." & *token_id_text(tk_get(x))
end function

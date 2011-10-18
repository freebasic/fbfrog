#include once "fbfrog.bi"
#include once "crt.bi"

dim shared as zstring ptr token_text(0 to (TK__COUNT - 1)) = _
{ _
	@"<eof>"        , _
	@"<todo>"       , _
	@"<byte>"       , _
	@"<eol>"        , _
	@"<space>"      , _
	@"<comment>"    , _
	@"<linecomment>", _
	@"<decnum>"     , _ '' Number literals
	@"<hexnum>"     , _
	@"<octnum>"     , _
	@"<string>"     , _ '' String literals
	@"<char>"       , _
	@"<wstring>"    , _
	@"<wchar>"      , _
	@"<estring>"    , _
	@"<echar>"      , _
	@"<ewstring>"   , _
	@"<ewchar>"     , _
	@"!"      , _ '' Main tokens
	@"!="     , _
	@"#"      , _
	@"##"     , _
	@"%"      , _
	@"%="     , _
	@"&"      , _
	@"&="     , _
	@"&&"     , _
	@"("      , _
	@")"      , _
	@"*"      , _
	@"*="     , _
	@"+"      , _
	@"+="     , _
	@"++"     , _
	@","      , _
	@"-"      , _
	@"-="     , _
	@"--"     , _
	@"->"     , _
	@"."      , _
	@"..."    , _
	@"/"      , _
	@"/="     , _
	@":"      , _
	@";"      , _
	@"<"      , _
	@"<<"     , _
	@"<<="    , _
	@"<="     , _
	@"<>"     , _
	@"="      , _
	@"=="     , _
	@">"      , _
	@">>"     , _
	@">>="    , _
	@">="     , _
	@"?"      , _
	@"@"      , _
	@"["      , _
	@"\"      , _
	@"]"      , _
	@"^"      , _
	@"^="     , _
	@"_"      , _
	@"{"      , _
	@"|"      , _
	@"|="     , _
	@"||"     , _
	@"}"      , _
	@"~"      , _
	@"<id>"      , _ '' TK_ID
	@"auto"      , _ '' C/FB keywords
	@"break"     , _
	@"case"      , _
	@"char"      , _
	@"const"     , _
	@"continue"  , _
	@"default"   , _
	@"define"    , _
	@"defined"   , _
	@"do"        , _
	@"double"    , _
	@"elif"      , _
	@"else"      , _
	@"endif"     , _
	@"enum"      , _
	@"extern"    , _
	@"float"     , _
	@"for"       , _
	@"goto"      , _
	@"if"        , _
	@"ifdef"     , _
	@"ifndef"    , _
	@"include"   , _
	@"inline"    , _
	@"int"       , _
	@"long"      , _
	@"pragma"    , _
	@"register"  , _
	@"restrict"  , _
	@"return"    , _
	@"short"     , _
	@"signed"    , _
	@"sizeof"    , _
	@"static"    , _
	@"struct"    , _
	@"switch"    , _
	@"typedef"   , _
	@"undef"     , _
	@"union"     , _
	@"unsigned"  , _
	@"void"      , _
	@"volatile"  , _
	@"while"     , _
	@"alias"      , _  '' FB-only keywords
	@"and"        , _
	@"andalso"    , _
	@"any"        , _
	@"as"         , _
	@"byte"       , _
	@"byval"      , _
	@"cast"       , _
	@"cdecl"      , _
	@"cptr"       , _
	@"declare"    , _
	@"dim"        , _
	@"elseif"     , _
	@"end"        , _
	@"exit"       , _
	@"export"     , _
	@"field"      , _
	@"function"   , _
	@"iif"        , _
	@"integer"    , _
	@"longint"    , _
	@"loop"       , _
	@"mod"        , _
	@"next"       , _
	@"not"        , _
	@"or"         , _
	@"orelse"     , _
	@"pascal"     , _
	@"private"    , _
	@"ptr"        , _
	@"scope"      , _
	@"select"     , _
	@"shared"     , _
	@"shl"        , _
	@"shr"        , _
	@"single"     , _
	@"stdcall"    , _
	@"sub"        , _
	@"then"       , _
	@"to"         , _
	@"type"       , _
	@"ubyte"      , _
	@"uinteger"   , _
	@"ulong"      , _
	@"ulongint"   , _
	@"ushort"     , _
	@"wend"       , _
	@"wstr"       , _
	@"wstring"    , _
	@"xor"        , _
	@"zstring"      _
}

dim shared as zstring ptr mark_text(0 to (MARK__COUNT - 1)) = _
{ _
	@"", _
	@"pp", _
	@"ppexpr", _
	@"extern", _
	@"endextern", _
	@"struct", _
	@"endenum", _
	@"endstruct", _
	@"endunion", _
	@"enumconst", _
	@"typedef", _
	@"topdecl", _
	@"procdecl", _
	@"vardecl", _
	@"fielddecl", _
	@"unknown", _
	@"unknownenumconst" _
}

type OneToken field = 1
	as short id         '' TK_*
	as short mark       '' MARK_*
	as zstring ptr text '' Identifiers and number/string literals, or NULL
end type

type TokenBuffer
	'' Gap buffer of tokens
	as OneToken ptr p   '' Buffer containing: front,gap,back
	as integer front    '' Front length; the gap's offset
	as integer gap      '' Gap length
	as integer size     '' Front + back

	as integer maxsize  '' Highest amount of tokens at once
	as integer reallocs '' Buffer reallocations
	as integer inserts
	as integer deletes
	as integer lookups '' tk_access() calls
end type

dim shared as TokenBuffer tk

private function tk_access(byval x as integer) as OneToken ptr
	'' All token information queries go through here.
	'' (For example from tk_get(), which is the most used function by
	'' the parser/translator)
	'' This can easily be called 10k times on only small headers,
	'' and hundred thousands of times for bigger ones.

	tk.lookups += 1

	'' Static EOF token for "error recovery"
	static as OneToken static_eof = (TK_EOF, MARK_TOPLEVEL, NULL)

	'' Inside end?
	if (x >= tk.front) then
		'' Invalid?
		if (x >= tk.size) then
			return @static_eof
		end if
		x += tk.gap
	else
		'' Invalid?
		if (x < 0) then
			return @static_eof
		end if
	end if

	return tk.p + x
end function

sub tk_raw_move_to(byval x as integer)
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

'' Insert token at current position, the current position moves forward.
sub tk_raw_insert _
	( _
		byval id as integer, _
		byval text as ubyte ptr, _
		byval length as integer _
	)

	dim as OneToken ptr p = any

	'' Make room for the new data, if necessary
	if (tk.gap = 0) then
		const NEWGAP = 512

		tk.reallocs += 1

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
	p->mark = MARK_TOPLEVEL
	if (length > 0) then
		p->text = str_duplicate(text, length)
	else
		p->text = NULL
	end if

	tk.front += 1
	tk.gap -= 1
	tk.size += 1

	tk.inserts += 1
	if (tk.maxsize < tk.size) then
		tk.maxsize = tk.size
	end if
end sub

sub tk_insert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr _
	)
	tk_raw_move_to(x)
	dim as integer length = any
	if (text) then
		length = len(*text)
	else
		length = 0
	end if
	tk_raw_insert(id, text, length)
end sub

sub tk_insert_space(byval x as integer)
	tk_insert(x, TK_SPACE, " ")
end sub

sub tk_copy _
	( _
		byval x as integer, _
		byval first as integer, _
		byval last as integer _
	)
	for i as integer = 0 to (last - first)
		dim as integer target = x + i
		dim as integer source = first + i
		tk_insert(target, tk_get(source), tk_text(source))
		tk_set_mark(tk_mark(source), target, target)
	next
end sub

sub tk_remove(byval first as integer, byval last as integer)
	'' TODO: clean up
	tk_raw_move_to(last + 1)
	while (last >= first)

		'' Delete token in front of current position (backwards deletion)
		if (tk.front >= 1) then
			tk.deletes += 1

			tk.front -= 1
			tk.gap += 1
			tk.size -= 1
			deallocate(tk.p[tk.front].text)
		end if

		last -= 1
	wend
end sub

sub tk_set_mark _
	( _
		byval mark as integer, _
		byval first as integer, _
		byval last as integer _
	)
	for i as integer = first to last
		dim as OneToken ptr p = tk_access(i)
		if (p->id <> TK_EOF) then
			p->mark = mark
		end if
	next
end sub

function tk_get(byval x as integer) as integer
	return tk_access(x)->id
end function

function tk_text(byval x as integer) as zstring ptr
	return tk_access(x)->text
end function

function tk_mark(byval x as integer) as integer
	return tk_access(x)->mark
end function

function tk_count() as integer
	return tk.size
end function

sub tk_init()
	tk.p = NULL
	tk.front = 0
	tk.gap = 0
	tk.size = 0

	tk.maxsize = 0
	tk.reallocs = 0
	tk.inserts = 0
	tk.deletes = 0
	tk.lookups = 0
end sub

sub tk_end()
	for i as integer = 0 to (tk.size - 1)
		deallocate(tk_access(i)->text)
	next
	deallocate(tk.p)

	if (frog.verbose) then
		print using "  tk: max load: & (& in, & out; & reallocs), & lookups"; _
			tk.maxsize, tk.inserts, tk.deletes, tk.reallocs, tk.lookups
	end if
end sub

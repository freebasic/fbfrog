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

type ONETOKEN
	as short id         '' TK_*
	as short mark       '' MARK_*
	as zstring ptr text '' Identifiers and number/string literals, or NULL
end type

type TOKENBUFFER
	'' Gap buffer of tokens
	p		as ONETOKEN ptr  '' Buffer containing: front,gap,back
	front		as integer  '' Front length; the gap's offset
	gap		as integer  '' Gap length
	size		as integer  '' Front + back

	maxsize		as integer  '' Highest amount of tokens at once
	reallocs	as integer  '' Buffer reallocations
	inserts		as integer
	deletes		as integer
	lookups		as integer  '' tk_access() calls
end type

dim shared as TOKENBUFFER tk

function strDuplicate _
	( _
		byval s as ubyte ptr, _
		byval length as integer _
	) as zstring ptr

	dim as zstring ptr p = any

	p = allocate( length + 1 )

	if( length > 0 ) then
		memcpy( p, s, length )
	end if
	p[length] = 0

	function = p
end function

private function tkAccess( byval x as integer ) as ONETOKEN ptr
	tk.lookups += 1

	'' Static EOF token for "error recovery"
	static as ONETOKEN static_eof = (TK_EOF, MARK_TOPLEVEL, NULL)

	'' Inside end?
	if( x >= tk.front ) then
		'' Invalid?
		if( x >= tk.size ) then
			return @static_eof
		end if
		x += tk.gap
	else
		'' Invalid?
		if( x < 0 ) then
			return @static_eof
		end if
	end if

	function = tk.p + x
end function

sub tkRawMoveTo( byval x as integer )
	dim as integer old = any
	dim as ONETOKEN ptr p = any

	if( x < 0 ) then
		x = 0
	elseif( x > tk.size ) then
		x = tk.size
	end if

	old = tk.front
	if( x < old ) then
		'' Move gap left
		p = tk.p + x
		memmove( p + tk.gap, p, (old - x) * sizeof( ONETOKEN ) )
	elseif (x > old) then
		'' Move gap right
		p = tk.p + old
		memmove( p, p + tk.gap, (x - old) * sizeof( ONETOKEN ) )
	end if

	tk.front = x
end sub

'' Insert token at current position, the current position moves forward.
sub tkRawInsert( byval id as integer, byval text as ubyte ptr )
	const NEWGAP = 512
	dim as ONETOKEN ptr p = any

	'' Make room for the new data, if necessary
	if( tk.gap = 0 ) then
		'' Reallocate the buffer, then move the back block to the
		'' end of the new buffer, so that the gap in the middle grows.
		tk.reallocs += 1
		tk.p = reallocate( tk.p, (tk.size + NEWGAP) * sizeof( ONETOKEN ) )
		p = tk.p + tk.front
		if( tk.size > tk.front ) then
			memmove( p + NEWGAP, p + tk.gap, _
			         (tk.size - tk.front) * sizeof( ONETOKEN ) )
		end if
		tk.gap = NEWGAP
	else
		p = tk.p + tk.front
	end if

	p->id = id
	p->mark = MARK_TOPLEVEL
	p->text = text

	tk.front += 1
	tk.gap -= 1
	tk.size += 1

	tk.inserts += 1
	if( tk.maxsize < tk.size ) then
		tk.maxsize = tk.size
	end if
end sub

sub tkInsert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr _
	)

	dim as integer dat = any

	tkRawMoveTo( x )

	'' See also lex.bas:add_text_token_raw(); this is the same, except
	'' that here we don't turn TK_IDs into KW_*s, and this is also used
	'' for non-text tokens.
	if( text ) then
		dat = -1
		text = storageStore( text, len( *text ), @dat )
	end if

	tkRawInsert( id, text )
end sub

sub tkInsertSpace( byval x as integer )
	tkInsert( x, TK_SPACE, " " )
end sub

sub tkCopy _
	( _
		byval x as integer, _
		byval first as integer, _
		byval last as integer _
	)

	dim as integer target = any, source = any

	for i as integer = 0 to (last - first)
		target = x + i
		source = first + i
		tkInsert( target, tkGet( source ), tkText( source ) )
		tkSetMark( tkMark( source ), target, target )
	next

end sub

sub tkRemove( byval first as integer, byval last as integer )
	dim as integer delta = any

	tkRawMoveTo( last + 1 )

	delta = last - first + 1
	if( delta > tk.front ) then
		delta = tk.front
	end if

	'' Delete tokens in front of current position (backwards deletion)
	tk.deletes += delta
	tk.front -= delta
	tk.gap += delta
	tk.size -= delta
end sub

sub tkSetMark _
	( _
		byval mark as integer, _
		byval first as integer, _
		byval last as integer _
	)

	dim as ONETOKEN ptr p = any

	for i as integer = first to last
		p = tkAccess( i )
		if( p->id <> TK_EOF ) then
			p->mark = mark
		end if
	next

end sub

function tkGet( byval x as integer ) as integer
	function = tkAccess( x )->id
end function

function tkText( byval x as integer ) as zstring ptr
	function = tkAccess( x )->text
end function

function tkMark( byval x as integer ) as integer
	function = tkAccess( x )->mark
end function

function tkCount( ) as integer
	function = tk.size
end function

sub tkInit( )
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

sub tkEnd( )
	deallocate( tk.p )
	if( frog.verbose ) then
		print using "  tokens: & max load, & resizes, " & _
				"& in, & out, & lookups"; _
			tk.maxsize; tk.reallocs; _
			tk.inserts; tk.deletes; tk.lookups
	end if
end sub

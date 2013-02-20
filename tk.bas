#include once "fbfrog.bi"
#include once "crt.bi"

dim shared as TOKENINFO tk_info(0 to (TK__COUNT - 1)) = _
{ _
	( TRUE , NULL  , @"eof"         ), _
	( TRUE , NULL  , @"#include"    ), _
	( TRUE , NULL  , @"#define"     ), _
	( TRUE , NULL  , @"structbegin" ), _
	( TRUE , NULL  , @"structend"   ), _
	( TRUE , NULL  , @"procdecl"    ), _
	( TRUE , NULL  , @"vardecl"     ), _
	( TRUE , NULL  , @"todo"        ), _
	( FALSE, NULL  , @"byte"        ), _
	( TRUE , NULL  , @"eol"         ), _
	( FALSE, NULL  , @"comment"     ), _
	( FALSE, NULL  , @"linecomment" ), _
	( FALSE, NULL  , @"decnum"      ), _ '' Number literals
	( FALSE, NULL  , @"hexnum"      ), _
	( FALSE, NULL  , @"octnum"      ), _
	( FALSE, NULL  , @"string"      ), _ '' String literals
	( FALSE, NULL  , @"char"        ), _
	( FALSE, NULL  , @"wstring"     ), _
	( FALSE, NULL  , @"wchar"       ), _
	( FALSE, NULL  , @"estring"     ), _
	( FALSE, NULL  , @"echar"       ), _
	( FALSE, NULL  , @"ewstring"    ), _
	( FALSE, NULL  , @"ewchar"      ), _
	( FALSE, @"!"  , @"tk" ), _ '' Main tokens
	( FALSE, @"!=" , @"tk" ), _
	( FALSE, @"#"  , @"tk" ), _
	( FALSE, @"##" , @"tk" ), _
	( FALSE, @"%"  , @"tk" ), _
	( FALSE, @"%=" , @"tk" ), _
	( FALSE, @"&"  , @"tk" ), _
	( FALSE, @"&=" , @"tk" ), _
	( FALSE, @"&&" , @"tk" ), _
	( FALSE, @"("  , @"tk" ), _
	( FALSE, @")"  , @"tk" ), _
	( FALSE, @"*"  , @"tk" ), _
	( FALSE, @"*=" , @"tk" ), _
	( FALSE, @"+"  , @"tk" ), _
	( FALSE, @"+=" , @"tk" ), _
	( FALSE, @"++" , @"tk" ), _
	( FALSE, @","  , @"tk" ), _
	( FALSE, @"-"  , @"tk" ), _
	( FALSE, @"-=" , @"tk" ), _
	( FALSE, @"--" , @"tk" ), _
	( FALSE, @"->" , @"tk" ), _
	( FALSE, @"."  , @"tk" ), _
	( FALSE, @"...", @"tk" ), _
	( FALSE, @"/"  , @"tk" ), _
	( FALSE, @"/=" , @"tk" ), _
	( FALSE, @":"  , @"tk" ), _
	( FALSE, @";"  , @"tk" ), _
	( FALSE, @"<"  , @"tk" ), _
	( FALSE, @"<<" , @"tk" ), _
	( FALSE, @"<<=", @"tk" ), _
	( FALSE, @"<=" , @"tk" ), _
	( FALSE, @"<>" , @"tk" ), _
	( FALSE, @"="  , @"tk" ), _
	( FALSE, @"==" , @"tk" ), _
	( FALSE, @">"  , @"tk" ), _
	( FALSE, @">>" , @"tk" ), _
	( FALSE, @">>=", @"tk" ), _
	( FALSE, @">=" , @"tk" ), _
	( FALSE, @"?"  , @"tk" ), _
	( FALSE, @"@"  , @"tk" ), _
	( FALSE, @"["  , @"tk" ), _
	( FALSE, @"\"  , @"tk" ), _
	( FALSE, @"]"  , @"tk" ), _
	( FALSE, @"^"  , @"tk" ), _
	( FALSE, @"^=" , @"tk" ), _
	( FALSE, @"_"  , @"tk" ), _
	( FALSE, @"{"  , @"tk" ), _
	( FALSE, @"|"  , @"tk" ), _
	( FALSE, @"|=" , @"tk" ), _
	( FALSE, @"||" , @"tk" ), _
	( FALSE, @"}"  , @"tk" ), _
	( FALSE, @"~"  , @"tk" ), _
	( FALSE, NULL  , @"id" ), _ '' TK_ID
	( FALSE, @"auto"    , @"kw" ), _ '' C/FB keywords
	( FALSE, @"break"   , @"kw" ), _
	( FALSE, @"case"    , @"kw" ), _
	( FALSE, @"char"    , @"kw" ), _
	( FALSE, @"const"   , @"kw" ), _
	( FALSE, @"continue", @"kw" ), _
	( FALSE, @"default" , @"kw" ), _
	( FALSE, @"define"  , @"kw" ), _
	( FALSE, @"defined" , @"kw" ), _
	( FALSE, @"do"      , @"kw" ), _
	( FALSE, @"double"  , @"kw" ), _
	( FALSE, @"elif"    , @"kw" ), _
	( FALSE, @"else"    , @"kw" ), _
	( FALSE, @"endif"   , @"kw" ), _
	( FALSE, @"enum"    , @"kw" ), _
	( FALSE, @"extern"  , @"kw" ), _
	( FALSE, @"float"   , @"kw" ), _
	( FALSE, @"for"     , @"kw" ), _
	( FALSE, @"goto"    , @"kw" ), _
	( FALSE, @"if"      , @"kw" ), _
	( FALSE, @"ifdef"   , @"kw" ), _
	( FALSE, @"ifndef"  , @"kw" ), _
	( FALSE, @"include" , @"kw" ), _
	( FALSE, @"inline"  , @"kw" ), _
	( FALSE, @"int"     , @"kw" ), _
	( FALSE, @"long"    , @"kw" ), _
	( FALSE, @"pragma"  , @"kw" ), _
	( FALSE, @"register", @"kw" ), _
	( FALSE, @"restrict", @"kw" ), _
	( FALSE, @"return"  , @"kw" ), _
	( FALSE, @"short"   , @"kw" ), _
	( FALSE, @"signed"  , @"kw" ), _
	( FALSE, @"sizeof"  , @"kw" ), _
	( FALSE, @"static"  , @"kw" ), _
	( FALSE, @"struct"  , @"kw" ), _
	( FALSE, @"switch"  , @"kw" ), _
	( FALSE, @"typedef" , @"kw" ), _
	( FALSE, @"undef"   , @"kw" ), _
	( FALSE, @"union"   , @"kw" ), _
	( FALSE, @"unsigned", @"kw" ), _
	( FALSE, @"void"    , @"kw" ), _
	( FALSE, @"volatile", @"kw" ), _
	( FALSE, @"while"   , @"kw" ), _
	( FALSE, @"alias"   , @"kw" ), _  '' FB-only keywords
	( FALSE, @"and"     , @"kw" ), _
	( FALSE, @"andalso" , @"kw" ), _
	( FALSE, @"any"     , @"kw" ), _
	( FALSE, @"as"      , @"kw" ), _
	( FALSE, @"byte"    , @"kw" ), _
	( FALSE, @"byval"   , @"kw" ), _
	( FALSE, @"ctk"    , @"kw" ), _
	( FALSE, @"cdecl"   , @"kw" ), _
	( FALSE, @"cptr"    , @"kw" ), _
	( FALSE, @"declare" , @"kw" ), _
	( FALSE, @"dim"     , @"kw" ), _
	( FALSE, @"elseif"  , @"kw" ), _
	( FALSE, @"end"     , @"kw" ), _
	( FALSE, @"exit"    , @"kw" ), _
	( FALSE, @"export"  , @"kw" ), _
	( FALSE, @"field"   , @"kw" ), _
	( FALSE, @"function", @"kw" ), _
	( FALSE, @"iif"     , @"kw" ), _
	( FALSE, @"integer" , @"kw" ), _
	( FALSE, @"longint" , @"kw" ), _
	( FALSE, @"loop"    , @"kw" ), _
	( FALSE, @"mod"     , @"kw" ), _
	( FALSE, @"next"    , @"kw" ), _
	( FALSE, @"not"     , @"kw" ), _
	( FALSE, @"or"      , @"kw" ), _
	( FALSE, @"orelse"  , @"kw" ), _
	( FALSE, @"pascal"  , @"kw" ), _
	( FALSE, @"private" , @"kw" ), _
	( FALSE, @"ptr"     , @"kw" ), _
	( FALSE, @"scope"   , @"kw" ), _
	( FALSE, @"select"  , @"kw" ), _
	( FALSE, @"shared"  , @"kw" ), _
	( FALSE, @"shl"     , @"kw" ), _
	( FALSE, @"shr"     , @"kw" ), _
	( FALSE, @"single"  , @"kw" ), _
	( FALSE, @"stdcall" , @"kw" ), _
	( FALSE, @"sub"     , @"kw" ), _
	( FALSE, @"then"    , @"kw" ), _
	( FALSE, @"to"      , @"kw" ), _
	( FALSE, @"type"    , @"kw" ), _
	( FALSE, @"ubyte"   , @"kw" ), _
	( FALSE, @"uinteger", @"kw" ), _
	( FALSE, @"ulong"   , @"kw" ), _
	( FALSE, @"ulongint", @"kw" ), _
	( FALSE, @"ushort"  , @"kw" ), _
	( FALSE, @"wend"    , @"kw" ), _
	( FALSE, @"wstr"    , @"kw" ), _
	( FALSE, @"wstring" , @"kw" ), _
	( FALSE, @"xor"     , @"kw" ), _
	( FALSE, @"zstring" , @"kw" )  _
}

type ONETOKEN
	id		as short  '' TK_*
	flags		as short
	text		as zstring ptr  '' Identifiers/literals, or NULL

	'' Data type (vars, fields, params, function results)
	dtype		as integer
	subtype		as zstring ptr
end type

type TOKENBUFFER
	'' Gap buffer of tokens
	p		as ONETOKEN ptr  '' Buffer containing: front,gap,back
	front		as integer  '' Front length; the gap's offset
	gap		as integer  '' Gap length
	size		as integer  '' Front + back

	'' Static EOF token for "error recovery"
	eof		as ONETOKEN
end type

type TOKENSTATS
	maxsize		as integer  '' Highest amount of tokens at once
	reallocs	as integer  '' Buffer reallocations
	inserts		as integer
	deletes		as integer
	lookups		as integer
	movedtokens	as longint
	maxstrlen	as integer
end type

dim shared as TOKENBUFFER tk
dim shared as TOKENSTATS stats

function strDuplicate( byval s as zstring ptr ) as zstring ptr
	dim as zstring ptr p = any
	dim as integer length = any

	if( s = NULL ) then
		return NULL
	end if

	length = len( *s )
	p = callocate( length + 1 )

	if( length > 0 ) then
		memcpy( p, s, length )
	end if
	p[length] = 0

	stats.maxstrlen += length

	function = p
end function

sub tkInit( )
	tk.p = NULL
	tk.front = 0
	tk.gap = 0
	tk.size = 0

	tk.eof.id = TK_EOF
	tk.eof.flags = 0
	tk.eof.text = NULL
end sub

private function tkAccess( byval x as integer ) as ONETOKEN ptr
	stats.lookups += 1

	'' Inside end?
	if( x >= tk.front ) then
		'' Invalid?
		if( x >= tk.size ) then
			return @tk.eof
		end if
		x += tk.gap
	else
		'' Invalid?
		if( x < 0 ) then
			return @tk.eof
		end if
	end if

	function = tk.p + x
end function

sub tkEnd( )
	for i as integer = 0 to tk.size - 1
		deallocate( tkAccess( i )->text )
	next
	deallocate( tk.p )
end sub

sub tkStats( )
	print "  tokens: " & stats.maxsize & " max, " & _
		stats.reallocs & " resizes, " & _
		stats.inserts & " in, " & _
		stats.deletes & " out, " & _
		stats.lookups & " lookups, " & _
		stats.movedtokens & " tokens moved, " & _
		stats.maxstrlen & " max strlen"
end sub

function tkDumpOne( byval x as integer ) as string
	dim as ONETOKEN ptr p = any
	dim as string s

	p = tkAccess( x )
	s += str( x ) + " "
	s += "[" + *tk_info(p->id).debug + "] "

	if( p->text ) then
		s += "'" + *p->text + "'"
	else
		if( tk_info(p->id).text ) then
			s += "'" + *tk_info(p->id).text + "'"
		end if
	end if

	function = s
end function

sub tkDump( )
	for i as integer = 0 to tk.size - 1
		print tkDumpOne( i )
	next
end sub

private sub tkRawMoveTo( byval x as integer )
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
		stats.movedtokens += old - x
	elseif( x > old ) then
		'' Move gap right
		p = tk.p + old
		memmove( p, p + tk.gap, (x - old) * sizeof( ONETOKEN ) )
		stats.movedtokens += x - old
	end if

	tk.front = x
end sub

'' Insert token at current position, the current position moves forward.
private sub tkRawInsert( byval id as integer, byval text as zstring ptr )
	const NEWGAP = 512
	dim as ONETOKEN ptr p = any

	'' Make room for the new data, if necessary
	if( tk.gap = 0 ) then
		'' Reallocate the buffer, then move the back block to the
		'' end of the new buffer, so that the gap in the middle grows.
		stats.reallocs += 1
		tk.p = reallocate( tk.p, (tk.size + NEWGAP) * sizeof( ONETOKEN ) )
		p = tk.p + tk.front
		if( tk.size > tk.front ) then
			memmove( p + NEWGAP, p + tk.gap, _
			         (tk.size - tk.front) * sizeof( ONETOKEN ) )
			stats.movedtokens += tk.size - tk.front
		end if
		tk.gap = NEWGAP
	else
		p = tk.p + tk.front
	end if

	p->id = id
	p->flags = 0
	p->text = strDuplicate( text )

	tk.front += 1
	tk.gap -= 1
	tk.size += 1

	stats.inserts += 1
	if( stats.maxsize < tk.size ) then
		stats.maxsize = tk.size
	end if
end sub

sub tkInsert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr _
	)

	tkRawMoveTo( x )
	tkRawInsert( id, text )

end sub

sub tkCopy _
	( _
		byval x as integer, _
		byval first as integer, _
		byval last as integer _
	)

	for i as integer = 0 to (last - first)
		tkInsert( x + i, tkGet( first + i ), tkGetText( first + i ) )
	next

end sub

sub tkRemove( byval first as integer, byval last as integer )
	dim as ONETOKEN ptr p = any
	dim as integer delta = any

	if( first < 0 ) then
		first = 0
	end if

	if( last >= tk.size ) then
		last = tk.size - 1
	end if

	if( first > last ) then
		exit sub
	end if

	tkRawMoveTo( last + 1 )

	for i as integer = first to last
		p = tkAccess( i )
		deallocate( p->text )
	next

	delta = last - first + 1
	if( delta > tk.front ) then
		delta = tk.front
	end if

	'' Delete tokens in front of current position (backwards deletion)
	stats.deletes += delta
	tk.front -= delta
	tk.gap += delta
	tk.size -= delta
end sub

sub tkSetFlags _
	( _
		byval first as integer, _
		byval last as integer, _
		byval flags as integer _
	)

	for i as integer = first to last
		tkAccess( i )->flags = flags
	next

	'' Reset in case it got changed
	tk.eof.flags = 0

end sub

function tkGet( byval x as integer ) as integer
	function = tkAccess( x )->id
end function

function tkGetFlags( byval x as integer ) as integer
	function = tkAccess( x )->flags
end function

function tkGetText( byval x as integer ) as zstring ptr
	dim as ONETOKEN ptr p = any

	p = tkAccess( x )

	if( p->text ) then
		function = p->text
	else
		if( p->id >= TK_EXCL ) then
			assert( p->id <> TK_ID )
			function = tk_info(p->id).text
		else
			function = @""
		end if
	end if
end function

function tkGetCount( ) as integer
	function = tk.size
end function

function tkIsStmtSep( byval x as integer ) as integer
	function = tk_info(tkAccess( x )->id).is_stmtsep
end function

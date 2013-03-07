#include once "fbfrog.bi"
#include once "crt.bi"

enum
	TKFLAG_STMTSEP = &h1
	TKFLAG_PROCPTR = &h2
end enum

type TOKENINFO
	text		as zstring ptr
	debug		as zstring ptr
	flags		as integer
end type

dim shared as TOKENINFO tk_info(0 to ...) = _
{ _
	( NULL  , @"eof"                  , TKFLAG_STMTSEP ), _
	( NULL  , @"divider"              , TKFLAG_STMTSEP ), _
	( NULL  , @"#include"             , TKFLAG_STMTSEP ), _
	( NULL  , @"#define begin"        , TKFLAG_STMTSEP ), _
	( NULL  , @"#define end"          , TKFLAG_STMTSEP ), _
	( NULL  , @"struct begin"         , TKFLAG_STMTSEP ), _
	( NULL  , @"struct end"           , TKFLAG_STMTSEP ), _
	( NULL  , @"todo begin"           , TKFLAG_STMTSEP ), _
	( NULL  , @"todo end"             , TKFLAG_STMTSEP ), _
	( NULL  , @"global"               , TKFLAG_STMTSEP ), _
	( NULL  , @"externglobal"         , TKFLAG_STMTSEP ), _
	( NULL  , @"staticglobal"         , TKFLAG_STMTSEP ), _
	( NULL  , @"globalprocptr"        , TKFLAG_STMTSEP or TKFLAG_PROCPTR ), _
	( NULL  , @"externglobalprocptr"  , TKFLAG_STMTSEP or TKFLAG_PROCPTR ), _
	( NULL  , @"staticglobalprocptr"  , TKFLAG_STMTSEP or TKFLAG_PROCPTR ), _
	( NULL  , @"field"                , TKFLAG_STMTSEP ), _
	( NULL  , @"fieldprocptr"         , TKFLAG_STMTSEP or TKFLAG_PROCPTR ), _
	( NULL  , @"proc"                 , TKFLAG_STMTSEP ), _
	( NULL  , @"param"                , TKFLAG_STMTSEP ), _
	( NULL  , @"paramprocptr"         , TKFLAG_STMTSEP or TKFLAG_PROCPTR ), _
	( NULL  , @"paramvararg"          , TKFLAG_STMTSEP ), _
	( NULL  , @"todo"                 , TKFLAG_STMTSEP ), _
	( NULL  , @"byte"                 ), _
	( NULL  , @"space"                ), _
	( NULL  , @"eol"                  , TKFLAG_STMTSEP ), _
	( NULL  , @"comment"              ), _
	( NULL  , @"linecomment"          ), _
	( NULL  , @"decnum"               ), _ '' Number literals
	( NULL  , @"hexnum"               ), _
	( NULL  , @"octnum"               ), _
	( NULL  , @"string"               ), _ '' String literals
	( NULL  , @"char"                 ), _
	( NULL  , @"wstring"              ), _
	( NULL  , @"wchar"                ), _
	( NULL  , @"estring"              ), _
	( NULL  , @"echar"                ), _
	( NULL  , @"ewstring"             ), _
	( NULL  , @"ewchar"               ), _
	( @"!"  , @"tk" ), _ '' Main tokens
	( @"!=" , @"tk" ), _
	( @"#"  , @"tk" ), _
	( @"##" , @"tk" ), _
	( @"%"  , @"tk" ), _
	( @"%=" , @"tk" ), _
	( @"&"  , @"tk" ), _
	( @"&=" , @"tk" ), _
	( @"&&" , @"tk" ), _
	( @"("  , @"tk" ), _
	( @")"  , @"tk" ), _
	( @"*"  , @"tk" ), _
	( @"*=" , @"tk" ), _
	( @"+"  , @"tk" ), _
	( @"+=" , @"tk" ), _
	( @"++" , @"tk" ), _
	( @","  , @"tk" ), _
	( @"-"  , @"tk" ), _
	( @"-=" , @"tk" ), _
	( @"--" , @"tk" ), _
	( @"->" , @"tk" ), _
	( @"."  , @"tk" ), _
	( @"...", @"tk" ), _
	( @"/"  , @"tk" ), _
	( @"/=" , @"tk" ), _
	( @":"  , @"tk" ), _
	( @";"  , @"tk" ), _
	( @"<"  , @"tk" ), _
	( @"<<" , @"tk" ), _
	( @"<<=", @"tk" ), _
	( @"<=" , @"tk" ), _
	( @"<>" , @"tk" ), _
	( @"="  , @"tk" ), _
	( @"==" , @"tk" ), _
	( @">"  , @"tk" ), _
	( @">>" , @"tk" ), _
	( @">>=", @"tk" ), _
	( @">=" , @"tk" ), _
	( @"?"  , @"tk" ), _
	( @"@"  , @"tk" ), _
	( @"["  , @"tk" ), _
	( @"\"  , @"tk" ), _
	( @"]"  , @"tk" ), _
	( @"^"  , @"tk" ), _
	( @"^=" , @"tk" ), _
	( @"_"  , @"tk" ), _
	( @"{"  , @"tk" ), _
	( @"|"  , @"tk" ), _
	( @"|=" , @"tk" ), _
	( @"||" , @"tk" ), _
	( @"}"  , @"tk" ), _
	( @"~"  , @"tk" ), _
	( NULL  , @"id" ), _ '' TK_ID
	( @"auto"    , @"kw" ), _ '' C/FB keywords
	( @"break"   , @"kw" ), _
	( @"case"    , @"kw" ), _
	( @"char"    , @"kw" ), _
	( @"const"   , @"kw" ), _
	( @"continue", @"kw" ), _
	( @"default" , @"kw" ), _
	( @"define"  , @"kw" ), _
	( @"defined" , @"kw" ), _
	( @"do"      , @"kw" ), _
	( @"double"  , @"kw" ), _
	( @"elif"    , @"kw" ), _
	( @"else"    , @"kw" ), _
	( @"endif"   , @"kw" ), _
	( @"enum"    , @"kw" ), _
	( @"extern"  , @"kw" ), _
	( @"float"   , @"kw" ), _
	( @"for"     , @"kw" ), _
	( @"goto"    , @"kw" ), _
	( @"if"      , @"kw" ), _
	( @"ifdef"   , @"kw" ), _
	( @"ifndef"  , @"kw" ), _
	( @"include" , @"kw" ), _
	( @"inline"  , @"kw" ), _
	( @"int"     , @"kw" ), _
	( @"long"    , @"kw" ), _
	( @"pragma"  , @"kw" ), _
	( @"register", @"kw" ), _
	( @"restrict", @"kw" ), _
	( @"return"  , @"kw" ), _
	( @"short"   , @"kw" ), _
	( @"signed"  , @"kw" ), _
	( @"sizeof"  , @"kw" ), _
	( @"static"  , @"kw" ), _
	( @"struct"  , @"kw" ), _
	( @"switch"  , @"kw" ), _
	( @"typedef" , @"kw" ), _
	( @"undef"   , @"kw" ), _
	( @"union"   , @"kw" ), _
	( @"unsigned", @"kw" ), _
	( @"void"    , @"kw" ), _
	( @"volatile", @"kw" ), _
	( @"while"   , @"kw" ), _
	( @"alias"   , @"kw" ), _  '' FB-only keywords
	( @"and"     , @"kw" ), _
	( @"andalso" , @"kw" ), _
	( @"any"     , @"kw" ), _
	( @"as"      , @"kw" ), _
	( @"byte"    , @"kw" ), _
	( @"byval"   , @"kw" ), _
	( @"ctk"    , @"kw" ), _
	( @"cdecl"   , @"kw" ), _
	( @"cptr"    , @"kw" ), _
	( @"declare" , @"kw" ), _
	( @"dim"     , @"kw" ), _
	( @"elseif"  , @"kw" ), _
	( @"end"     , @"kw" ), _
	( @"exit"    , @"kw" ), _
	( @"export"  , @"kw" ), _
	( @"field"   , @"kw" ), _
	( @"function", @"kw" ), _
	( @"iif"     , @"kw" ), _
	( @"integer" , @"kw" ), _
	( @"longint" , @"kw" ), _
	( @"loop"    , @"kw" ), _
	( @"mod"     , @"kw" ), _
	( @"next"    , @"kw" ), _
	( @"not"     , @"kw" ), _
	( @"or"      , @"kw" ), _
	( @"orelse"  , @"kw" ), _
	( @"pascal"  , @"kw" ), _
	( @"private" , @"kw" ), _
	( @"ptr"     , @"kw" ), _
	( @"scope"   , @"kw" ), _
	( @"select"  , @"kw" ), _
	( @"shared"  , @"kw" ), _
	( @"shl"     , @"kw" ), _
	( @"shr"     , @"kw" ), _
	( @"single"  , @"kw" ), _
	( @"stdcall" , @"kw" ), _
	( @"sub"     , @"kw" ), _
	( @"then"    , @"kw" ), _
	( @"to"      , @"kw" ), _
	( @"type"    , @"kw" ), _
	( @"ubyte"   , @"kw" ), _
	( @"uinteger", @"kw" ), _
	( @"ulong"   , @"kw" ), _
	( @"ulongint", @"kw" ), _
	( @"ushort"  , @"kw" ), _
	( @"wend"    , @"kw" ), _
	( @"wstr"    , @"kw" ), _
	( @"wstring" , @"kw" ), _
	( @"xor"     , @"kw" ), _
	( @"zstring" , @"kw" )  _
}

#if ubound( tk_info ) < TK__COUNT - 1
#error "you forgot to update the tk_info() table again!"
#endif

function tkInfoText( byval tk as integer ) as zstring ptr
	function = tk_info(tk).text
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type MAPENTRY
	base		as integer  '' (location - base) = line number
	filename	as zstring ptr
end type

type TKBUFFER
	'' Gap buffer of tokens
	p		as ONETOKEN ptr  '' Buffer containing: front,gap,back
	front		as integer  '' Front length; the gap's offset
	gap		as integer  '' Gap length
	size		as integer  '' Front + back

	'' Static EOF token for out-of-bounds accesses
	eof		as ONETOKEN

	map		as TLIST  '' MAPENTRYs
	location	as integer
end type

type TKSTATS
	maxsize		as integer  '' Highest amount of tokens at once
	lookups		as integer
	moved		as integer
end type

dim shared as TKBUFFER tk
dim shared as TKSTATS stats

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

	function = p
end function

sub tkInit( )
	tk.p = NULL
	tk.front = 0
	tk.gap = 0
	tk.size = 0

	tk.eof.id = TK_EOF
	tk.eof.text = NULL
	tk.eof.dtype = TYPE_NONE
	tk.eof.subtype = NULL
	tk.eof.location = -1

	listInit( @tk.map, sizeof( MAPENTRY ) )
	tk.location = 0
end sub

function tkAccess( byval x as integer ) as ONETOKEN ptr
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
	dim as MAPENTRY ptr entry = any

	entry = listGetHead( @tk.map )
	while( entry )
		deallocate( entry->filename )
		entry = listGetNext( entry )
	wend
	listEnd( @tk.map )

	tkRemove( 0, tk.size - 1 )
	deallocate( tk.p )
end sub

sub tkStats( )
	print "tokens: " & _
		stats.maxsize & " max, " & _
		stats.lookups & " lookups, " & _
		stats.moved & " moved"
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

private sub hMoveTo( byval x as integer )
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
		stats.moved += old - x
	elseif( x > old ) then
		'' Move gap right
		p = tk.p + old
		memmove( p, p + tk.gap, (x - old) * sizeof( ONETOKEN ) )
		stats.moved += x - old
	end if

	tk.front = x
end sub

sub tkDtor( byval p as ONETOKEN ptr )
	deallocate( p->text )
	deallocate( p->subtype )
	deallocate( p->comment )
end sub

function tkGetCount( ) as integer
	function = tk.size
end function

'' Insert new token in front of token at the given position,
'' so that the new token ends up at that position
sub tkInsert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr _
	)

	const NEWGAP = 512
	dim as ONETOKEN ptr p = any

	'' Move gap in front of the position
	hMoveTo( x )

	'' Make room for the new data, if necessary
	if( tk.gap = 0 ) then
		'' Reallocate the buffer, then move the back block to the
		'' end of the new buffer, so that the gap in the middle grows.
		tk.p = reallocate( tk.p, (tk.size + NEWGAP) * sizeof( ONETOKEN ) )
		p = tk.p + tk.front
		if( tk.size > tk.front ) then
			memmove( p + NEWGAP, p + tk.gap, _
			         (tk.size - tk.front) * sizeof( ONETOKEN ) )
			stats.moved += tk.size - tk.front
		end if
		tk.gap = NEWGAP
	else
		p = tk.p + tk.front
	end if

	p->id = id
	p->text = strDuplicate( text )
	p->dtype = TYPE_NONE
	p->subtype = NULL
	p->location = -1
	p->comment = NULL

	'' Extend front part of the buffer
	tk.front += 1
	tk.gap -= 1
	tk.size += 1

	if( stats.maxsize < tk.size ) then
		stats.maxsize = tk.size
	end if

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

	for i as integer = first to last
		tkDtor( tkAccess( i ) )
	next

	delta = last - first + 1

	'' Gap is in front of first token to delete?
	if( tk.front = first ) then
		'' Then do a forward deletion
		assert( delta <= (tk.size - tk.front) )
	else
		'' Otherwise, move the gap behind the last token,
		'' and do a backwards deletion
		hMoveTo( last + 1 )
		assert( delta <= tk.front )
		tk.front -= delta
	end if

	tk.gap += delta
	tk.size -= delta
end sub

function tkGet( byval x as integer ) as integer
	function = tkAccess( x )->id
end function

function tkIsStmtSep( byval x as integer ) as integer
	function = ((tk_info(tkGet( x )).flags and TKFLAG_STMTSEP) <> 0)
end function

function tkIsProcPtr( byval x as integer ) as integer
	function = ((tk_info(tkGet( x )).flags and TKFLAG_PROCPTR) <> 0)
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

sub tkSetType _
	( _
		byval x as integer, _
		byval dtype as integer, _
		byval subtype as zstring ptr _
	)

	dim as ONETOKEN ptr p = any

	p = tkAccess( x )
	if( p->id <> TK_EOF ) then
		p->dtype = dtype
		p->subtype = strDuplicate( subtype )
	end if

end sub

function tkGetType( byval x as integer ) as integer
	function = tkAccess( x )->dtype
end function

function tkGetSubtype( byval x as integer ) as zstring ptr
	function = tkAccess( x )->subtype
end function

sub tkLocationNewFile( byval filename as zstring ptr )
	dim as MAPENTRY ptr entry = any

	entry = listAppend( @tk.map )
	entry->base = tk.location
	entry->filename = strDuplicate( filename )
end sub

function tkLocationNewLine( ) as integer
	function = tk.location
	tk.location += 1
end function

sub tkSetLocation( byval x as integer, byval location as integer )
	dim as ONETOKEN ptr p = any
	p = tkAccess( x )
	if( p->id <> TK_EOF ) then
		p->location = location
	end if
end sub

function tkHasSourceLocation( byval x as integer ) as integer
	function = (tkAccess( x )->location >= 0)
end function

'' Find the map entry that "contains" this token's location
'' (assuming there always is one)
private function hLookupLocation( byval location as integer ) as MAPENTRY ptr
	dim as MAPENTRY ptr entry = any

	entry = listGetTail( @tk.map )
	while( entry->base > location )
		entry = listGetPrev( entry )
	wend

	function = entry
end function

function tkGetSourceFile( byval x as integer ) as zstring ptr
	assert( tkHasSourceLocation( x ) )
	function = hLookupLocation( tkAccess( x )->location )->filename
end function

function tkGetLineNum( byval x as integer ) as integer
	dim as integer location = any

	assert( tkHasSourceLocation( x ) )
	location = tkAccess( x )->location

	function = location - hLookupLocation( location )->base
end function

#include once "fbfrog.bi"
#include once "crt.bi"

declare sub astDtorTK( byval n as ASTNODE ptr )

dim shared as sub( byval as ASTNODE ptr ) astdtors(0 to ASTCLASS__COUNT-1) = _
{ _
	@astDtorTK _  '' ASTCLASS_TK
}

dim shared as zstring ptr token_text(0 to (TK__COUNT - 1)) = _
{ _
	@"<todo>"       , _
	@"<byte>"       , _
	@"<eol>"        , _
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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function astNew( byval class_ as integer ) as ASTNODE ptr
	dim as ASTNODE ptr n = any

	n = callocate( sizeof( ASTNODE ) )
	n->class = class_

	function = n
end function

sub astDelete( byval t as ASTNODE ptr )
	if( astdtors(t->class) ) then
		astdtors(t->class)( t )
	end if
	deallocate( t )
end sub

function strDuplicate( byval s as zstring ptr ) as zstring ptr
	dim as zstring ptr p = any
	dim as integer length = any

	if( s = NULL ) then exit function

	length = len( *s )
	p = callocate( length + 1 )

	if( length > 0 ) then
		memcpy( p, s, length )
	end if
	p[length] = 0

	function = p
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function astNewBLOCK( ) as ASTNODE ptr
	function = astNew( ASTCLASS_BLOCK )
end function

function astGetNodeCount( byval block as ASTNODE ptr ) as integer
	dim as ASTNODE ptr n = any
	dim as integer count = any

	count = 0
	n = block->block.head
	while( n )
		count += 1
		n = n->next
	wend

	function = count
end function

sub astAddInFront _
	( _
		byval block as ASTNODE ptr, _
		byval ref as ASTNODE ptr, _
		byval n as ASTNODE ptr _
	)
end sub

sub astAddBehind _
	( _
		byval block as ASTNODE ptr, _
		byval ref as ASTNODE ptr, _
		byval n as ASTNODE ptr _
	)
end sub

sub astAppend( byval block as ASTNODE ptr, byval n as ASTNODE ptr )
	if( block->block.head = NULL ) then
		block->block.head = n
	end if
	if( block->block.tail ) then
		block->block.tail->next = n
	end if
	n->prev = block->block.tail
	n->next = NULL
	block->block.tail = n
end sub

function astContains _
	( _
		byval t as ASTNODE ptr, _
		byval n as ASTNODE ptr _
	) as integer

	dim as ASTNODE ptr i = any

	if( t = n ) then
		return TRUE
	end if

	if( t->class = ASTCLASS_BLOCK ) then
		i = t->block.head
		while( i )
			if( astContains( i, n ) ) then
				return TRUE
			end if
			i = i->next
		wend
	end if

	function = FALSE
end function

sub astRemove( byval block as ASTNODE ptr, byval n as ASTNODE ptr )
	dim as ASTNODE ptr nxt = any, prv = any
	assert( astContains( block, n ) )

	nxt = n->next
	prv = n->prev
	if( prv ) then
		prv->next = nxt
	else
		block->block.head = nxt
	end if
	if( nxt ) then
		nxt->prev = prv
	else
		block->block.tail = prv
	end if

	astDelete( n )
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function astNewTK( byval id as integer, byval text as zstring ptr ) as ASTNODE ptr
	dim as ASTNODE ptr n = any

	n = astNew( ASTCLASS_TK )
	n->tk.id = id
	n->tk.text = strDuplicate( text )

	function = n
end function

private sub astDtorTK( byval n as ASTNODE ptr )
	deallocate( n->tk.text )
end sub

function astIsTK( byval n as ASTNODE ptr, byval id as integer ) as integer
	if( n ) then
		if( n->class = ASTCLASS_TK ) then
			function = (n->tk.id = id) or (id < 0)
		end if
	end if
end function

function astGetText( byval n as ASTNODE ptr ) as zstring ptr
	assert( astIsTK( n ) )
	if( n->tk.text ) then
		function = n->tk.text
	else
		function = token_text(n->tk.id)
	end if
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function astNewVARDECL _
	( _
		byval id as zstring ptr, _
		byval dtype as integer, _
		byval subtype as zstring ptr _
	) as ASTNODE ptr

	dim as ASTNODE ptr n = any

	n = astNew( ASTCLASS_VARDECL )
	n->vardecl.id = strDuplicate( id )
	n->vardecl.dtype = dtype
	n->vardecl.subtype = strDuplicate( subtype )

	function = n
end function

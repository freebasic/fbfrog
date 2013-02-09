#include once "fbfrog.bi"
#include once "crt.bi"

declare sub astDtorTK( byval n as ASTNODE ptr )

type ASTSTUFF
	dtors(0 to ASTCLASS__COUNT-1)	as sub( byval as ASTNODE ptr )

	statements	as TLIST '' ASTNODE ptr's
end type

dim shared as ASTSTUFF ast = _
( _
	{ _
		@astDtorTK _  '' ASTCLASS_TK
	} _
)

sub astInit( )
	listInit( @ast.statements, sizeof( ASTNODE ptr ) )
end sub

sub astEnd( )
	dim as ASTNODE ptr ptr stmt = any

	stmt = listGetHead( @ast.statements )
	while( stmt )
		astDelete( *stmt )
		stmt = listGetNext( stmt )
	wend

	listEnd( @ast.statements )
end sub

sub astAdd( byval t as ASTNODE ptr )
	dim as ASTNODE ptr ptr stmt = any

	stmt = listAppend( @ast.statements )
	*stmt = t
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function astNew( byval class_ as integer ) as ASTNODE ptr
	dim as ASTNODE ptr n = any

	n = callocate( sizeof( ASTNODE ) )
	n->class = class_

	function = n
end function

sub astDelete( byval t as ASTNODE ptr )
	if( ast.dtors(t->class) ) then
		ast.dtors(t->class)( t )
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

dim shared as zstring ptr token_text(0 to (TK__COUNT - 1)) = _
{ _
	@"<eof>"        , _
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

function astNewTK( ) as ASTNODE ptr
	dim as ASTNODE ptr n = any

	n = astNew( ASTCLASS_TK )
	listInit( @n->tk, sizeof( ASTTOKEN ) )

	function = n
end function

private sub astTokenDtor( byval tk as ASTTOKEN ptr )
	deallocate( tk->text )
end sub

private sub astDtorTK( byval n as ASTNODE ptr )
	dim as ASTTOKEN ptr tk = any

	'' Free token text if any tokens are left
	tk = listGetHead( @n->tk )
	while( tk )
		astTokenDtor( tk )
		tk = listGetNext( tk )
	wend

	listEnd( @n->tk )
end sub

function astTkAppend _
	( _
		byval n as ASTNODE ptr, _
		byval id as integer, _
		byval text as zstring ptr _
	) as ASTTOKEN ptr

	dim as ASTTOKEN ptr tk = any

	assert( n->class = ASTCLASS_TK )

	tk = listAppend( @n->tk )
	tk->id = id
	tk->text = strDuplicate( text )

	function = tk
end function

#if __FB_DEBUG__
private function astTkContainsToken _
	( _
		byval n as ASTNODE ptr, _
		byval tk as ASTTOKEN ptr _
	) as integer

	dim as ASTTOKEN ptr i = any

	i = listGetHead( @n->tk )
	while( i )
		if( i = tk ) then
			return TRUE
		end if
		i = listGetNext( i )
	wend

	function = FALSE
end function
#endif

sub astTkRemove( byval n as ASTNODE ptr, byval tk as ASTTOKEN ptr )
	assert( astTkContainsToken( n, tk ) )
	astTokenDtor( tk )
	listDelete( @n->tk, tk )
end sub

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

#include once "fbfrog.bi"
#include once "crt.bi"

dim shared as zstring ptr token_text(0 to (TK__COUNT - 1)) = _
{ _
	@"<block>"      , _
	@"<ppinclude>"  , _
	@"<struct>"     , _
	@"<procdecl>"   , _
	@"<vardecl>"    , _
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

sub astDump( byval n as ASTNODE ptr )
	static as integer reclevel
	dim as ASTNODE ptr i = any

	if( reclevel > 0 ) then
		print string( reclevel, !"\t" );
	end if

	if( n = NULL ) then
		print "[NULL]"
		exit sub
	end if

	#if 1
		print "[";hex( n, 8 );"] ";
	#endif

	select case( n->id )
	case TK_BLOCK
		print "block (" & astCount( n ) & " nodes)"
		reclevel += 1

		i = n->block.head
		while( i )
			astDump( i )
			i = i->next
		wend

		reclevel -= 1
		print "end block"

	case TK_ID
		print *n->text

	case TK_STRING
		print """";*n->text;""""

	case TK_PPINCLUDE
		print "#include """;*n->text;""""

	case else
		if( (n->id >= 0) and (n->id < TK__COUNT) ) then
			print *token_text(n->id)
		else
			print "invalid id: " & n->id
		end if
	end select
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

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

function astNew _
	( _
		byval id as integer, _
		byval text as zstring ptr = NULL _
	) as ASTNODE ptr

	dim as ASTNODE ptr n = any

	n = callocate( sizeof( ASTNODE ) )
	n->id = id
	n->text = strDuplicate( text )

	function = n
end function

sub astDelete( byval n as ASTNODE ptr )
	dim as ASTNODE ptr i = any, nxt = any

	if( n->id = TK_BLOCK ) then
		i = n->block.head
		while( i )
			nxt = i->next
			astDelete( i )
			i = nxt
		wend
	end if

	deallocate( n->text )
	deallocate( n )
end sub

function astGet( byval n as ASTNODE ptr ) as integer
	if( n ) then
		function = n->id
	else
		function = -1
	end if
end function

function astGetText( byval n as ASTNODE ptr ) as zstring ptr
	if( n->text ) then
		function = n->text
	else
		function = token_text(n->id)
	end if
end function

function astIsAtBOL( byval i as ASTNODE ptr ) as integer
	if( i = NULL ) then
		return TRUE
	end if

	'' BOF?
	if( i->prev = NULL ) then
		return TRUE
	end if

	'' BOL?
	function = (i->prev->id = TK_EOL)
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function astNewBLOCK( ) as ASTNODE ptr
	function = astNew( TK_BLOCK )
end function

function astCount( byval block as ASTNODE ptr ) as integer
	dim as ASTNODE ptr n = any
	dim as integer count = any

	assert( astIsBLOCK( block ) )

	count = 0
	n = block->block.head
	while( n )
		count += 1
		n = n->next
	wend

	function = count
end function

'' Link in N in front of REF
sub astInsert _
	( _
		byval block as ASTNODE ptr, _
		byval n as ASTNODE ptr, _
		byval ref as ASTNODE ptr _
	)

	assert( astIsBLOCK( block ) )
	assert( astContains( block, ref ) )

	n->prev = ref->prev
	n->next = ref

	if( ref->prev ) then
		ref->prev->next = n
	else
		block->block.head = n
	end if
	ref->prev = n

end sub

sub astAppend( byval block as ASTNODE ptr, byval n as ASTNODE ptr )
	assert( astIsBLOCK( block ) )
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

	assert( astIsBLOCK( t ) )

	i = t->block.head
	while( i )
		if( i = n ) then
			return TRUE
		end if
		i = i->next
	wend

	function = FALSE
end function

'' Delete COUNT nodes, starting from N
'' result = the node that ends up in N's place after the deletion, if any
function astRemove _
	( _
		byval block as ASTNODE ptr, _
		byval n as ASTNODE ptr, _
		byval count as integer = 1 _
	) as ASTNODE ptr

	dim as ASTNODE ptr nxt = any, prv = any
	assert( astIsBLOCK( block ) )
	assert( astContains( block, n ) )

	while( (n <> NULL) and (count > 0) )
		'' Link out N
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

		'' Go to next node, if any, and maybe delete that too
		n = nxt
		count -= 1
	wend

	function = n
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function astNewVARDECL _
	( _
		byval id as zstring ptr, _
		byval dtype as integer, _
		byval subtype as zstring ptr _
	) as ASTNODE ptr

	dim as ASTNODE ptr n = any

	n = astNew( TK_VARDECL )
	n->text = strDuplicate( id )
	n->vardecl.dtype = dtype
	n->vardecl.subtype = strDuplicate( subtype )

	function = n
end function

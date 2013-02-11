#include once "fbfrog.bi"
#include once "crt.bi"

dim shared as zstring ptr token_text(0 to (TK__COUNT - 1)) = _
{ _
	@"<file>"       , _
	@"<ppdefine>"   , _
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
	case TK_FILE
		print "file: " + *n->text

	case TK_PPINCLUDE
		print "#include """;*n->text;""""

	case TK_PPDEFINE
		print "#define ";*n->text

	case TK_ID
		print *n->text

	case TK_STRING
		print """";*n->text;""""

	case else
		if( (n->id >= 0) and (n->id < TK__COUNT) ) then
			print *token_text(n->id)
		else
			print "invalid id: " & n->id
		end if
	end select

	'' Children, if any
	reclevel += 1
	i = n->head
	while( i )
		astDump( i )
		i = i->next
	wend
	reclevel -= 1
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

	i = n->head
	while( i )
		nxt = i->next
		astDelete( i )
		i = nxt
	wend

	deallocate( n->subtype )
	deallocate( n->text )
	deallocate( n )
end sub

function astClone( byval n as ASTNODE ptr ) as ASTNODE ptr
	dim as ASTNODE ptr c = any

	c = astNew( n->id, n->text )

	c->dtype = n->dtype
	c->subtype = strDuplicate( n->subtype )

	astCloneInto( c, n->head, n->tail )

	function = c
end function

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
		if( n->id >= TK_EXCL ) then
			assert( n->id <> TK_ID )
			function = token_text(n->id)
		else
			function = @""
		end if
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

'' Returns the last token in front of EOL/EOF, or NULL if starting at EOL/EOF,
'' i.e. there is no other token coming in between anymore.
function astFindLastInLine( byval i as ASTNODE ptr ) as ASTNODE ptr
	if( i = NULL ) then
		return NULL
	end if

	if( i->id = TK_EOL ) then
		return NULL
	end if

	while( i->next )
		if( i->next->id = TK_EOL ) then
			exit while
		end if
		i = i->next
	wend

	function = i
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub astCloneInto _
	( _
		byval parent as ASTNODE ptr, _
		byval head as ASTNODE ptr, _
		byval tail as ASTNODE ptr _
	)

	while( head <> NULL )
		astAppend( parent, astClone( head ) )
		if( head = tail ) then
			exit while
		end if
		head = head->next
	wend

end sub

'' Link in N in front of REF
sub astInsert _
	( _
		byval parent as ASTNODE ptr, _
		byval n as ASTNODE ptr, _
		byval ref as ASTNODE ptr _
	)

	assert( astContains( parent, ref ) )

	n->prev = ref->prev
	n->next = ref

	if( ref->prev ) then
		ref->prev->next = n
	else
		parent->head = n
	end if
	ref->prev = n

end sub

sub astAppend( byval parent as ASTNODE ptr, byval n as ASTNODE ptr )
	if( parent->head = NULL ) then
		parent->head = n
	end if
	if( parent->tail ) then
		parent->tail->next = n
	end if
	n->prev = parent->tail
	n->next = NULL
	parent->tail = n
end sub

function astContains _
	( _
		byval tree as ASTNODE ptr, _
		byval lookfor as ASTNODE ptr _
	) as integer

	dim as ASTNODE ptr i = any

	if( tree = lookfor ) then
		return TRUE
	end if

	i = tree->head
	while( i )
		if( i = lookfor ) then
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
		byval parent as ASTNODE ptr, _
		byval n as ASTNODE ptr, _
		byval count as integer = 1 _
	) as ASTNODE ptr

	dim as ASTNODE ptr nxt = any, prv = any
	assert( astContains( parent, n ) )

	while( (n <> NULL) and (count > 0) )
		'' Link out N
		nxt = n->next
		prv = n->prev
		if( prv ) then
			prv->next = nxt
		else
			parent->head = nxt
		end if
		if( nxt ) then
			nxt->prev = prv
		else
			parent->tail = prv
		end if

		astDelete( n )

		'' Go to next node, if any, and maybe delete that too
		n = nxt
		count -= 1
	wend

	function = n
end function

function astRemoveUntilBehindEol _
	( _
		byval parent as ASTNODE ptr, _
		byval i as ASTNODE ptr _
	) as ASTNODE ptr

	dim as integer saw_eol = any
	assert( astContains( parent, i ) )

	saw_eol = FALSE
	while( (i <> NULL) and (not saw_eol) )
		saw_eol = (i->id = TK_EOL)
		i = astRemove( parent, i )
	wend

	function = i
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function astNewVARDECL _
	( _
		byval id as zstring ptr, _
		byval dtype as integer, _
		byval subtype as zstring ptr _
	) as ASTNODE ptr

	dim as ASTNODE ptr n = any

	n = astNew( TK_VARDECL, id )
	n->dtype = dtype
	n->subtype = strDuplicate( subtype )

	function = n
end function

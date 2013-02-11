#include once "fbfrog.bi"
#include once "crt.bi"

type ASTSTATS
	totalnodes	as integer
	livenodes	as integer
	maxnodes	as integer
	maxstrlen	as integer
end type

dim shared as ASTSTATS stats

dim shared as ASTINFO ast_info(0 to (TK__COUNT - 1)) = _
{ _
	( TRUE , NULL  , @"file"        ), _
	( TRUE , NULL  , @"#include"    ), _
	( TRUE , NULL  , @"#define"     ), _
	( TRUE , NULL  , @"struct"      ), _
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
	( FALSE, @"cast"    , @"kw" ), _
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

sub astDump( byval n as ASTNODE ptr )
	static as integer reclevel
	dim as ASTNODE ptr i = any
	dim as string s

	if( reclevel > 0 ) then
		print space( reclevel * 4 );
	end if

	if( n = NULL ) then
		print "[NULL]"
		exit sub
	end if

	if( (n->id < 0) or (n->id >= TK__COUNT) ) then
		print "[invalid id: " & n->id & "]"
		exit sub
	end if

	#if 0
		print "[";hex( n, 8 );"] ";
	#endif

	s = *ast_info(n->id).debug

	'' Align if there are siblings
	if( (reclevel > 0) and ((n->next <> NULL) or (n->prev <> NULL)) ) then
		s += space( 8 - len( s ) )
	end if
	s = "[" + s + "] "

	if( n->text ) then
		s += "'" + *n->text + "'"
	else
		if( ast_info(n->id).text ) then
			s += "'" + *ast_info(n->id).text + "'"
		end if
	end if

	print s

	'' Children, if any
	reclevel += 1
	i = n->head
	while( i )
		astDump( i )
		i = i->next
	wend
	reclevel -= 1
end sub

sub astStats( )
	print "ast nodes: " & stats.maxnodes & " max, " & stats.totalnodes & " total"
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

	stats.maxstrlen += length

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

	stats.totalnodes += 1
	stats.livenodes += 1
	if( stats.maxnodes < stats.livenodes ) then
		stats.maxnodes = stats.livenodes
	end if

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

	stats.livenodes -= 1
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
			function = ast_info(n->id).text
		else
			function = @""
		end if
	end if
end function

function astIsStmtSep( byval n as ASTNODE ptr ) as integer
	if( n ) then
		function = ast_info(n->id).is_stmtsep
	else
		function = TRUE
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
	function = astIsStmtSep( i->prev )
end function

'' Returns the last token in front of EOL/EOF, or NULL if starting at EOL/EOF,
'' i.e. there is no other token coming in between anymore.
function astFindLastInLine( byval i as ASTNODE ptr ) as ASTNODE ptr
	if( i = NULL ) then
		return NULL
	end if

	if( astIsStmtSep( i ) ) then
		return NULL
	end if

	while( i->next )
		if( astIsStmtSep( i->next ) ) then
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
		saw_eol = astIsStmtSep( i )
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

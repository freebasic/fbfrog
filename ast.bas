'' AST build up/helper functions

#include once "fbfrog.bi"

function typeToSigned( byval dtype as integer ) as integer
	select case( typeGetDtAndPtr( dtype ) )
	case TYPE_UBYTE, TYPE_USHORT, TYPE_ULONG, TYPE_ULONGINT
		dtype = typeGetConst( dtype ) or (typeGetDt( dtype ) - 1)
	end select
	function = dtype
end function

function typeToUnsigned( byval dtype as integer ) as integer
	select case( typeGetDtAndPtr( dtype ) )
	case TYPE_BYTE, TYPE_SHORT, TYPE_LONG, TYPE_LONGINT
		dtype = typeGetConst( dtype ) or (typeGetDt( dtype ) + 1)
	end select
	function = dtype
end function

function typeIsFloat( byval dtype as integer ) as integer
	select case( typeGetDtAndPtr( dtype ) )
	case TYPE_SINGLE, TYPE_DOUBLE
		function = TRUE
	end select
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type ASTNODEINFO
	name		as zstring * 16
end type

dim shared as ASTNODEINFO astnodeinfo(0 to ...) = _
{ _
	( "nop"      ), _
	( "group"    ), _
	( "verblock" ), _
	( "divider"  ), _
	( "download" ), _
	( "extract"  ), _
	( "copyfile" ), _
	( "file"     ), _
	( "dir"      ), _
	( "expand"   ), _
	( "remove"   ), _
	( "#include" ), _
	( "#define"  ), _
	( "#undef"   ), _
	( "#if"      ), _
	( "#elseif"  ), _
	( "#else"    ), _
	( "#endif"   ), _
	( "struct"  ), _
	( "union"   ), _
	( "enum"    ), _
	( "typedef" ), _
	( "structfwd" ), _
	( "unionfwd" ), _
	( "enumfwd" ), _
	( "var"     ), _
	( "field"   ), _
	( "enumconst" ), _
	( "proc"    ), _
	( "param"   ), _
	( "array"   ), _
	( "dimension" ), _
	( "externbegin" ), _
	( "externend" ), _
	( "macrobody" ), _
	( "macroparam" ), _
	( "tk"      ), _
	( "const"   ), _
	( "id"      ), _
	( "text"    ), _
	( "string"  ), _
	( "char"    ), _
	( "wildcard" ), _
	( "dos"     ), _
	( "linux"   ), _
	( "win32"   ), _
	( "uop"     ), _
	( "bop"     ), _
	( "iif"     ), _
	( "ppmerge" ), _
	( "call"    ), _
	( "frogfile" ) _
}

#assert ubound( astnodeinfo ) = ASTCLASS__COUNT - 1

namespace aststats
	dim shared as integer maxnodes, livenodes, maxlivenodes
	dim shared as integer foldpasses, minfoldpasses, maxfoldpasses
end namespace

sub astPrintStats( )
	using aststats
	print "ast nodes: " & _
		maxlivenodes & " max (" + hMakePrettyByteSize( maxlivenodes * sizeof( ASTNODE ) ) + "), " & _
		maxnodes &   " total"
	print "ast folding passes: min " & minfoldpasses & ", max " & maxfoldpasses & ", total " & foldpasses
end sub

function astNew overload( byval class_ as integer ) as ASTNODE ptr
	dim as ASTNODE ptr n = callocate( sizeof( ASTNODE ) )
	n->class = class_

	aststats.maxnodes += 1
	aststats.livenodes += 1
	if( aststats.maxlivenodes < aststats.livenodes ) then
		aststats.maxlivenodes = aststats.livenodes
	end if

	function = n
end function

function astNew overload _
	( _
		byval class_ as integer, _
		byval text as zstring ptr _
	) as ASTNODE ptr

	var n = astNew( class_ )
	n->text = strDuplicate( text )

	function = n
end function

function astNewUOP _
	( _
		byval op as integer, _
		byval l as ASTNODE ptr _
	) as ASTNODE ptr

	var n = astNew( ASTCLASS_UOP )
	n->l = l
	n->op = op

	function = n
end function

function astNewBOP _
	( _
		byval op as integer, _
		byval l as ASTNODE ptr, _
		byval r as ASTNODE ptr _
	) as ASTNODE ptr

	var n = astNew( ASTCLASS_BOP )
	n->l = l
	n->r = r
	n->op = op

	function = n
end function

function astNewIIF _
	( _
		byval cond as ASTNODE ptr, _
		byval l as ASTNODE ptr, _
		byval r as ASTNODE ptr _
	) as ASTNODE ptr

	var n = astNew( ASTCLASS_IIF )
	n->expr = cond
	n->l = l
	n->r = r

	function = n
end function

function astNewGROUP( ) as ASTNODE ptr
	function = astNew( ASTCLASS_GROUP )
end function

function astNewGROUP( byval child as ASTNODE ptr ) as ASTNODE ptr
	var n = astNewGROUP( )
	astAppend( n, child )
	function = n
end function

function astNewVERBLOCK overload _
	( _
		byval verexpr1 as ASTNODE ptr, _
		byval verexpr2 as ASTNODE ptr, _
		byval child as ASTNODE ptr _
	) as ASTNODE ptr

	var n = astNew( ASTCLASS_VERBLOCK )

	if( verexpr2 ) then
		assert( verexpr1 )
		n->expr = astNewBOP( ASTOP_OR, verexpr1, verexpr2 )
	else
		n->expr = verexpr1
	end if
	astAppend( n, child )

	function = n
end function

function astNewDIMENSION _
	( _
		byval lb as ASTNODE ptr, _
		byval ub as ASTNODE ptr _
	) as ASTNODE ptr

	var n = astNew( ASTCLASS_DIMENSION )
	n->l = lb
	n->r = ub

	function = n
end function

function astNewCONST _
	( _
		byval i as longint, _
		byval f as double, _
		byval dtype as integer _
	) as ASTNODE ptr

	var n = astNew( ASTCLASS_CONST )
	n->dtype = dtype

	if( typeIsFloat( dtype ) ) then
		n->valf = f
	else
		n->vali = i
	end if

	function = n
end function

function astNewTK( byval x as integer ) as ASTNODE ptr
	var n = astNew( ASTCLASS_TK, tkGetText( x ) )
	n->tk = tkGet( x )
	n->location = *tkGetLocation( x )
	function = n
end function

function astNewFROGFILE _
	( _
		byval normed as zstring ptr, _
		byval pretty as zstring ptr _
	) as ASTNODE ptr

	var n = astNew( ASTCLASS_FROGFILE, normed )
	astSetComment( n, pretty )

	function = n
end function

sub astDelete( byval n as ASTNODE ptr )
	if( n = NULL ) then
		exit sub
	end if

	var child = n->head
	while( child )
		var nxt = child->next
		astDelete( child )
		child = nxt
	wend

	astDelete( n->r )
	astDelete( n->l )
	astDelete( n->expr )
	astDelete( n->array )
	deallocate( n->text )
	astDelete( n->subtype )
	deallocate( n )

	aststats.livenodes -= 1
end sub

sub astPrepend( byval parent as ASTNODE ptr, byval n as ASTNODE ptr )
	if( n = NULL ) then
		exit sub
	end if

	select case( n->class )
	case ASTCLASS_GROUP
		'' If it's a GROUP, add its children and delete the GROUP itself
		var child = n->tail
		while( child )
			astPrepend( parent, astClone( child ) )
			child = child->prev
		wend

		astDelete( n )
		exit sub

	case ASTCLASS_NOP
		'' Don't bother adding NOPs
		astDelete( n )
		exit sub

	end select

	n->next = parent->head
	if( parent->head ) then
		parent->head->prev = n
	end if
	parent->head = n
	if( parent->tail = NULL ) then
		parent->tail = n
	end if
end sub

sub astAppend( byval parent as ASTNODE ptr, byval n as ASTNODE ptr )
	if( n = NULL ) then
		exit sub
	end if

	select case( n->class )
	case ASTCLASS_GROUP
		'' If it's a GROUP, add its children and delete the GROUP itself
		var child = n->head
		while( child )
			astAppend( parent, astClone( child ) )
			child = child->next
		wend

		astDelete( n )
		exit sub

	case ASTCLASS_NOP
		'' Don't bother adding NOPs
		astDelete( n )
		exit sub

	end select

	n->prev = parent->tail
	if( parent->tail ) then
		parent->tail->next = n
	end if
	parent->tail = n
	if( parent->head = NULL ) then
		parent->head = n
	end if
end sub

sub astCloneAndAddAllChildrenOf( byval d as ASTNODE ptr, byval s as ASTNODE ptr )
	var child = s->head
	while( child )
		astAppend( d, astClone( child ) )
		child = child->next
	wend
end sub

private function astVersionMatches _
	( _
		byval pattern as ASTNODE ptr, _
		byval target as ASTNODE ptr _
	) as integer

	function = FALSE

	select case( pattern->class )
	case ASTCLASS_WILDCARD
		function = TRUE

	case ASTCLASS_BOP
		select case( pattern->op )
		case ASTOP_OR
			function = astVersionMatches( pattern->l, target ) or _
			           astVersionMatches( pattern->r, target )

		case ASTOP_MEMBER
			'' pattern a.a should match a.a, but not a.b or a
			if( target->class <> ASTCLASS_BOP ) then exit function
			if( target->op <> ASTOP_MEMBER ) then exit function
			function = astVersionMatches( pattern->l, target->l ) and _
			           astVersionMatches( pattern->r, target->r )

		case else
			assert( FALSE )
		end select

	'' Same node class?
	case target->class
		'' pattern a should match a, but not b
		select case( pattern->class )
		case ASTCLASS_STRING, ASTCLASS_ID
			function = (*pattern->text = *target->text)
		case ASTCLASS_DOS, ASTCLASS_LINUX, ASTCLASS_WIN32
			function = TRUE
		case else
			assert( FALSE )
		end select

	case else
		select case( target->class )
		case ASTCLASS_BOP
			select case( target->op )
			case ASTOP_MEMBER
				'' pattern a should match any of a.a, a.b, a.c etc.
				function = astVersionMatches( pattern, target->l )
			case ASTOP_OR
				'' pattern a should match a or b, but also b or a
				function = astVersionMatches( pattern, target->l ) or _
				           astVersionMatches( pattern, target->r )
			end select
		end select
	end select

end function

private function astPrefixVersion _
	( _
		byval verprefix as ASTNODE ptr, _
		byval nestedversions as ASTNODE ptr _
	) as ASTNODE ptr

	var versions = astNewGROUP( )

	var nestedversion = nestedversions->head
	while( nestedversion )

		astAppend( versions, _
			astNewBOP( ASTOP_MEMBER, _
				astClone( verprefix ), _
				astClone( nestedversion ) ) )

		nestedversion = nestedversion->next
	wend

	function = versions
end function

function astCollectVersions( byval context as ASTNODE ptr ) as ASTNODE ptr
	var versions = astNewGROUP( )

	'' For each nested VERBLOCK...
	var child = context->head
	while( child )

		if( child->class = ASTCLASS_VERBLOCK ) then
			var nestedversions = astCollectVersions( child )

			'' If this is a wildcard, apply nestedversions to all
			'' versions in this context except for other wildcards
			'' or itself:
			''
			''    version "1"
			''    version "2"
			''    version *
			''        version linux
			''
			'' should result in these colected versions:
			''
			''    version "1"
			''    version "1".linux
			''    version "2"
			''    version "2".linux
			''
			'' Otherwise, apply nestedversions just to this version:
			''
			''    version "1"
			''    version "2"
			''    version "3"
			''        version linux
			''
			'' should result in these colected versions:
			''
			''    version "1"
			''    version "2"
			''    version "3"
			''    version "3".linux

			'' a.b should have been split up into separate a and b VERBLOCKs
			assert( child->expr->class <> ASTCLASS_BOP )

			if( child->expr->class = ASTCLASS_WILDCARD ) then
				var child2 = context->head
				do
					if( child2->class = ASTCLASS_VERBLOCK ) then
						assert( child2->expr->class <> ASTCLASS_BOP )
						if( child2->expr->class <> ASTCLASS_WILDCARD ) then
							astAppend( versions, astPrefixVersion( child2->expr, nestedversions ) )
						end if
					end if
					child2 = child2->next
				loop while( child2 )
			else
				astAppend( versions, astClone( child->expr ) )
				astAppend( versions, astPrefixVersion( child->expr, nestedversions ) )
			end if
		end if

		child = child->next
	wend

	function = versions
end function

sub astAddVersionedChild( byval n as ASTNODE ptr, byval child as ASTNODE ptr )
	assert( n->class = ASTCLASS_GROUP )
	assert( child->class = ASTCLASS_VERBLOCK )

	'' If the tree's last VERBLOCK has the same version numbers, then
	'' just add the new children nodes to that instead of opening a new
	'' separate VERBLOCK.
	if( n->tail ) then
		assert( n->tail->class = ASTCLASS_VERBLOCK )
		if( astIsEqualDecl( n->tail->expr, child->expr ) ) then
			astCloneAndAddAllChildrenOf( n->tail, child )
			astDelete( child )
			exit sub
		end if
	end if

	astAppend( n, child )
end sub

function astGet1VersionOnly _
	( _
		byval code as ASTNODE ptr, _
		byval matchversion as ASTNODE ptr _
	) as ASTNODE ptr

	var result = astNewGROUP( )

	var child = code->head
	while( child )

		if( child->class = ASTCLASS_VERBLOCK ) then
			if( astVersionMatches( matchversion, child->expr ) ) then
				astAppend( result, astGet1VersionOnly( child, matchversion ) )
			end if
		else
			astAppend( result, astClone( child ) )
		end if

		child = child->next
	wend

	function = result
end function

private function astRemoveVerBlockWrapping _
	( _
		byval nodes as ASTNODE ptr, _
		byval matchversion as ASTNODE ptr _
	) as ASTNODE ptr

	var cleannodes = astNewGROUP( )

	assert( nodes->class = ASTCLASS_GROUP )
	var verblock = nodes->head
	while( verblock )
		assert( verblock->class = ASTCLASS_VERBLOCK )

		if( astIsEqualDecl( verblock->expr, matchversion ) ) then
			'' Add only the VERBLOCK's child nodes
			astCloneAndAddAllChildrenOf( cleannodes, verblock )
		else
			'' Add the whole VERBLOCK
			astAppend( cleannodes, astClone( verblock ) )
		end if

		verblock = verblock->next
	wend

	astDelete( nodes )
	astDelete( matchversion )
	function = cleannodes
end function

'' Build up a version matching expression that matches all versions from the
'' given list (a OR b OR c ...).
private function astBuildFullVersion( byval versions as ASTNODE ptr ) as ASTNODE ptr
	var v = versions->head
	assert( v )

	var r = astClone( v )
	do
		v = v->next
		if( v = NULL ) then exit do
		r = astNewBOP( ASTOP_OR, r, astClone( v ) )
	loop

	function = r
end function

'' Removes VERBLOCKs if they cover all versions, because if code that they
'' contain appears in all versions, then the VERBLOCK isn't needed.
'' VERBLOCKs are only needed for code that is specific to some versions
'' but not all.
private function astRemoveFullVerBlockWrapping _
	( _
		byval ast as ASTNODE ptr, _
		byval versions as ASTNODE ptr _
	) as ASTNODE ptr
	function = astRemoveVerBlockWrapping( ast, astBuildFullVersion( versions ) )
end function

sub astRemoveFullVerBlockWrappingFromFiles _
	( _
		byval files as ASTNODE ptr, _
		byval versions as ASTNODE ptr _
	)

	var f = files->head
	while( f )

		if( f->expr ) then
			f->expr = astRemoveFullVerBlockWrapping( f->expr, versions )
		end if

		f = f->next
	wend

end sub

function astIsChildOf _
	( _
		byval parent as ASTNODE ptr, _
		byval lookfor as ASTNODE ptr _
	) as integer

	var child = parent->head
	while( child )
		if( child = lookfor ) then
			return TRUE
		end if
		child = child->next
	wend

	function = FALSE
end function

sub astAddChildBefore _
	( _
		byval parent as ASTNODE ptr, _
		byval n as ASTNODE ptr, _
		byval ref as ASTNODE ptr _
	)

	if( ref ) then
		assert( astIsChildOf( parent, ref ) )
		if( ref->prev ) then
			ref->prev->next = n
		else
			parent->head = n
		end if
		n->next = ref
		n->prev = ref->prev
		ref->prev = n
	else
		if( parent->tail ) then
			parent->tail->next = n
		else
			parent->head = n
		end if
		n->prev = parent->tail
		n->next = NULL
		parent->tail = n
	end if

end sub

function astReplaceChild _
	( _
		byval parent as ASTNODE ptr, _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr _
	) as ASTNODE ptr

	assert( astIsChildOf( parent, a ) )
	astAddChildBefore( parent, b, a )
	astRemoveChild( parent, a )

	function = b
end function

sub astRemoveChild( byval parent as ASTNODE ptr, byval a as ASTNODE ptr )
	assert( astIsChildOf( parent, a ) )

	if( a->prev ) then
		a->prev->next = a->next
	else
		assert( parent->head = a )
		parent->head = a->next
	end if

	if( a->next ) then
		a->next->prev = a->prev
	else
		assert( parent->tail = a )
		parent->tail = a->prev
	end if

	astDelete( a )
end sub

sub astSetText( byval n as ASTNODE ptr, byval text as zstring ptr )
	deallocate( n->text )
	n->text = strDuplicate( text )
end sub

sub astRemoveText( byval n as ASTNODE ptr )
	deallocate( n->text )
	n->text = NULL
end sub

sub astSetType _
	( _
		byval n as ASTNODE ptr, _
		byval dtype as integer, _
		byval subtype as ASTNODE ptr _
	)

	astDelete( n->subtype )
	n->dtype = dtype
	n->subtype = astClone( subtype )

end sub

sub astSetComment( byval n as ASTNODE ptr, byval comment as zstring ptr )
	n->comment = strDuplicate( comment )
end sub

sub astAddComment( byval n as ASTNODE ptr, byval comment as zstring ptr )
	dim as string s

	if( len( *comment ) = 0 ) then
		exit sub
	end if

	if( n->comment ) then
		s = *n->comment + !"\n"
		deallocate( n->comment )
	end if

	s += *comment

	astSetComment( n, s )
end sub

'' astClone() but without children
function astCloneNode( byval n as ASTNODE ptr ) as ASTNODE ptr
	if( n = NULL ) then
		return NULL
	end if

	var c = astNew( n->class )
	c->attrib      = n->attrib

	c->text        = strDuplicate( n->text )
	c->comment     = strDuplicate( n->comment )

	c->dtype       = n->dtype
	c->subtype     = astClone( n->subtype )
	c->array       = astClone( n->array )

	c->location    = n->location

	c->expr        = astClone( n->expr )
	c->l           = astClone( n->l )
	c->r           = astClone( n->r )

	select case( n->class )
	case ASTCLASS_CONST
		if( typeIsFloat( n->dtype ) ) then
			c->valf = n->valf
		else
			c->vali = n->vali
		end if
	case ASTCLASS_TK
		c->tk = n->tk
	case ASTCLASS_PPDEFINE
		c->paramcount = n->paramcount
	case ASTCLASS_UOP, ASTCLASS_BOP
		c->op = n->op
	case ASTCLASS_FROGFILE
		c->refcount = n->refcount
		c->mergeparent = n->mergeparent
	end select

	function = c
end function

function astClone( byval n as ASTNODE ptr ) as ASTNODE ptr
	var c = astCloneNode( n )
	if( c = NULL ) then
		return NULL
	end if

	var child = n->head
	while( child )
		astAppend( c, astClone( child ) )
		child = child->next
	wend

	function = c
end function

'' Check whether two ASTs represent equal declarations, i.e. most fields must be
'' equal, but some things may be different as long as it would still result in
'' compatible C/FB code.
'' For example, two procedures must have the same kind of parameters, but it
'' doesn't matter whether two CONST expressions both originally were
'' oct/hex/dec, as long as they're the same value.
function astIsEqualDecl _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr, _
		byval ignore_fields as integer, _
		byval ignore_hiddencallconv as integer _
	) as integer

	'' If one is NULL, both must be NULL
	if( (a = NULL) or (b = NULL) ) then
		return ((a = NULL) and (b = NULL))
	end if

	if( a->class <> b->class ) then exit function

	if( (a->attrib and ASTATTRIB_EXTERN) <> _
	    (b->attrib and ASTATTRIB_EXTERN) ) then
		exit function
	end if

	if( (a->attrib and ASTATTRIB_PRIVATE) <> _
	    (b->attrib and ASTATTRIB_PRIVATE) ) then
		exit function
	end if

	var ignore_callconv = FALSE
	if( ignore_hiddencallconv ) then
		var ahidden = ((a->attrib and ASTATTRIB_HIDECALLCONV) <> 0)
		var bhidden = ((b->attrib and ASTATTRIB_HIDECALLCONV) <> 0)
		ignore_callconv = (ahidden = bhidden)
	end if

	if( ignore_callconv = FALSE ) then
		if( (a->attrib and ASTATTRIB_CDECL) <> _
		    (b->attrib and ASTATTRIB_CDECL) ) then
			exit function
		end if

		if( (a->attrib and ASTATTRIB_STDCALL) <> _
		    (b->attrib and ASTATTRIB_STDCALL) ) then
			exit function
		end if
	end if

	if( (a->text <> NULL) and (b->text <> NULL) ) then
		if( *a->text <> *b->text ) then exit function
	else
		if( (a->text <> NULL) <> (b->text <> NULL) ) then exit function
	end if

	if( a->dtype <> b->dtype ) then exit function
	if( astIsEqualDecl( a->subtype, b->subtype, ignore_fields, ignore_hiddencallconv ) = FALSE ) then exit function
	if( astIsEqualDecl( a->array, b->array, ignore_fields, ignore_hiddencallconv ) = FALSE ) then exit function

	if( astIsEqualDecl( a->expr, b->expr, ignore_fields, ignore_hiddencallconv ) = FALSE ) then exit function
	if( astIsEqualDecl( a->l, b->l, ignore_fields, ignore_hiddencallconv ) = FALSE ) then exit function
	if( astIsEqualDecl( a->r, b->r, ignore_fields, ignore_hiddencallconv ) = FALSE ) then exit function

	select case( a->class )
	case ASTCLASS_CONST
		if( typeIsFloat( a->dtype ) ) then
			const EPSILON_DBL as double = 2.2204460492503131e-016
			if( abs( a->valf - b->valf ) >= EPSILON_DBL ) then exit function
		else
			if( a->vali <> b->vali ) then exit function
		end if

	case ASTCLASS_TK
		if( a->tk <> b->tk ) then exit function

	case ASTCLASS_PPDEFINE
		if( a->paramcount <> b->paramcount ) then exit function

	case ASTCLASS_UOP, ASTCLASS_BOP
		if( a->op <> b->op ) then exit function

	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		if( ignore_fields ) then return TRUE

	case ASTCLASS_FROGFILE
		if( a->refcount <> b->refcount ) then exit function
		if( a->mergeparent <> b->mergeparent ) then exit function
	end select

	'' Children
	a = a->head
	b = b->head
	while( (a <> NULL) and (b <> NULL) )
		if( astIsEqualDecl( a, b, ignore_fields, ignore_hiddencallconv ) = FALSE ) then
			exit function
		end if
		a = a->next
		b = b->next
	wend

	'' Both a's and b's last child must be reached at the same time
	function = ((a = NULL) and (b = NULL))
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function astExprContains _
	( _
		byval n as ASTNODE ptr, _
		byval nodeclass as integer _
	) as integer

	if( n = NULL ) then return FALSE
	if( n->class = nodeclass ) then return TRUE

	function = astExprContains( n->expr, nodeclass ) or _
		astExprContains( n->l, nodeclass ) or _
		astExprContains( n->r, nodeclass )
end function

#define astExprHasSideFx( n ) astExprContains( n, ASTCLASS_CALL )

private function astOpC2FB _
	( _
		byval n as ASTNODE ptr, _
		byval fbop as integer _
	) as ASTNODE ptr

	assert( (n->class = ASTCLASS_UOP) or (n->class = ASTCLASS_BOP) )
	n->op = fbop

	function = astNewUOP( ASTOP_NEGATE, n )
end function

function astOpsC2FB( byval n as ASTNODE ptr ) as ASTNODE ptr
	if( n = NULL ) then exit function

	n->expr = astOpsC2FB( n->expr )
	n->l = astOpsC2FB( n->l )
	n->r = astOpsC2FB( n->r )

	select case( n->class )
	case ASTCLASS_UOP
		select case( n->op )
		case ASTOP_CLOGNOT
			'' !x    =    x = 0
			n->class = ASTCLASS_BOP
			n->r = astNewCONST( 0, 0, TYPE_LONG )
			n = astOpC2FB( n, ASTOP_EQ )

		case ASTOP_CDEFINED
			n = astOpC2FB( n, ASTOP_DEFINED )
		end select

	case ASTCLASS_BOP
		select case( n->op )
		case ASTOP_CLOGOR  : n = astOpC2FB( n, ASTOP_ORELSE )
		case ASTOP_CLOGAND : n = astOpC2FB( n, ASTOP_ANDALSO )
		case ASTOP_CEQ     : n = astOpC2FB( n, ASTOP_EQ )
		case ASTOP_CNE     : n = astOpC2FB( n, ASTOP_NE )
		case ASTOP_CLT     : n = astOpC2FB( n, ASTOP_LT )
		case ASTOP_CLE     : n = astOpC2FB( n, ASTOP_LE )
		case ASTOP_CGT     : n = astOpC2FB( n, ASTOP_GT )
		case ASTOP_CGE     : n = astOpC2FB( n, ASTOP_GE )
		end select

	end select

	function = n
end function

private function astFoldKnownDefineds _
	( _
		byval n as ASTNODE ptr, _
		byval macros as THASH ptr _
	) as ASTNODE ptr

	if( n = NULL ) then exit function
	function = n

	n->expr = astFoldKnownDefineds( n->expr, macros )
	n->l = astFoldKnownDefineds( n->l, macros )
	n->r = astFoldKnownDefineds( n->r, macros )

	if( n->class = ASTCLASS_UOP ) then
		if( n->op = ASTOP_DEFINED ) then
			'' defined() on known symbol?
			'' (assuming it's the eval.macros hash table from parse-cpp.bas)
			assert( n->l->class = ASTCLASS_ID )
			var id = n->l->text
			var item = hashLookup( macros, id, hashHash( id ) )
			if( item->s ) then
				'' Currently defined?
				var is_defined = (cint( item->data ) >= 0)

				'' FB defined()    ->   -1|0
				'' item->data = is_defined
				function = astNewCONST( is_defined, 0, TYPE_LONG )
				astDelete( n )
			end if
		end if
	end if

end function

private function astFoldConsts( byval n as ASTNODE ptr ) as ASTNODE ptr
	if( n = NULL ) then exit function
	function = n

	n->expr = astFoldConsts( n->expr )
	n->l = astFoldConsts( n->l )
	n->r = astFoldConsts( n->r )

	select case( n->class )
	case ASTCLASS_UOP
		if( (n->l->class = ASTCLASS_CONST) and _
		    (not typeIsFloat( n->l->dtype )) ) then
			var v1 = n->l->vali

			select case( n->op )
			'case ASTOP_CLOGNOT   : v1 = iif( v1, 0, 1 )
			case ASTOP_NOT       : v1 = not v1
			case ASTOP_NEGATE    : v1 = -v1
			case ASTOP_UNARYPLUS : '' nothing to do
			case else
				assert( FALSE )
			end select

			function = astNewCONST( v1, 0, TYPE_LONG )
			astDelete( n )
		end if

	case ASTCLASS_BOP
		if( (n->l->class = ASTCLASS_CONST) and _
		    (n->r->class = ASTCLASS_CONST) ) then
			if( (not typeIsFloat( n->l->dtype )) and _
			    (not typeIsFloat( n->r->dtype )) ) then
				var v1 = n->l->vali
				var v2 = n->r->vali

				var divbyzero = FALSE

				select case as const( n->op )
				'case ASTOP_CLOGOR   : v1    = -(v1 orelse  v2)
				'case ASTOP_CLOGAND  : v1    = -(v1 andalso v2)
				case ASTOP_ORELSE   : v1    =   v1 orelse  v2
				case ASTOP_ANDALSO  : v1    =   v1 andalso v2
				case ASTOP_OR       : v1  or= v2
				case ASTOP_XOR      : v1 xor= v2
				case ASTOP_AND      : v1 and= v2
				'case ASTOP_CEQ      : v1    = -(v1 =  v2)
				'case ASTOP_CNE      : v1    = -(v1 <> v2)
				'case ASTOP_CLT      : v1    = -(v1 <  v2)
				'case ASTOP_CLE      : v1    = -(v1 <= v2)
				'case ASTOP_CGT      : v1    = -(v1 >  v2)
				'case ASTOP_CGE      : v1    = -(v1 >= v2)
				case ASTOP_EQ       : v1    =   v1 =  v2
				case ASTOP_NE       : v1    =   v1 <> v2
				case ASTOP_LT       : v1    =   v1 <  v2
				case ASTOP_LE       : v1    =   v1 <= v2
				case ASTOP_GT       : v1    =   v1 >  v2
				case ASTOP_GE       : v1    =   v1 >= v2
				case ASTOP_SHL      : v1 shl= v2
				case ASTOP_SHR      : v1 shr= v2
				case ASTOP_ADD      : v1   += v2
				case ASTOP_SUB      : v1   -= v2
				case ASTOP_MUL      : v1   *= v2
				case ASTOP_DIV
					if( v2 = 0 ) then
						divbyzero = TRUE
					else
						v1 \= v2
					end if
				case ASTOP_MOD
					if( v2 = 0 ) then
						divbyzero = TRUE
					else
						v1 mod= v2
					end if
				case else
					assert( FALSE )
				end select

				if( divbyzero = FALSE ) then
					function = astNewCONST( v1, 0, TYPE_LONG )
					astDelete( n )
				end if
			end if
		end if

	case ASTCLASS_IIF
		'' Constant condition?
		if( (n->expr->class = ASTCLASS_CONST) and _
		    (not typeIsFloat( n->expr->dtype )) ) then
			'' iif( true , l, r ) = l
			'' iif( false, l, r ) = r
			if( n->expr->vali ) then
				function = n->l
				n->l = NULL
			else
				function = n->r
				n->r = NULL
			end if
			astDelete( n )

		end if
	end select

end function

'' For commutative BOPs where only the lhs is a CONST, swap lhs/rhs so the
'' CONST ends up on the rhs on as many BOPs as possible (not on all, because
'' not all are commutative). That simplifies some checks for BOPs with only
'' one CONST operand, because only the rhs needs to be checked.
private sub astSwapConstsToRhs( byval n as ASTNODE ptr )
	if( n = NULL ) then exit sub

	astSwapConstsToRhs( n->expr )
	astSwapConstsToRhs( n->l )
	astSwapConstsToRhs( n->r )

	if( n->class = ASTCLASS_BOP ) then
		'' Only the lhs is a CONST?
		if( (n->l->class = ASTCLASS_CONST) and _
		    (n->r->class <> ASTCLASS_CONST) ) then
			select case( n->op )
			'' N and x   =   x and N
			'' N or  x   =   x or  N
			'' N xor x   =   x xor N
			'' N =   x   =   x =   N
			'' N <>  x   =   x <>  N
			'' N +   x   =   x +   N
			'' N *   x   =   x *   N
			case ASTOP_ADD, ASTOP_MUL, ASTOP_AND, ASTOP_OR, _
			     ASTOP_XOR, ASTOP_EQ, ASTOP_NE
				swap n->l, n->r

			'' N <  x   =   x >  N
			'' N <= x   =   x >= N
			'' N >  x   =   x <  N
			'' N >= x   =   x <= N
			case ASTOP_LT, ASTOP_LE, ASTOP_GT, ASTOP_GE
				select case( n->op )
				case ASTOP_LT : n->op = ASTOP_GT
				case ASTOP_LE : n->op = ASTOP_GE
				case ASTOP_GT : n->op = ASTOP_LT
				case ASTOP_GE : n->op = ASTOP_LE
				end select
				swap n->l, n->r

			'' N - x   =   -x + N
			case ASTOP_SUB
				swap n->l, n->r
				n->l = astNewUOP( ASTOP_NEGATE, n->l )
				n->op = ASTOP_ADD
			end select
		end if
	end if
end sub

private function hIsBoolOp( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_UOP
		function = (n->op = ASTOP_DEFINED)
	case ASTCLASS_BOP
		select case( n->op )
		case ASTOP_EQ, ASTOP_NE, ASTOP_LT, _
		     ASTOP_LE, ASTOP_GT, ASTOP_GE, _
		     ASTOP_ORELSE, ASTOP_ANDALSO
			function = TRUE
		end select
	end select
end function

private function astFoldNops( byval n as ASTNODE ptr ) as ASTNODE ptr
	if( n = NULL ) then exit function
	function = n

	n->expr = astFoldNops( n->expr )
	n->l = astFoldNops( n->l )
	n->r = astFoldNops( n->r )

	select case( n->class )
	case ASTCLASS_UOP
		select case( n->op )
		'' +x = x
		case ASTOP_UNARYPLUS
			function = n->l : n->l = NULL
			astDelete( n )
		end select

	case ASTCLASS_BOP
		'' Only the lhs is a CONST? Check for NOPs
		if( (n->l->class = ASTCLASS_CONST) and _
		    (n->r->class <> ASTCLASS_CONST) ) then
			if( typeIsFloat( n->l->dtype ) = FALSE ) then
				var v1 = n->l->vali

				select case( n->op )

				'' true  orelse x   = -1
				'' false orelse x   = x
				case ASTOP_ORELSE
					if( v1 ) then
						function = astNewCONST( -1, 0, TYPE_LONG )
					else
						function = n->r : n->r = NULL
					end if
					astDelete( n )

				'' true  andalso x   = x
				'' false andalso x   = 0
				case ASTOP_ANDALSO
					if( v1 ) then
						function = n->r : n->r = NULL
					else
						function = astNewCONST( 0, 0, TYPE_LONG )
					end if
					astDelete( n )

				'' 0 shl x = 0    unless sidefx
				'' 0 shr x = 0    unless sidefx
				'' 0 /   x = 0    unless sidefx
				'' 0 mod x = 0    unless sidefx
				case ASTOP_AND, ASTOP_SHL, ASTOP_SHR, _
				     ASTOP_DIV, ASTOP_MOD
					if( v1 = 0 ) then
						if( astExprHasSideFx( n->r ) = FALSE ) then
							function = astNewCONST( 0, 0, TYPE_LONG )
							astDelete( n )
						end if
					end if

				end select
			end if

		'' Only the rhs is a CONST? Check for NOPs
		elseif( (n->l->class <> ASTCLASS_CONST) and _
		        (n->r->class = ASTCLASS_CONST) ) then
			if( typeIsFloat( n->r->dtype ) = FALSE ) then
				var v2 = n->r->vali

				select case( n->op )
				'' x orelse true    = -1    unless sidefx
				'' x orelse false   = x
				case ASTOP_ORELSE
					if( v2 ) then
						if( astExprHasSideFx( n->l ) = FALSE ) then
							function = astNewCONST( -1, 0, TYPE_LONG )
							astDelete( n )
						end if
					else
						function = n->l : n->l = NULL
						astDelete( n )
					end if

				'' x andalso true    = x
				'' x andalso false   = 0    unless sidefx
				case ASTOP_ANDALSO
					if( v2 ) then
						function = n->l : n->l = NULL
						astDelete( n )
					else
						if( astExprHasSideFx( n->l ) = FALSE ) then
							function = astNewCONST( 0, 0, TYPE_LONG )
							astDelete( n )
						end if
					end if

				'' bool <>  0   =    bool
				'' bool <> -1   =    not bool
				case ASTOP_NE
					if( hIsBoolOp( n->l ) ) then
						select case( v2 )
						case 0
							function = n->l : n->l = NULL
							astDelete( n )
						case -1
							'' Delete rhs and turn <> BOP into not UOP
							astDelete( n->r ) : n->r = NULL
							n->class = ASTCLASS_UOP
							n->op = ASTOP_NOT
						end select
					end if

				'' bool =  0    =    not bool
				'' bool = -1    =    bool
				case ASTOP_EQ
					if( hIsBoolOp( n->l ) ) then
						select case( v2 )
						case 0
							'' Delete rhs and turn = BOP into not UOP
							astDelete( n->r ) : n->r = NULL
							n->class = ASTCLASS_UOP
							n->op = ASTOP_NOT
						case -1
							function = n->l : n->l = NULL
							astDelete( n )
						end select
					end if

				'' x or  0 =  x
				'' x or -1 = -1    unless sidefx
				case ASTOP_OR
					select case( v2 )
					case 0
						function = n->l : n->l = NULL
						astDelete( n )
					case -1
						if( astExprHasSideFx( n->l ) = FALSE ) then
							function = astNewCONST( -1, 0, TYPE_LONG )
							astDelete( n )
						end if
					end select

				'' x +   0 =  x
				'' x -   0 =  x
				'' x shl 0 =  x
				'' x shr 0 =  x
				case ASTOP_ADD, ASTOP_SUB, ASTOP_SHL, ASTOP_SHR
					if( v2 = 0 ) then
						function = n->l : n->l = NULL
						astDelete( n )
					end if

				'' x and 0 = 0    unless sidefx
				case ASTOP_AND
					if( v2 = 0 ) then
						if( astExprHasSideFx( n->l ) = FALSE ) then
							function = astNewCONST( 0, 0, TYPE_LONG )
							astDelete( n )
						end if
					end if

				'' x * 0 = 0    unless sidefx
				'' x * 1 = x
				case ASTOP_MUL
					select case( v2 )
					case 0
						if( astExprHasSideFx( n->l ) = FALSE ) then
							function = astNewCONST( 0, 0, TYPE_LONG )
							astDelete( n )
						end if
					case 1
						function = n->l : n->l = NULL
						astDelete( n )
					end select

				end select
			end if
		end if

	case ASTCLASS_IIF
		'' Same true/false expressions?
		'' iif( condition, X, X ) = X    unless sidefx in condition
		if( astIsEqualDecl( n->l, n->r ) ) then
			if( astExprHasSideFx( n->expr ) = FALSE ) then
				function = n->l : n->l = NULL
				astDelete( n )
			end if
		end if
	end select

end function

private function astFoldNestedOps( byval n as ASTNODE ptr ) as ASTNODE ptr
	if( n = NULL ) then exit function
	function = n

	n->expr = astFoldNestedOps( n->expr )
	n->l = astFoldNestedOps( n->l )
	n->r = astFoldNestedOps( n->r )

	select case( n->class )
	case ASTCLASS_UOP
		'' l = UOP?
		if( n->l->class = ASTCLASS_UOP ) then
			select case( n->op )
			'' not (not x)   = x
			''       -(-x)   = x
			case ASTOP_NOT, ASTOP_NEGATE
				if( n->op = n->l->op ) then
					function = n->l->l : n->l->l = NULL
					astDelete( n )
				end if
			end select

		'' l = BOP?
		elseif( n->l->class = ASTCLASS_BOP ) then
			select case( n->op )
			'' not (ll relbop lr)   = ll inverse_relbop lr
			case ASTOP_NOT
				select case( n->l->op )
				case ASTOP_EQ, ASTOP_NE, ASTOP_LT, _
				     ASTOP_LE, ASTOP_GT, ASTOP_GE
					select case( n->l->op )
					case ASTOP_EQ : n->l->op = ASTOP_NE '' not =   ->  <>
					case ASTOP_NE : n->l->op = ASTOP_EQ '' not <>  ->  =
					case ASTOP_LT : n->l->op = ASTOP_GE '' not <   ->  >=
					case ASTOP_LE : n->l->op = ASTOP_GT '' not <=  ->  >
					case ASTOP_GT : n->l->op = ASTOP_LE '' not >   ->  <=
					case ASTOP_GE : n->l->op = ASTOP_LT '' not >=  ->  <
					end select

					function = n->l : n->l = NULL
					astDelete( n )
				end select
			end select
		end if

	case ASTCLASS_BOP

		'' l=UOP, r=CONST?
		if( (n->l->class = ASTCLASS_UOP) and _
		    (n->r->class = ASTCLASS_CONST) ) then
			'' (-bool) = 1    =    bool = -1
			if( n->op = ASTOP_EQ ) then
				if( n->l->op = ASTOP_NEGATE ) then
					if( n->r->vali = 1 ) then
						if( hIsBoolOp( n->l->l ) ) then
							'' On the lhs, replace the - UOP by its own operand
							var negatenode = n->l
							n->l = negatenode->l : negatenode->l = NULL
							astDelete( negatenode )
							'' On the rhs, turn 1 into -1
							n->r->vali = -1
						end if
					end if
				end if
			end if

		'' l=BOP, r=CONST?
		elseif( (n->l->class = ASTCLASS_BOP) and _
		        (n->r->class = ASTCLASS_CONST) ) then
			'' (x = 0) = 0    =    x <> 0
			if( n->op = ASTOP_EQ ) then
				if( n->l->op = ASTOP_EQ ) then
					'' lr=CONST?
					if( n->l->r->class = ASTCLASS_CONST ) then
						'' r and lr both '0'?
						if( (n->r->vali = 0) and (n->l->r->vali = 0) ) then
							n->l->op = ASTOP_NE
							function = n->l : n->l = NULL
							astDelete( n )
						end if
					end if
				end if
			end if
		end if
	end select

end function

private function astFoldBoolContextNops _
	( _
		byval n as ASTNODE ptr, _
		byval is_bool_context as integer _
	) as ASTNODE ptr

	if( n = NULL ) then exit function
	function = n

	select case( n->class )
	case ASTCLASS_UOP
		n->l = astFoldBoolContextNops( n->l, FALSE )

		select case( n->op )
		'' if( -x ) then   =   if( x ) then
		case ASTOP_NEGATE
			if( is_bool_context ) then
				function = n->l : n->l = NULL
				astDelete( n )
			end if
		end select

	case ASTCLASS_BOP
		var l_is_bool_context = FALSE
		var r_is_bool_context = FALSE

		'' BOP operand in bool context?
		select case( n->op )
		'' x = 0
		'' x <> 0
		case ASTOP_EQ, ASTOP_NE
			if( n->r->class = ASTCLASS_CONST ) then
				l_is_bool_context = (n->r->vali = 0)
			end if

		'' andalso/orelse operands are always treated as bools
		case ASTOP_ORELSE, ASTOP_ANDALSO
			l_is_bool_context = TRUE
			r_is_bool_context = TRUE
		end select

		n->l = astFoldBoolContextNops( n->l, l_is_bool_context )
		n->r = astFoldBoolContextNops( n->r, r_is_bool_context )

	case ASTCLASS_IIF
		'' iif() condition always is treated as bool
		n->expr = astFoldBoolContextNops( n->expr, TRUE )
		n->l = astFoldBoolContextNops( n->l, FALSE )
		n->r = astFoldBoolContextNops( n->r, FALSE )

	end select
end function

private sub hComplainAboutExpr _
	( _
		byval n as ASTNODE ptr, _
		byval message as zstring ptr _
	)

	if( verbose ) then
		if( n->location.file ) then
			hReportLocation( @n->location, message )
		else
			print *message
		end if
	end if

end sub

private function astFoldUnknownIds( byval n as ASTNODE ptr ) as ASTNODE ptr
	function = n
	if( n = NULL ) then
		exit function
	end if

	select case( n->class )
	case ASTCLASS_ID
		'' Unexpanded identifier, assume it's undefined, like a CPP
		hComplainAboutExpr( n, "treating unexpanded identifier '" + *n->text + "' as literal zero" )

		'' id   ->   0
		function = astNewCONST( 0, 0, TYPE_LONGINT )
		astDelete( n )

	case ASTCLASS_UOP
		select case( n->op )
		case ASTOP_DEFINED
			'' Unsolved defined(), must be an unknown symbol, so it
			'' should expand to FALSE. (see also astFoldKnownDefineds())
			assert( n->l->class = ASTCLASS_ID )
			hComplainAboutExpr( n->l, "assuming symbol '" + *n->l->text + "' is undefined" )

			'' defined()   ->   0
			function = astNewCONST( 0, 0, TYPE_LONGINT )
			astDelete( n )

		case ASTOP_SIZEOF, ASTOP_STRINGIFY
			'' Don't handle the ID for these recursively, because
			'' astFoldConsts() would currently choke, trying to
			'' fold sizeof()/#stringify with CONST operands. But
			'' luckily, these two UOPs can't appear in CPP
			'' expressions anyways.

		case else
			n->l = astFoldUnknownIds( n->l )
		end select

	case ASTCLASS_BOP
		n->l = astFoldUnknownIds( n->l )
		n->r = astFoldUnknownIds( n->r )

	case ASTCLASS_IIF
		n->expr = astFoldUnknownIds( n->expr )
		n->l = astFoldUnknownIds( n->l )
		n->r = astFoldUnknownIds( n->r )
	end select

end function

function astFold _
	( _
		byval n as ASTNODE ptr, _
		byval macros as THASH ptr, _
		byval fold_unknowns as integer, _
		byval is_bool_context as integer _
	) as ASTNODE ptr

	n = astFoldKnownDefineds( n, macros )

	dim as ASTNODE ptr c
	var passes = 0
	do
		c = astClone( n )
		passes += 1
		aststats.foldpasses += 1

		n = astFoldConsts( n )
		astSwapConstsToRhs( n )
		n = astFoldNops( n )
		n = astFoldNestedOps( n )
		n = astFoldBoolContextNops( n, is_bool_context )

		'' At the end of the 1st pass only, report unsolved defined()'s
		'' and atom identifiers, then solve them out like a CPP would
		if( (passes = 1) and fold_unknowns ) then
			n = astFoldUnknownIds( n )
		end if

		'' Loop until the folding no longer changes the expression
		if( astIsEqualDecl( n, c ) ) then
			exit do
		end if

		astDelete( c )
	loop

	if( aststats.minfoldpasses = 0 ) then
		aststats.minfoldpasses = passes
		aststats.maxfoldpasses = passes
	else
		if( aststats.minfoldpasses > passes ) then
			aststats.minfoldpasses = passes
		end if
		if( aststats.maxfoldpasses < passes ) then
			aststats.maxfoldpasses = passes
		end if
	end if

	function = n
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function astLookupMacroParam _
	( _
		byval macro as ASTNODE ptr, _
		byval id as zstring ptr _
	) as integer

	var index = 0

	assert( macro->class = ASTCLASS_PPDEFINE )

	var param = macro->head
	while( param )

		assert( param->class = ASTCLASS_MACROPARAM )
		if( *param->text = *id ) then
			return index
		end if

		index += 1
		param = param->next
	wend

	function = -1
end function

sub astNodeToNop _
	( _
		byval n as ASTNODE ptr, _
		byval astclass as integer, _
		byref id as string _
	)

	if( (n->class = astclass) and (*n->text = id) ) then
		n->class = ASTCLASS_NOP
		exit sub
	end if

	var child = n->head
	while( child )
		astNodeToNop( child, astclass, id )
		child = child->next
	wend

end sub

private sub astMakeProcsDefaultToCdecl( byval n as ASTNODE ptr )
	if( n->class = ASTCLASS_PROC ) then
		'' No calling convention specified yet?
		if( (n->attrib and (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)) = 0 ) then
			n->attrib or= ASTATTRIB_CDECL
		end if
	end if

	'' Don't forget the procptr subtypes
	if( typeGetDt( n->dtype ) = TYPE_PROC ) then
		astMakeProcsDefaultToCdecl( n->subtype )
	end if

	var child = n->head
	while( child )
		astMakeProcsDefaultToCdecl( child )
		child = child->next
	wend
end sub

private function astCountCallConv _
	( _
		byval n as ASTNODE ptr, _
		byval callconv as integer _
	) as integer

	var count = 0

	if( n->class = ASTCLASS_PROC ) then
		if( n->attrib and callconv ) then
			count += 1
		end if
	end if

	'' Don't forget the procptr subtypes
	if( typeGetDt( n->dtype ) = TYPE_PROC ) then
		count += astCountCallConv( n->subtype, callconv )
	end if

	var child = n->head
	while( child )
		count += astCountCallConv( child, callconv )
		child = child->next
	wend

	function = count
end function

private sub astHideCallConv( byval n as ASTNODE ptr, byval callconv as integer )
	if( n->class = ASTCLASS_PROC ) then
		if( n->attrib and callconv ) then
			n->attrib or= ASTATTRIB_HIDECALLCONV
		end if
	end if

	'' Don't forget the procptr subtypes
	if( typeGetDt( n->dtype ) = TYPE_PROC ) then
		astHideCallConv( n->subtype, callconv )
	end if

	var child = n->head
	while( child )
		astHideCallConv( child, callconv )
		child = child->next
	wend
end sub

private function astFindMainCallConv( byval ast as ASTNODE ptr ) as integer
	var   cdeclcount = astCountCallConv( ast, ASTATTRIB_CDECL   )
	var stdcallcount = astCountCallConv( ast, ASTATTRIB_STDCALL )
	if( (cdeclcount = 0) and (stdcallcount = 0) ) then
		function = -1
	elseif( stdcallcount > cdeclcount ) then
		function = ASTATTRIB_STDCALL
	else
		function = ASTATTRIB_CDECL
	end if
end function

private sub astTurnCallConvIntoExternBlock _
	( _
		byval ast as ASTNODE ptr, _
		byval externcallconv as integer, _
		byval use_stdcallms as integer _
	)

	var externblock = @"C"
	if( externcallconv = ASTATTRIB_STDCALL ) then
		if( use_stdcallms ) then
			externblock = @"Windows-MS"
		else
			externblock = @"Windows"
		end if
	end if

	'' Remove the calling convention from all procdecls, the Extern block
	'' will take over
	astHideCallConv( ast, externcallconv )

	assert( ast->class = ASTCLASS_GROUP )
	astPrepend( ast, astNew( ASTCLASS_DIVIDER ) )
	astPrepend( ast, astNew( ASTCLASS_EXTERNBLOCKBEGIN, externblock ) )
	astAppend( ast, astNew( ASTCLASS_DIVIDER ) )
	astAppend( ast, astNew( ASTCLASS_EXTERNBLOCKEND ) )

end sub

sub astAutoExtern _
	( _
		byval ast as ASTNODE ptr, _
		byval use_stdcallms as integer = FALSE _
	)

	astMakeProcsDefaultToCdecl( ast )

	var maincallconv = astFindMainCallConv( ast )
	if( maincallconv >= 0 ) then
		astTurnCallConvIntoExternBlock( ast, maincallconv, use_stdcallms )
	end if

end sub

sub astRemoveParamNames( byval n as ASTNODE ptr )
	if( n->class = ASTCLASS_PARAM ) then
		astRemoveText( n )
	end if

	'' Don't forget the procptr subtypes
	if( typeGetDt( n->dtype ) = TYPE_PROC ) then
		astRemoveParamNames( n->subtype )
	end if

	var child = n->head
	while( child )
		astRemoveParamNames( child )
		child = child->next
	wend
end sub

sub astFixArrayParams( byval n as ASTNODE ptr )
	if( n->class = ASTCLASS_PARAM ) then
		'' C array parameters are really just pointers (i.e. the array
		'' is passed byref), and FB doesn't support array parameters
		'' like that, so turn them into pointers:
		''    int a[5]  ->  byval a as long ptr
		if( n->array ) then
			astDelete( n->array )
			n->array = NULL
			n->dtype = typeAddrOf( n->dtype )
		end if
	end if

	var child = n->head
	while( child )
		astFixArrayParams( child )
		child = child->next
	wend
end sub

private sub hReplaceTypedefBaseSubtype _
	( _
		byval n as ASTNODE ptr, _
		byval anon as ASTNODE ptr, _
		byval aliastypedef as ASTNODE ptr _
	)

	if( n->subtype = NULL ) then exit sub

	select case( n->subtype->class )
	'' UDT subtypes
	case ASTCLASS_ID
		if( *n->subtype->text = *anon->text ) then
			astDelete( n->subtype )
			n->subtype = astNewID( aliastypedef->text )
		end if

	'' Function pointer subtypes too
	case ASTCLASS_PROC
		hReplaceTypedefBaseSubtype( n->subtype, anon, aliastypedef )
	end select

end sub

''
'' Look for TYPEDEFs that have the given anon UDT as subtype. There should be
'' at least one; if not, report an error.
'' The first TYPEDEF's id can become the anon UDT's id, and then that TYPEDEF
'' can be removed. All other TYPEDEFs need to be changed over from the old anon
'' subtype to the new id subtype.
''
'' For example:
''    typedef struct { ... } A, B, C;
'' is parsed into:
''    struct __fbfrog_anon1
''        ...
''    typedef A as __fbfrog_anon1
''    typedef B as __fbfrog_anon1
''    typedef C as __fbfrog_anon1
'' and should now be changed to:
''    struct A
''        ...
''    typedef B as A
''    typedef C as A
''
'' Not all cases can be solved out, for example:
''    typedef struct { ... } *A;
'' is parsed into:
''    struct __fbfrog_anon1
''        ...
''    typedef A as __fbfrog_anon1 ptr
'' i.e. the typedef is a pointer to the anon struct, not an alias for it.
''
private sub hTryFixAnon(  byval anon as ASTNODE ptr )
	'' (Assuming that the parser will only insert typedefs using the anon id
	'' behind the anon UDT node...)

	'' 1. Find alias typedef
	dim as ASTNODE ptr aliastypedef
	var typedef = anon->next
	while( typedef )
		if( typedef->class <> ASTCLASS_TYPEDEF ) then
			typedef = NULL
			exit while
		end if

		'' Must be a plain alias, can't be a pointer or non-UDT
		if( typeGetDtAndPtr( typedef->dtype ) = TYPE_UDT ) then
			assert( typedef->subtype->class = ASTCLASS_ID )
			if( *typedef->subtype->text = *anon->text ) then
				aliastypedef = typedef
				exit while
			end if
		end if

		typedef = typedef->next
	wend

	'' Can't be solved out?
	if( aliastypedef = NULL ) then
		exit sub
	end if

	'' 2. Go through all typedefs behind the anon, and replace the subtypes
	'' (or perhaps the subtype's subtype, in case it's a procptr typedef)
	typedef = anon->next
	while( typedef )
		if( typedef->class <> ASTCLASS_TYPEDEF ) then
			exit while
		end if

		hReplaceTypedefBaseSubtype( typedef, anon, aliastypedef )

		typedef = typedef->next
	wend

	'' Rename the anon UDT to the alias typedef's id, now that its old
	'' __fbfrog_anon* isn't need for comparison above anymore
	astSetText( anon, aliastypedef->text )

	'' "Remove" the alias typedef
	aliastypedef->class = ASTCLASS_NOP
end sub

sub astFixAnonUDTs( byval n as ASTNODE ptr )
	var udt = n->head
	while( udt )

		'' Anon UDT?
		select case( udt->class )
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			if( strStartsWith( *udt->text, FROG_ANON_PREFIX ) ) then
				hTryFixAnon( udt )
			end if
		end select

		udt = udt->next
	wend
end sub

'' Removes typedefs where the typedef identifier is the same as the struct tag,
'' e.g. "typedef struct T T;" since FB doesn't have separate struct/type
'' namespaces and such typedefs aren't needed.
sub astRemoveRedundantTypedefs( byval n as ASTNODE ptr )
	var child = n->head
	while( child )
		astRemoveRedundantTypedefs( child )
		child = child->next
	wend

	if( (n->class = ASTCLASS_TYPEDEF) and _
	    (typeGetDtAndPtr( n->dtype ) = TYPE_UDT) ) then
		assert( n->subtype->class = ASTCLASS_ID )
		if( ucase( *n->text, 1 ) = ucase( *n->subtype->text ) ) then
			n->class = ASTCLASS_NOP
		end if
	end if
end sub

sub astMergeDIVIDERs( byval n as ASTNODE ptr )
	assert( n->class = ASTCLASS_GROUP )

	var child = n->head
	while( child )
		var nxt = child->next

		if( nxt ) then
			if( (child->class = ASTCLASS_DIVIDER) and _
			    (  nxt->class = ASTCLASS_DIVIDER) ) then
				astRemoveChild( n, child )
			end if
		end if

		child = nxt
	wend
end sub

type DECLNODE
	decl as ASTNODE ptr     '' The declaration at that index
	version as ASTNODE ptr  '' Parent VERSION node of the declaration
end type

type DECLTABLE
	array	as DECLNODE ptr
	count	as integer
	room	as integer
end type

private sub hAddDecl _
	( _
		byval c as ASTNODE ptr, _
		byval array as DECLNODE ptr, _
		byval i as integer _
	)

	astAddVersionedChild( c, _
		astNewVERBLOCK( astClone( array[i].version ), NULL, astClone( array[i].decl ) ) )

end sub

'' See also hTurnCallConvIntoExternBlock():
''
'' Procdecls with callconv covered by the Extern block are given the
'' ASTATTRIB_HIDECALLCONV flag.
''
'' If we're merging two procdecls here, and they both have
'' ASTATTRIB_HIDECALLCONV, then they can be emitted without explicit
'' callconv, as the Extern blocks will take care of that and remap
'' the callconv as needed. In this case, the merged node shouldn't have
'' any callconv flag at all, but only ASTATTRIB_HIDECALLCONV.
'' hAstLCS() calls astIsEqualDecl() with the proper flags to allow this.
''
'' If merging two procdecls and only one side has
'' ASTATTRIB_HIDECALLCONV, then they must have the same callconv,
'' otherwise hAstLCS()'s astIsEqualDecl() call wouldn't have treated
'' them as equal. In this case the callconv must be preserved on
'' the merged node, so it will be emitted explicitly, since the Extern
'' blocks don't cover it. ASTATTRIB_HIDECALLCONV shouldn't be preserved
'' in this case.
''
'' The same applies to procptr subtypes, though to handle it for those,
'' we need a recursive function.

private sub hFindCommonCallConvsOnMergedDecl _
	( _
		byval mdecl as ASTNODE ptr, _
		byval adecl as ASTNODE ptr, _
		byval bdecl as ASTNODE ptr _
	)

	assert( mdecl->class = adecl->class )
	assert( adecl->class = bdecl->class )

	if( mdecl->class = ASTCLASS_PROC ) then
		if( ((adecl->attrib and ASTATTRIB_HIDECALLCONV) <> 0) and _
		    ((bdecl->attrib and ASTATTRIB_HIDECALLCONV) <> 0) ) then
			mdecl->attrib and= not (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)
			assert( mdecl->attrib and ASTATTRIB_HIDECALLCONV ) '' was preserved by astClone() already
		elseif( ((adecl->attrib and ASTATTRIB_HIDECALLCONV) <> 0) or _
			((bdecl->attrib and ASTATTRIB_HIDECALLCONV) <> 0) ) then
			assert( (adecl->attrib and (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)) = _
				(bdecl->attrib and (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)) )
			mdecl->attrib and= not ASTATTRIB_HIDECALLCONV
			assert( (mdecl->attrib and (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)) <> 0 ) '' ditto
		end if
	end if

	'' Don't forget the procptr subtypes
	if( typeGetDt( mdecl->dtype ) = TYPE_PROC ) then
		assert( typeGetDt( adecl->dtype ) = TYPE_PROC )
		assert( typeGetDt( bdecl->dtype ) = TYPE_PROC )
		hFindCommonCallConvsOnMergedDecl( mdecl->subtype, adecl->subtype, bdecl->subtype )
	end if

	var mchild = mdecl->head
	var achild = adecl->head
	var bchild = bdecl->head
	while( mchild )
		assert( achild )
		assert( bchild )

		hFindCommonCallConvsOnMergedDecl( mchild, achild, bchild )

		mchild = mchild->next
		achild = achild->next
		bchild = bchild->next
	wend
	assert( mchild = NULL )
	assert( achild = NULL )
	assert( bchild = NULL )
end sub

private sub hAddMergedDecl _
	( _
		byval c as ASTNODE ptr, _
		byval aarray as DECLNODE ptr, _
		byval ai as integer, _
		byval barray as DECLNODE ptr, _
		byval bi as integer _
	)

	var adecl = aarray[ai].decl
	var bdecl = barray[bi].decl
	var mdecl = astClone( adecl )

	hFindCommonCallConvsOnMergedDecl( mdecl, adecl, bdecl )

	astAddVersionedChild( c, _
		astNewVERBLOCK( _
			astClone( aarray[ai].version ), _
			astClone( barray[bi].version ), _
			mdecl ) )

end sub

'' Determine longest common substring, by building an l x r matrix:
''
'' if l[i] = r[j] then
''     if( i-1 or j-1 would be out-of-bounds ) then
''         matrix[i][j] = 1
''     else
''         matrix[i][j] = matrix[i-1][j-1] + 1
''     end if
'' else
''     matrix[i][j] = 0
'' end if
''
'' 0 = characters not equal
'' 1 = characters equal
'' 2 = characters equal here, and also at one position to top/left
'' ...
'' i.e. the non-zero diagonal parts in the matrix determine the common
'' substrings.
''
'' Some examples:
''
''             Longest common substring:
''   b a a c           b a a c
'' a 0 1 1 0         a   1
'' a 0 1 2 0         a     2
'' b 1 0 0 0         b
'' c 0 0 0 1         c
''
''   c a a b           c a a b
'' a 0 1 1 0         a   1
'' a 0 1 2 0         a     2
'' b 1 0 0 3         b       3
'' c 1 0 0 0         c
''
''   b a b c           b a b c
'' c 0 1 0 1         c
'' a 0 1 0 0         a   1
'' b 1 0 2 0         b     2
'' c 0 0 0 3         c       3
''
private sub hAstLCS _
	( _
		byval larray as DECLNODE ptr, _
		byval lfirst as integer, _
		byval llast as integer, _
		byref llcsfirst as integer, _
		byref llcslast as integer, _
		byval rarray as DECLNODE ptr, _
		byval rfirst as integer, _
		byval rlast as integer, _
		byref rlcsfirst as integer, _
		byref rlcslast as integer _
	)

	var llen = llast - lfirst + 1
	var rlen = rlast - rfirst + 1
	var max = 0, maxi = 0, maxj = 0

	dim as integer ptr matrix = callocate( sizeof( integer ) * llen * rlen )

	for i as integer = 0 to llen-1
		for j as integer = 0 to rlen-1
			var newval = 0
			if( astIsEqualDecl( larray[lfirst+i].decl, _
			                    rarray[rfirst+j].decl, _
			                    TRUE, TRUE ) ) then
				if( (i = 0) or (j = 0) ) then
					newval = 1
				else
					newval = matrix[(i-1)+((j-1)*llen)] + 1
				end if
			end if
			if( max < newval ) then
				max = newval
				maxi = i
				maxj = j
			end if
			matrix[i+(j*llen)] = newval
		next
	next

	deallocate( matrix )

	llcsfirst = lfirst + maxi - max + 1
	rlcsfirst = rfirst + maxj - max + 1
	llcslast  = llcsfirst + max - 1
	rlcslast  = rlcsfirst + max - 1
end sub

private function hMergeStructsManually _
	( _
		byval astruct as ASTNODE ptr, _
		byval aversion as ASTNODE ptr, _
		byval bstruct as ASTNODE ptr, _
		byval bversion as ASTNODE ptr _
	) as ASTNODE ptr

	''
	'' For example:
	''
	''     version 1                   version 2
	''         struct FOO                  struct FOO
	''             field a as integer          field a as integer
	''             field b as integer          field c as integer
	''
	'' should become:
	''
	''     version 1, 2
	''         struct FOO
	''             field a as integer
	''             version 1
	''                 field b as integer
	''             version 2
	''                 field c as integer
	''
	'' instead of:
	''
	''     version 1
	''         struct FOO
	''             field a as integer
	''             field b as integer
	''     version 2
	''         struct FOO
	''             field a as integer
	''             field c as integer
	''

	'' Copy astruct's fields into temp VERSION for a's version(s)
	var afields = astNewGROUP( )
	astCloneAndAddAllChildrenOf( afields, astruct )
	afields = astNewGROUP( astNewVERBLOCK( astClone( aversion ), NULL, afields ) )

	'' Copy bstruct's fields into temp VERSION for b's version(s)
	var bfields = astNewGROUP( )
	astCloneAndAddAllChildrenOf( bfields, bstruct )
	bfields = astNewGROUP( astNewVERBLOCK( astClone( bversion ), NULL, bfields ) )

	'' Merge both set of fields
	var fields = astMergeVerBlocks( afields, bfields )

	'' Solve out any VERSIONs (but preserving their children) that have the
	'' same version numbers that the struct itself is going to have.
	fields = astRemoveVerBlockWrapping( fields, _
			astNewBOP( ASTOP_OR, astClone( aversion ), astClone( bversion ) ) )

	'' Create a result struct with the new set of fields
	var cstruct = astCloneNode( astruct )
	astAppend( cstruct, fields )

	function = cstruct
end function

private sub hAstMerge _
	( _
		byval c as ASTNODE ptr, _
		byval aarray as DECLNODE ptr, _
		byval afirst as integer, _
		byval alast as integer, _
		byval barray as DECLNODE ptr, _
		byval bfirst as integer, _
		byval blast as integer _
	)

	static reclevel as integer
	#if 0
		#define DEBUG( x ) print string( reclevel + 1, " " ) & x
	#else
		#define DEBUG( x )
	#endif

	DEBUG( "hAstMerge( reclevel=" & reclevel & ", a=" & afirst & ".." & alast & ", b=" & bfirst & ".." & blast & " )" )

	'' No longest common substring possible?
	if( afirst > alast ) then
		'' Add bfirst..blast to result
		DEBUG( "no LCS possible due to a, adding b as-is" )
		for i as integer = bfirst to blast
			hAddDecl( c, barray, i )
		next
		exit sub
	elseif( bfirst > blast ) then
		'' Add afirst..alast to result
		DEBUG( "no LCS possible due to b, adding a as-is" )
		for i as integer = afirst to alast
			hAddDecl( c, aarray, i )
		next
		exit sub
	end if

	'' Find longest common substring
	DEBUG( "searching LCS..." )
	dim as integer alcsfirst, alcslast, blcsfirst, blcslast
	hAstLCS( aarray, afirst, alast, alcsfirst, alcslast, _
	         barray, bfirst, blast, blcsfirst, blcslast )
	DEBUG( "LCS: a=" & alcsfirst & ".." & alcslast & ", b=" & blcsfirst & ".." & blcslast )

	'' No LCS found?
	if( alcsfirst > alcslast ) then
		'' Add a first, then b. This order makes the most sense: keeping
		'' the old declarations at the top, add new ones to the bottom.
		DEBUG( "no LCS found, adding both as-is" )
		for i as integer = afirst to alast
			hAddDecl( c, aarray, i )
		next
		for i as integer = bfirst to blast
			hAddDecl( c, barray, i )
		next
		exit sub
	end if

	'' Do both sides have decls before the LCS?
	if( (alcsfirst > afirst) and (blcsfirst > bfirst) ) then
		'' Do LCS on that recursively
		DEBUG( "both sides have decls before LCS, recursing" )
		reclevel += 1
		hAstMerge( c, aarray, afirst, alcsfirst - 1, _
		              barray, bfirst, blcsfirst - 1 )
		reclevel -= 1
	elseif( alcsfirst > afirst ) then
		'' Only a has decls before the LCS; copy them into result first
		DEBUG( "only a has decls before LCS" )
		for i as integer = afirst to alcsfirst - 1
			hAddDecl( c, aarray, i )
		next
	elseif( blcsfirst > bfirst ) then
		'' Only b has decls before the LCS; copy them into result first
		DEBUG( "only b has decls before LCS" )
		for i as integer = bfirst to blcsfirst - 1
			hAddDecl( c, barray, i )
		next
	end if

	'' Add LCS
	DEBUG( "adding LCS" )
	assert( (alcslast - alcsfirst + 1) = (blcslast - blcsfirst + 1) )
	for i as integer = 0 to (alcslast - alcsfirst + 1)-1
		'' The LCS may include merged structs/unions/enums that were put
		'' into the LCS despite having different fields on both sides.
		''
		'' They should be merged recursively now, so the struct/union/enum itself
		'' can be common, while the fields/enumconsts may be version dependant.
		''
		'' (relying on hAstLCS() to allow structs/unions/enums to match
		'' even if they have different fields/enumconsts)
		var astruct = aarray[alcsfirst+i].decl
		select case( astruct->class )
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			var aversion = aarray[alcsfirst+i].version
			var bstruct  = barray[blcsfirst+i].decl
			var bversion = barray[blcsfirst+i].version
			assert( bstruct->class = ASTCLASS_STRUCT )

			var cstruct = hMergeStructsManually( astruct, aversion, bstruct, bversion )

			'' Add struct to result tree, under both a's and b's version numbers
			astAddVersionedChild( c, astNewVERBLOCK( astClone( aversion ), astClone( bversion ), cstruct ) )

			continue for
		end select

		hAddMergedDecl( c, aarray, alcsfirst + i, barray, blcsfirst + i )
	next

	'' Do both sides have decls behind the LCS?
	if( (alcslast < alast) and (blcslast < blast) ) then
		'' Do LCS on that recursively
		DEBUG( "both sides have decls behind LCS, recursing" )
		reclevel += 1
		hAstMerge( c, aarray, alcslast + 1, alast, barray, blcslast + 1, blast )
		reclevel -= 1
	elseif( alcslast < alast ) then
		'' Only a has decls behind the LCS
		DEBUG( "only a has decls behind LCS" )
		for i as integer = alcslast + 1 to alast
			hAddDecl( c, aarray, i )
		next
	elseif( blcslast < blast ) then
		'' Only b has decls behind the LCS
		DEBUG( "only b has decls behind LCS" )
		for i as integer = blcslast + 1 to blast
			hAddDecl( c, barray, i )
		next
	end if

end sub

private sub decltableAdd _
	( _
		byval table as DECLTABLE ptr, _
		byval decl as ASTNODE ptr, _
		byval version as ASTNODE ptr _
	)

	if( table->count = table->room ) then
		table->room += 256
		table->array = reallocate( table->array, table->room * sizeof( DECLNODE ) )
	end if

	with( table->array[table->count] )
		.decl = decl
		.version = version
	end with

	table->count += 1

end sub

private sub decltableInit _
	( _
		byval table as DECLTABLE ptr, _
		byval code as ASTNODE ptr _
	)

	table->array = NULL
	table->count = 0
	table->room = 0

	'' Add each declaration node from the AST to the table
	'' For each VERBLOCK...
	assert( code->class = ASTCLASS_GROUP )
	var verblock = code->head
	while( verblock )
		assert( verblock->class = ASTCLASS_VERBLOCK )

		'' For each declaration in that VERBLOCK...
		var decl = verblock->head
		while( decl )
			decltableAdd( table, decl, verblock->expr )
			decl = decl->next
		wend

		verblock = verblock->next
	wend
end sub

private sub decltableEnd( byval table as DECLTABLE ptr )
	deallocate( table->array )
end sub

function astMergeVerBlocks _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr _
	) as ASTNODE ptr

	#if 0
		print "a:"
		astDump( a, 1 )
		print "b:"
		astDump( b, 1 )
	#endif

	if( a = NULL ) then
		a = astNewGROUP( )
	end if
	if( b = NULL ) then
		b = astNewGROUP( )
	end if

	var c = astNewGROUP( )
	assert( a->class = ASTCLASS_GROUP )
	assert( b->class = ASTCLASS_GROUP )

	'' Create a lookup table for each side, so we can find the declarations
	'' at certain indices in O(1) instead of having to cycle through the
	'' whole list of preceding nodes everytime. Especially by the LCS
	'' algorithm needs to find declaratinos by index a lot, this makes that
	'' much faster.
	dim atable as DECLTABLE
	dim btable as DECLTABLE

	decltableInit( @atable, a )
	decltableInit( @btable, b )

	hAstMerge( c, atable.array, 0, atable.count - 1, _
	              btable.array, 0, btable.count - 1 )

	decltableEnd( @btable )
	decltableEnd( @atable )

	#if 0
		print "c:"
		astDump( c, 1 )
	#endif

	function = c
end function

function astMergeFiles _
	( _
		byval files1 as ASTNODE ptr, _
		byval files2 as ASTNODE ptr _
	) as ASTNODE ptr

	if( files1 = NULL ) then
		return files2
	end if

	'' Merge files2 into files1
	''
	'' For example:
	'' files1:
	''    libfoo-1/foo.h
	''    libfoo-1/fooconfig.h
	'' files2:
	''    libfoo-2/foo.h
	''    libfoo-2/foo2.h
	'' foo.h exists in both versions and thus the two should be combined,
	'' but the other files only exist in their respective version,
	'' so they shouldn't be combined, just preserved.

	var f2 = files2->head
	while( f2 )

		'' Does a file with similar name exist in files1?
		var name2 = pathStrip( *f2->text )

		var f1 = files1->head
		while( f1 )

			var name1 = pathStrip( *f1->text )
			if( name1 = name2 ) then
				exit while
			end if

			f1 = f1->next
		wend

		if( f1 ) then
			'' File found in files1; merge the two files' ASTs
			if( (f1->expr <> NULL) and (f2->expr <> NULL) ) then
				f1->expr = astMergeVerBlocks( f1->expr, f2->expr )
			elseif( f2->expr ) then
				f1->expr = f2->expr
			end if
			f2->expr = NULL
		else
			'' File exists only in files2, copy over to files1
			astAppend( files1, astClone( f2 ) )
		end if

		f2 = f2->next
	wend

	astDelete( files2 )
	function = files1
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function astDumpOne( byval n as ASTNODE ptr ) as string
	dim as string s

	if( n = NULL ) then
		return "<NULL>"
	end if

	#if 0
		s += "[" & hex( n ) & "] "
	#endif

	s += astnodeinfo(n->class).name

	#macro checkAttrib( a )
		if( n->attrib and ASTATTRIB_##a ) then s += " " + lcase( #a, 1 )
	#endmacro
	checkAttrib( EXTERN )
	checkAttrib( PRIVATE )
	checkAttrib( OCT )
	checkAttrib( HEX )
	checkAttrib( CDECL )
	checkAttrib( STDCALL )
	checkAttrib( HIDECALLCONV )
	checkAttrib( REMOVE )
	checkAttrib( REPORTED )

	if( n->class <> ASTCLASS_TK ) then
		if( n->text ) then
			s += " """ + strMakePrintable( *n->text ) + """"
		end if
	end if

	select case( n->class )
	case ASTCLASS_CONST
		if( typeIsFloat( n->dtype ) ) then
			s += " " + str( n->valf )
		else
			if( n->attrib and ASTATTRIB_OCT ) then
				s += " &o" + oct( n->vali )
			elseif( n->attrib and ASTATTRIB_HEX ) then
				s += " &h" + hex( n->vali )
			else
				s += " " + str( n->vali )
			end if
		end if

	case ASTCLASS_TK
		s += " " + tkDumpBasic( n->tk, n->text )

	case ASTCLASS_UOP, ASTCLASS_BOP
		static as zstring * 16 ops(ASTOP_CLOGOR to ASTOP_SIZEOF) = _
		{ _
			"c ||"		, _ '' ASTOP_CLOGOR
			"c &&"		, _ '' ASTOP_CLOGAND
			"orelse"	, _ '' ASTOP_ORELSE
			"andalso"	, _ '' ASTOP_ANDALSO
			"or"		, _ '' ASTOP_OR
			"xor"		, _ '' ASTOP_XOR
			"and"		, _ '' ASTOP_AND
			"c ="		, _ '' ASTOP_CEQ
			"c <>"		, _ '' ASTOP_CNE
			"c <"		, _ '' ASTOP_CLT
			"c <="		, _ '' ASTOP_CLE
			"c >"		, _ '' ASTOP_CGT
			"c >="		, _ '' ASTOP_CGE
			"="		, _ '' ASTOP_EQ
			"<>"		, _ '' ASTOP_NE
			"<"		, _ '' ASTOP_LT
			"<="		, _ '' ASTOP_LE
			">"		, _ '' ASTOP_GT
			">="		, _ '' ASTOP_GE
			"shl"		, _ '' ASTOP_SHL
			"shr"		, _ '' ASTOP_SHR
			"+"		, _ '' ASTOP_ADD
			"-"		, _ '' ASTOP_SUB
			"*"		, _ '' ASTOP_MUL
			"/"		, _ '' ASTOP_DIV
			"mod"		, _ '' ASTOP_MOD
			"[]"		, _ '' ASTOP_INDEX
			"."		, _ '' ASTOP_MEMBER
			"->"		, _ '' ASTOP_MEMBERDEREF
			"str +"		, _ '' ASTOP_STRCAT
			"C !"		, _ '' ASTOP_CLOGNOT
			"not"		, _ '' ASTOP_NOT
			"negate"	, _ '' ASTOP_NEGATE
			"unary +"	, _ '' ASTOP_UNARYPLUS
			"C defined()"	, _ '' ASTOP_CDEFINED
			"defined()"	, _ '' ASTOP_DEFINED
			"@"		, _ '' ASTOP_ADDROF
			"deref"		, _ '' ASTOP_DEREF
			"#"		, _ '' ASTOP_STRINGIFY
			"sizeof"	  _ '' ASTOP_SIZEOF
		}

		s += " " + ops(n->op)
	end select

	if( n->dtype <> TYPE_NONE ) then
		s += " as " + emitType( n->dtype, NULL, TRUE )
	end if

	s += hDumpComment( n->comment )

	function = s
end function

function astDumpInline( byval n as ASTNODE ptr ) as string
	var s = astDumpOne( n )
	var args = 0

	#macro more( arg )
		if( args = 0 ) then
			s += "("
		else
			s += ", "
		end if
		s += arg
		args += 1
	#endmacro

	if( n->subtype ) then
		more( "subtype=" + astDumpInline( n->subtype ) )
	end if
	if( n->array ) then
		more( "array=" + astDumpInline( n->array ) )
	end if
	if( n->expr ) then
		more( "expr=" + astDumpInline( n->expr ) )
	end if
	if( n->l ) then
		more( "l=" + astDumpInline( n->l ) )
	end if
	if( n->r ) then
		more( "r=" + astDumpInline( n->r ) )
	end if

	var child = n->head
	while( child )
		more( astDumpInline( child ) )
		child = child->next
	wend

	if( args > 0 ) then
		s += ")"
	end if

	function = s
end function

private sub hPrintIndentation( byval nestlevel as integer )
	for i as integer = 2 to nestlevel
		print "   ";
	next
end sub

sub astDump _
	( _
		byval n as ASTNODE ptr, _
		byval nestlevel as integer, _
		byref prefix as string _
	)

	nestlevel += 1

	if( n ) then
		hPrintIndentation( nestlevel )
		if( len( prefix ) > 0 ) then
			print prefix + ": ";
		end if
		print astDumpOne( n )

		#macro dumpField( field )
			if( n->field ) then
				astDump( n->field, nestlevel, #field )
			end if
		#endmacro

		dumpField( subtype )
		dumpField( array )
		dumpField( expr )
		dumpField( l )
		dumpField( r )

		var child = n->head
		while( child )
			astDump( child, nestlevel )
			child = child->next
		wend
	else
		hPrintIndentation( nestlevel )
		print "<NULL>"
	end if

	nestlevel -= 1
end sub

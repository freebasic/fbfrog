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
	( "version"  ), _
	( "divider"  ), _
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
	( "uop"     ), _
	( "bop"     ), _
	( "iif"     ), _
	( "call"    )  _
}

#assert ubound( astnodeinfo ) = ASTCLASS__COUNT - 1

function astNew overload( byval class_ as integer ) as ASTNODE ptr
	dim as ASTNODE ptr n = callocate( sizeof( ASTNODE ) )
	n->class = class_
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

function astNewVERSION overload( ) as ASTNODE ptr
	var n = astNew( ASTCLASS_VERSION )
	n->expr = astNewGROUP( )
	function = n
end function

function astNewVERSION overload _
	( _
		byval versionnum as ASTNODE ptr, _
		byval child as ASTNODE ptr _
	) as ASTNODE ptr

	var n = astNewVERSION( )

	assert( versionnum->class = ASTCLASS_TEXT )
	astAppend( n->expr, versionnum )
	astAppend( n, child )

	function = n
end function

function astNewVERSION overload _
	( _
		byval version1 as ASTNODE ptr, _
		byval version2 as ASTNODE ptr, _
		byval child as ASTNODE ptr _
	) as ASTNODE ptr

	var n = astNewVERSION( )

	assert( version1->class = ASTCLASS_VERSION )
	astCloneAndAddAllChildrenOf( n->expr, version1->expr )

	if( version2 ) then
		assert( version2->class = ASTCLASS_VERSION )
		astCloneAndAddAllChildrenOf( n->expr, version2->expr )
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

function astNewTK _
	( _
		byval tk as integer, _
		byval text as zstring ptr _
	) as ASTNODE ptr

	var n = astNew( ASTCLASS_TK, text )
	n->tk = tk

	function = n
end function

function astNewMACROPARAM _
	( _
		byval id as zstring ptr, _
		byval paramindex as integer _
	) as ASTNODE ptr

	var n = astNew( ASTCLASS_MACROPARAM, id )
	n->paramindex = paramindex

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

function astVersionsMatch( byval a as ASTNODE ptr, byval b as ASTNODE ptr ) as integer
	assert( a->class = ASTCLASS_VERSION )
	assert( b->class = ASTCLASS_VERSION )
	function = astIsEqualDecl( a->expr, b->expr )
end function

sub astAddVersionedChild( byval n as ASTNODE ptr, byval child as ASTNODE ptr )
	assert( n->class = ASTCLASS_GROUP )
	assert( child->class = ASTCLASS_VERSION )

	'' If the tree's last VERSION node has the same version numbers, then
	'' just add the new children nodes to that instead of opening a new
	'' separate VERSION node.
	if( n->tail ) then
		assert( n->tail->class = ASTCLASS_VERSION )
		if( astVersionsMatch( n->tail, child ) ) then
			astCloneAndAddAllChildrenOf( n->tail, child )
			astDelete( child )
			exit sub
		end if
	end if

	astAppend( n, child )
end sub

function astSolveVersionsOut _
	( _
		byval nodes as ASTNODE ptr, _
		byval matchversion as ASTNODE ptr _
	) as ASTNODE ptr

	var cleannodes = astNewGROUP( )

	assert( nodes->class = ASTCLASS_GROUP )
	var version = nodes->head
	while( version )
		assert( version->class = ASTCLASS_VERSION )

		if( astVersionsMatch( version, matchversion ) ) then
			'' Add only the VERSION's child nodes
			astCloneAndAddAllChildrenOf( cleannodes, version )
		else
			'' Add the whole VERSION
			astAppend( cleannodes, astClone( version ) )
		end if

		version = version->next
	wend

	astDelete( nodes )
	astDelete( matchversion )
	function = cleannodes
end function

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
	case ASTCLASS_MACROPARAM
		c->paramindex = n->paramindex
	case ASTCLASS_PPDEFINE
		c->paramcount = n->paramcount
	case ASTCLASS_UOP, ASTCLASS_BOP
		c->op = n->op
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

	if( (a->attrib and ASTATTRIB_MERGEWITHPREV) <> _
	    (b->attrib and ASTATTRIB_MERGEWITHPREV) ) then
		exit function
	end if

	if( (a->attrib and ASTATTRIB_STRINGIFY) <> _
	    (b->attrib and ASTATTRIB_STRINGIFY) ) then
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

	case ASTCLASS_MACROPARAM
		if( a->paramindex <> b->paramindex ) then exit function

	case ASTCLASS_PPDEFINE
		if( a->paramcount <> b->paramcount ) then exit function

	case ASTCLASS_UOP, ASTCLASS_BOP
		if( a->op <> b->op ) then exit function

	case ASTCLASS_STRUCT
		if( ignore_fields ) then return TRUE
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
		astNewVERSION( array[i].version, NULL, astClone( array[i].decl ) ) )

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
		astNewVERSION( aarray[ai].version, barray[bi].version, mdecl ) )

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
	afields = astNewVERSION( aversion, NULL, afields )

	'' Copy bstruct's fields into temp VERSION for b's version(s)
	var bfields = astNewGROUP( )
	astCloneAndAddAllChildrenOf( bfields, bstruct )
	bfields = astNewVERSION( bversion, NULL, bfields )

	'' Merge both set of fields
	var fields = astMergeVersions( astMergeVersions( NULL, afields ), bfields )

	'' Solve out any VERSIONs (but preserving their children) that have the
	'' same version numbers that the struct itself is going to have.
	var cleanfields = astSolveVersionsOut( fields, _
			astNewVERSION( aversion, bversion, NULL ) )

	'' Create a result struct with the new set of fields
	var cstruct = astCloneNode( astruct )
	astAppend( cstruct, cleanfields )

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
		'' The LCS may include structs but with different fields on both
		'' sides, they must be merged manually so the struct itself can
		'' be common, but the fields may be version dependant.
		'' (relying on hAstLCS() to allow structs to match even if they
		'' have different fields)
		var astruct = aarray[alcsfirst+i].decl
		if( astruct->class = ASTCLASS_STRUCT ) then
			var aversion = aarray[alcsfirst+i].version
			var bstruct  = barray[blcsfirst+i].decl
			var bversion = barray[blcsfirst+i].version
			assert( bstruct->class = ASTCLASS_STRUCT )

			var cstruct = hMergeStructsManually( astruct, aversion, bstruct, bversion )

			'' Add struct to result tree, under both a's and b's version numbers
			astAddVersionedChild( c, astNewVERSION( aversion, bversion, cstruct ) )

			continue for
		end if

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

private sub decltableInit( byval table as DECLTABLE ptr, byval n as ASTNODE ptr )
	table->array = NULL
	table->count = 0
	table->room = 0

	'' Add each declaration node from the AST to the table
	if( n = NULL ) then
		exit sub
	end if

	var version = n
	if( version->class = ASTCLASS_GROUP ) then
		version = version->head
		if( version = NULL ) then
			exit sub
		end if
	end if

	'' For each VERSION...
	do
		assert( version->class = ASTCLASS_VERSION )

		'' For each declaration in that VERSION...
		var decl = version->head
		while( decl )
			decltableAdd( table, decl, version )
			decl = decl->next
		wend

		version = version->next
	loop while( version )
end sub

private sub decltableEnd( byval table as DECLTABLE ptr )
	deallocate( table->array )
end sub

function astMergeVersions _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr _
	) as ASTNODE ptr

	'' a = existing GROUP holding one or more VERSIONs, or NULL
	'' b = new VERSION that should be integrated into a
	'' c = resulting GROUP holding one or more VERSIONs

	if( b = NULL ) then
		return astClone( a )
	end if

	var c = astNewGROUP( )

	if( a = NULL ) then
		astAppend( c, astClone( b ) )
		return c
	end if

	#if 0
		print "a:"
		astDump( a, 1 )
		print "b:"
		astDump( b, 1 )
	#endif

	assert( a->class = ASTCLASS_GROUP )
	assert( a->head->class = ASTCLASS_VERSION )
	assert( b->class = ASTCLASS_VERSION )

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
	checkAttrib( MERGEWITHPREV )
	checkAttrib( STRINGIFY )
	checkAttrib( CDECL )
	checkAttrib( STDCALL )
	checkAttrib( HIDECALLCONV )

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
		static as zstring * 10 ops(ASTOP_LOGOR to ASTOP_DEFINED) = _
		{ _
			"orelse"  , _  '' ASTOP_LOGOR
			"andalso" , _  '' ASTOP_LOGAND
			"or"      , _  '' ASTOP_BITOR
			"xor"     , _  '' ASTOP_BITXOR
			"and"     , _  '' ASTOP_BITAND
			"="       , _  '' ASTOP_EQ
			"<>"      , _  '' ASTOP_NE
			"<"       , _  '' ASTOP_LT
			"<="      , _  '' ASTOP_LE
			">"       , _  '' ASTOP_GT
			">="      , _  '' ASTOP_GE
			"shl"     , _  '' ASTOP_SHL
			"shr"     , _  '' ASTOP_SHR
			"+"       , _  '' ASTOP_ADD
			"-"       , _  '' ASTOP_SUB
			"*"       , _  '' ASTOP_MUL
			"/"       , _  '' ASTOP_DIV
			"mod"     , _  '' ASTOP_MOD
			"lognot"  , _  '' ASTOP_LOGNOT
			"bitnot"  , _  '' ASTOP_BITNOT
			"negate"  , _  '' ASTOP_NEGATE
			"unary +" , _  '' ASTOP_UNARYPLUS
			"defined()" _  '' ASTOP_DEFINED
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

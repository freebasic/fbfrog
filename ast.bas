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
	( "#if"      ), _
	( "#elseif"  ), _
	( "#else"    ), _
	( "#endif"   ), _
	( "#undef"   ), _
	( "#unknown" ), _
	( "struct"  ), _
	( "union"   ), _
	( "enum"    ), _
	( "typedef" ), _
	( "structfwd" ), _
	( "var"     ), _
	( "field"   ), _
	( "enumconst" ), _
	( "proc"    ), _
	( "param"   ), _
	( "array"   ), _
	( "dimension" ), _
	( "unknown" ), _
	( "macrobody" ), _
	( "macroparam" ), _
	( "tk"      ), _
	( "const"   ), _
	( "id"      ), _
	( "text"    ), _
	( "defined" ), _
	( "iif"     ), _
	( "orelse"  ), _
	( "andalso" ), _
	( "or"  ), _
	( "xor" ), _
	( "and" ), _
	( "="   ), _
	( "<>"  ), _
	( "<"   ), _
	( "<="  ), _
	( ">"   ), _
	( ">="  ), _
	( "shl" ), _
	( "shr" ), _
	( "+"   ), _
	( "-"   ), _
	( "*"   ), _
	( "/"   ), _
	( "mod" ), _
	( "lognot"  ), _
	( "not"     ), _
	( "unary -" ), _
	( "unary +" )  _
}

#if ubound( astnodeinfo ) < ASTCLASS__COUNT - 1
#error "please update the astnodeinfo() table!"
#endif

function astNew overload( byval class_ as integer ) as ASTNODE ptr
	dim as ASTNODE ptr n = callocate( sizeof( ASTNODE ) )
	n->class = class_
	function = n
end function

function astNew overload _
	( _
		byval class_ as integer, _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr, _
		byval c as ASTNODE ptr _
	) as ASTNODE ptr

	var n = astNew( class_ )
	astAddChild( n, a )
	astAddChild( n, b )
	astAddChild( n, c )

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

private function astNewVERSION overload( ) as ASTNODE ptr
	var n = astNew( ASTCLASS_VERSION )
	n->initializer = astNew( ASTCLASS_GROUP )
	function = n
end function

function astNewVERSION overload _
	( _
		byval child as ASTNODE ptr, _
		byval versionnum as integer _
	) as ASTNODE ptr

	var n = astNewVERSION( )

	astAddChild( n->initializer, astNewCONST( versionnum, 0, TYPE_INTEGER ) )
	astAddChild( n, child )

	function = n
end function

function astNewVERSION overload _
	( _
		byval child as ASTNODE ptr, _
		byval version1 as ASTNODE ptr, _
		byval version2 as ASTNODE ptr _
	) as ASTNODE ptr

	var n = astNewVERSION( )

	assert( version1->class = ASTCLASS_VERSION )
	astCloneAndAddAllChildrenOf( n->initializer, version1->initializer )

	if( version2 ) then
		assert( version2->class = ASTCLASS_VERSION )
		astCloneAndAddAllChildrenOf( n->initializer, version2->initializer )
	end if

	astAddChild( n, child )

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
		n->val.f = f
	else
		n->val.i = i
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

function astNewMACROPARAM( byval paramindex as integer ) as ASTNODE ptr
	var n = astNew( ASTCLASS_MACROPARAM )
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

	astDelete( n->initializer )
	astDelete( n->array )
	deallocate( n->text )
	astDelete( n->subtype )
	deallocate( n )
end sub

sub astAddChild( byval parent as ASTNODE ptr, byval n as ASTNODE ptr )
	if( n = NULL ) then
		exit sub
	end if

	select case( n->class )
	case ASTCLASS_GROUP
		'' If it's a GROUP, add its children and delete the GROUP itself
		var child = n->head
		while( child )
			astAddChild( parent, astClone( child ) )
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
		astAddChild( d, astClone( child ) )
		child = child->next
	wend
end sub

sub astAddVersionedChild( byval n as ASTNODE ptr, byval child as ASTNODE ptr )
	assert( n->class = ASTCLASS_GROUP )
	assert( child->class = ASTCLASS_VERSION )

	'' If the tree's last VERSION node has the same version numbers, then
	'' just add the new children nodes to that instead of opening a new
	'' separate VERSION node.
	if( n->tail ) then
		assert( n->tail->class = ASTCLASS_VERSION )
		if( astIsEqualDecl( n->tail->initializer, child->initializer ) ) then
			astCloneAndAddAllChildrenOf( n->tail, child )
			astDelete( child )
			exit sub
		end if
	end if

	astAddChild( n, child )
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

	n->comment = strDuplicate( s )
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

	c->initializer = astClone( n->initializer )

	c->sourcefile  = n->sourcefile
	c->sourceline  = n->sourceline

	c->includefile = n->includefile

	c->val         = n->val
	c->tk          = n->tk
	c->paramindex  = n->paramindex
	c->paramcount  = n->paramcount

	function = c
end function

function astClone( byval n as ASTNODE ptr ) as ASTNODE ptr
	var c = astCloneNode( n )
	if( c = NULL ) then
		return NULL
	end if

	var child = n->head
	while( child )
		astAddChild( c, astClone( child ) )
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
		byval ignore_fields as integer _
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

	if( (a->text <> NULL) and (b->text <> NULL) ) then
		if( *a->text <> *b->text ) then exit function
	else
		if( (a->text <> NULL) <> (b->text <> NULL) ) then exit function
	end if

	if( a->dtype <> b->dtype ) then exit function
	if( astIsEqualDecl( a->subtype, b->subtype, ignore_fields ) = FALSE ) then exit function
	if( astIsEqualDecl( a->array, b->array, ignore_fields ) = FALSE ) then exit function

	if( astIsEqualDecl( a->initializer, b->initializer, ignore_fields ) = FALSE ) then exit function

	if( a->includefile <> b->includefile ) then exit function

	select case( a->class )
	case ASTCLASS_CONST
		if( typeIsFloat( a->dtype ) ) then
			const EPSILON_DBL as double = 2.2204460492503131e-016
			if( abs( a->val.f - b->val.f ) >= EPSILON_DBL ) then exit function
		else
			if( a->val.i <> b->val.i ) then exit function
		end if

	case ASTCLASS_TK
		if( a->tk <> b->tk ) then exit function

	case ASTCLASS_MACROPARAM
		if( a->paramindex <> b->paramindex ) then exit function

	case ASTCLASS_PPDEFINE
		if( a->paramcount <> b->paramcount ) then exit function

	case ASTCLASS_STRUCT
		if( ignore_fields ) then return TRUE
	end select

	'' Children
	a = a->head
	b = b->head
	while( (a <> NULL) and (b <> NULL) )
		if( astIsEqualDecl( a, b, ignore_fields ) = FALSE ) then
			exit function
		end if
		a = a->next
		b = b->next
	wend

	'' Both a's and b's last child must be reached at the same time
	function = ((a = NULL) and (b = NULL))
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

	if( n->text ) then
		s += " " + strReplace( *n->text, !"\n", "\n" )
	end if

	select case( n->class )
	case ASTCLASS_CONST
		if( typeIsFloat( n->dtype ) ) then
			s += " " + str( n->val.f )
		else
			if( n->attrib and ASTATTRIB_OCT ) then
				s += " &o" + oct( n->val.i )
			elseif( n->attrib and ASTATTRIB_HEX ) then
				s += " &h" + hex( n->val.i )
			else
				s += " " + str( n->val.i )
			end if
		end if
	case ASTCLASS_TK
		s += " " + tkDumpBasic( n->tk, n->text )
	end select

	if( n->dtype <> TYPE_NONE ) then
		s += " as " + emitType( n->dtype, NULL, TRUE )
	end if

	#macro checkAttrib( a )
		if( n->attrib and ASTATTRIB_##a ) then s += " " + lcase( #a, 1 )
	#endmacro
	checkAttrib( EXTERN )
	checkAttrib( PRIVATE )
	checkAttrib( OCT )
	checkAttrib( HEX )
	checkAttrib( PPINDENTBEGIN )
	checkAttrib( PPINDENTEND )
	checkAttrib( MERGEWITHPREV )
	checkAttrib( STRINGIFY )

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
	if( n->initializer ) then
		more( "initializer=" + astDumpInline( n->initializer ) )
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

sub astDump( byval n as ASTNODE ptr, byval nestlevel as integer )
	nestlevel += 1

	if( n ) then
		hPrintIndentation( nestlevel )
		print astDumpOne( n )

		if( n->subtype ) then
			nestlevel += 1
			hPrintIndentation( nestlevel )
			print "subtype:"
			astDump( n->subtype, nestlevel )
			nestlevel -= 1
		end if

		if( n->array ) then
			nestlevel += 1
			hPrintIndentation( nestlevel )
			print "array:"
			astDump( n->array, nestlevel )
			nestlevel -= 1
		end if

		if( n->initializer ) then
			nestlevel += 1
			hPrintIndentation( nestlevel )
			print "initializer:"
			astDump( n->initializer, nestlevel )
			nestlevel -= 1
		end if

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

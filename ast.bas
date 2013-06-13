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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type ASTNODEINFO
	name		as zstring * 10
	is_expr		as integer
end type

dim shared as ASTNODEINFO astnodeinfo(0 to ...) = _
{ _
	( "nop"     , FALSE ), _
	( "group"   , FALSE ), _
	( "divider" , FALSE ), _
	( "#include", FALSE ), _
	( "#define" , FALSE ), _
	( "#if"     , FALSE ), _
	( "#elseif" , FALSE ), _
	( "#else"   , FALSE ), _
	( "#endif"  , FALSE ), _
	( "#unknown", FALSE ), _
	( "struct"  , FALSE ), _
	( "typedef" , FALSE ), _
	( "var"     , FALSE ), _
	( "field"   , FALSE ), _
	( "proc"    , FALSE ), _
	( "param"   , FALSE ), _
	( "unknown" , FALSE ), _
	( "const"   , TRUE  ), _
	( "id"      , TRUE  ), _
	( "text"    , TRUE  ), _
	( "defined" , TRUE  ), _
	( "iif"     , TRUE  ), _
	( "orelse"  , TRUE  ), _
	( "andalso" , TRUE  ), _
	( "or"      , TRUE  ), _
	( "xor"     , TRUE  ), _
	( "and"     , TRUE  ), _
	( "="       , TRUE  ), _
	( "<>"      , TRUE  ), _
	( "<"       , TRUE  ), _
	( "<="      , TRUE  ), _
	( ">"       , TRUE  ), _
	( ">="      , TRUE  ), _
	( "shl"     , TRUE  ), _
	( "shr"     , TRUE  ), _
	( "+"       , TRUE  ), _
	( "-"       , TRUE  ), _
	( "*"       , TRUE  ), _
	( "/"       , TRUE  ), _
	( "mod"     , TRUE  ), _
	( "lognot"  , TRUE  ), _
	( "not"     , TRUE  ), _
	( "unary -" , TRUE  ), _
	( "unary +" , TRUE  )  _
}

#if ubound( astnodeinfo ) < ASTCLASS__COUNT - 1
#error "please update the astnodeinfo() table!"
#endif

function astIsExpr( byval n as ASTNODE ptr ) as integer
	function = astnodeinfo(n->class).is_expr
end function

function astNew overload( byval class_ as integer ) as ASTNODE ptr
	dim as ASTNODE ptr n = any

	n = callocate( sizeof( ASTNODE ) )
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

	dim as ASTNODE ptr n = any

	n = astNew( class_ )
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

	dim as ASTNODE ptr n = any

	n = astNew( class_ )
	n->text = strDuplicate( text )

	function = n
end function

sub astDelete( byval n as ASTNODE ptr )
	dim as ASTNODE ptr child = any, nxt = any

	if( n = NULL ) then
		exit sub
	end if

	deallocate( n->text )
	astDelete( n->subtype )

	if( astIsExpr( n ) ) then
		astDelete( n->l )
		astDelete( n->r )
		astDelete( n->next )
	else
		child = n->l
		while( child )
			nxt = child->next
			astDelete( child )
			child = nxt
		wend
	end if

	deallocate( n )
end sub

sub astAddChild( byval parent as ASTNODE ptr, byval t as ASTNODE ptr )
	dim as ASTNODE ptr child = any

	if( t = NULL ) then
		exit sub
	end if

	select case( t->class )
	case ASTCLASS_GROUP
		'' If it's a GROUP, add its children, and delete the GROUP itself
		child = t->l
		while( child )
			astAddChild( parent, astClone( child ) )
			child = child->next
		wend

		astDelete( t )
		exit sub

	case ASTCLASS_NOP
		'' Don't bother adding NOPs
		astDelete( t )
		exit sub

	end select

	assert( astIsExpr( parent ) = FALSE )
	t->prev = parent->r
	if( parent->r ) then
		parent->r->next = t
	end if
	parent->r = t
	if( parent->l = NULL ) then
		parent->l = t
	end if
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

sub astCopyNodeData( byval d as ASTNODE ptr, byval s as ASTNODE ptr )
	d->attrib     = s->attrib
	d->text       = strDuplicate( s->text )
	d->comment    = strDuplicate( s->comment )
	d->intval     = s->intval
	d->dtype      = s->dtype
	d->subtype    = astClone( s->subtype )
	d->sourcefile = s->sourcefile
	d->sourceline = s->sourceline
end sub

function astClone( byval n as ASTNODE ptr ) as ASTNODE ptr
	dim as ASTNODE ptr c = any, child = any

	if( n = NULL ) then
		return NULL
	end if

	c = astNew( n->class )
	astCopyNodeData( c, n )

	if( astIsExpr( n ) ) then
		c->l = astClone( n->l )
		c->r = astClone( n->r )
		c->next = astClone( n->next )
	else
		child = n->l
		while( child )
			astAddChild( c, astClone( child ) )
			child = child->next
		wend
	end if

	function = c
end function

function astNewCONSTi _
	( _
		byval intval as longint, _
		byval dtype as integer _
	) as ASTNODE ptr

	dim as ASTNODE ptr n = any

	n = astNew( ASTCLASS_CONST )
	n->dtype = dtype
	n->intval = intval

	function = n
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

	if( n->class = ASTCLASS_CONST ) then
		s += " " + str( n->intval )
	end if

	if( n->dtype <> TYPE_NONE ) then
		s += " as " + emitType( n->dtype, NULL, TRUE )
	end if

	function = s
end function

private sub hPrintIndentation( byval nestlevel as integer )
	for i as integer = 2 to nestlevel
		print "   ";
	next
end sub

sub astDump( byval n as ASTNODE ptr, byval nestlevel as integer )
	dim as ASTNODE ptr child = any

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

		if( astIsExpr( n ) ) then
			if( n->l ) then
				astDump( n->l, nestlevel )
			end if
			if( n->r ) then
				astDump( n->r, nestlevel )
			end if
			if( n->next ) then
				astDump( n->next, nestlevel )
			end if
		else
			child = n->l
			if( child ) then
				do
					astDump( child, nestlevel )
					child = child->next
				loop while( child )
			end if
		end if
	else
		hPrintIndentation( nestlevel )
		print "<NULL>"
	end if

	nestlevel -= 1
end sub

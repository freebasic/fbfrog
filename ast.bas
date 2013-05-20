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

function astNew( byval class_ as integer ) as ASTNODE ptr
	dim as ASTNODE ptr n = any

	n = callocate( sizeof( ASTNODE ) )
	n->class = class_

	function = n
end function

sub astDelete( byval n as ASTNODE ptr )
	dim as ASTNODE ptr child = any, nxt = any

	if( n = NULL ) then
		exit sub
	end if

	deallocate( n->id )
	deallocate( n->text )
	astDelete( n->subtype )

	astDelete( n->l )
	astDelete( n->r )

	child = n->childhead
	while( child )
		nxt = child->next
		astDelete( child )
		child = nxt
	wend

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
		child = t->childhead
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

	t->prev = parent->childtail
	if( parent->childtail ) then
		parent->childtail->next = t
	end if
	parent->childtail = t
	if( parent->childhead = NULL ) then
		parent->childhead = t
	end if
end sub

sub astSetId( byval n as ASTNODE ptr, byval id as zstring ptr )
	deallocate( n->id )
	n->id = strDuplicate( id )
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

function astClone( byval n as ASTNODE ptr ) as ASTNODE ptr
	dim as ASTNODE ptr c = any, child = any

	if( n = NULL ) then
		return NULL
	end if

	c = astNew( n->class )
	c->attrib     = n->attrib
	c->id         = strDuplicate( n->id )
	c->text       = strDuplicate( n->text )
	c->comment    = strDuplicate( n->comment )
	c->intval     = n->intval
	c->dtype      = n->dtype
	c->subtype    = astClone( n->subtype )
	c->sourcefile = n->sourcefile
	c->sourceline = n->sourceline

	c->l = astClone( n->l )
	c->r = astClone( n->r )

	child = n->childhead
	while( child )
		astAddChild( c, astClone( child ) )
		child = child->next
	wend

	function = c
end function

function astNewPPDEFINE( byval id as zstring ptr ) as ASTNODE ptr
	dim as ASTNODE ptr n = any
	n = astNew( ASTCLASS_PPDEFINE )
	n->id = strDuplicate( id )
	function = n
end function

function astNewPPINCLUDE( byval filename as zstring ptr ) as ASTNODE ptr
	dim as ASTNODE ptr n = any
	n = astNew( ASTCLASS_PPINCLUDE )
	n->text = strDuplicate( filename )
	function = n
end function

function astNewPPIF( byval expr as ASTNODE ptr ) as ASTNODE ptr
	dim as ASTNODE ptr n = any
	n = astNew( ASTCLASS_PPIF )
	n->l = expr
	function = n
end function

function astNewPPELSE( ) as ASTNODE ptr
	function = astNew( ASTCLASS_PPELSE )
end function

function astNewPPENDIF( ) as ASTNODE ptr
	function = astNew( ASTCLASS_PPENDIF )
end function

function astNewPPUNKNOWN( byval text as zstring ptr ) as ASTNODE ptr
	dim as ASTNODE ptr n = any
	n = astNew( ASTCLASS_PPUNKNOWN )
	n->text = strDuplicate( text )
	function = n
end function

function astNewUNKNOWN( byval text as zstring ptr ) as ASTNODE ptr
	dim as ASTNODE ptr n = any
	n = astNew( ASTCLASS_UNKNOWN )
	n->text = strDuplicate( text )
	function = n
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

function astNewID( byval id as zstring ptr ) as ASTNODE ptr
	dim as ASTNODE ptr n = any
	n = astNew( ASTCLASS_ID )
	n->id = strDuplicate( id )
	function = n
end function

function astNewDEFINED( byval l as ASTNODE ptr ) as ASTNODE ptr
	dim as ASTNODE ptr n = any
	n = astNew( ASTCLASS_DEFINED )
	n->l = l
	function = n
end function

function astNewLOGICNOT( byval l as ASTNODE ptr ) as ASTNODE ptr
	dim as ASTNODE ptr n = any
	n = astNew( ASTCLASS_LOGICNOT )
	n->l = l
	function = n
end function

dim shared as zstring ptr astclassnames(0 to ASTCLASS__COUNT-1) = _
{ _
	@"nop"     , _
	@"group"   , _
	@"divider" , _
	@"#include", _
	@"#define" , _
	@"#if"     , _
	@"#else"   , _
	@"#endif"  , _
	@"#unknown", _
	@"struct"  , _
	@"typedef" , _
	@"var"     , _
	@"field"   , _
	@"proc"    , _
	@"param"   , _
	@"unknown" , _
	@"const"   , _
	@"id"      , _
	@"defined" , _
	@"logicnot"  _
}

function astDumpOne( byval n as ASTNODE ptr ) as string
	dim as string s

	if( n = NULL ) then
		return "<NULL>"
	end if

	#if 0
		s += "[" & hex( n ) & "] "
	#endif

	s += *astclassnames(n->class)

	if( n->id ) then
		s += " " + *n->id
	end if

	if( n->text ) then
		s += " """ + *n->text + """"
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

sub astDump( byval n as ASTNODE ptr )
	static as integer nestlevel
	dim as ASTNODE ptr child = any

	nestlevel += 1

	if( n ) then
		hPrintIndentation( nestlevel )
		print astDumpOne( n )

		if( n->subtype ) then
			nestlevel += 1
			hPrintIndentation( nestlevel )
			print "subtype:"
			astDump( n->subtype )
			nestlevel -= 1
		end if

		child = n->childhead
		if( child ) then
			do
				astDump( child )
				child = child->next
			loop while( child )
		end if
	else
		hPrintIndentation( nestlevel )
		print "<NULL>"
	end if

	nestlevel -= 1
end sub

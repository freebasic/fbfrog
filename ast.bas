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

	child = n->head
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
		child = t->head
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

	t->prev = parent->tail
	if( parent->tail ) then
		parent->tail->next = t
	end if
	parent->tail = t
	if( parent->head = NULL ) then
		parent->head = t
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

function astClone( byval n as ASTNODE ptr ) as ASTNODE ptr
	dim as ASTNODE ptr c = any, child = any

	if( n = NULL ) then
		return NULL
	end if

	c = astNew( n->class )
	c->attrib     = n->attrib
	c->text       = strDuplicate( n->text )
	c->comment    = strDuplicate( n->comment )
	c->intval     = n->intval
	c->dtype      = n->dtype
	c->subtype    = astClone( n->subtype )
	c->sourcefile = n->sourcefile
	c->sourceline = n->sourceline

	child = n->head
	while( child )
		astAddChild( c, astClone( child ) )
		child = child->next
	wend

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

dim shared as zstring ptr astclassnames(0 to ASTCLASS__COUNT-1) = _
{ _
	@"nop"     , _
	@"group"   , _
	@"divider" , _
	_
	@"#include", _
	@"#define" , _
	@"#if"     , _
	@"#else"   , _
	@"#endif"  , _
	@"#unknown", _
	_
	@"struct"  , _
	@"typedef" , _
	@"var"     , _
	@"field"   , _
	@"proc"    , _
	@"param"   , _
	@"unknown" , _
	_
	@"const"   , _
	@"id"      , _
	@"text"    , _
	@"defined" , _
	_
	@"iif"     , _
	@"orelse"  , _
	@"andalso" , _
	@"or"      , _
	@"xor"     , _
	@"and"     , _
	@"="       , _
	@"<>"      , _
	@"<"       , _
	@"<="      , _
	@">"       , _
	@">="      , _
	@"shl"     , _
	@"shr"     , _
	@"+"       , _
	@"-"       , _
	@"*"       , _
	@"/"       , _
	@"mod"     , _
	@"lognot"  , _
	@"not"     , _
	@"unary -" , _
	@"unary +"   _
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

	if( n->text ) then
		s += " " + *n->text
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

		child = n->head
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

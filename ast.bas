'' AST build up/helper functions

#include once "fbfrog.bi"

'' Merge/expand dtype b into dtype a, overwriting a's base dtype, but preserving its ptrs/consts
'' This is useful for expanding typedefs into the context of another dtype.
function typeExpand( byval a as integer, byval b as integer ) as integer
	var acount = typeGetPtrCount( a )
	var bcount = typeGetPtrCount( b )
	if( acount + bcount > TYPEMAX_PTR ) then
		return TYPE_NONE
	end if
	function = typeMultAddrOf( b, acount ) or typeGetConst( a )
end function

function typeUnsetBaseConst( byval dtype as integer ) as integer
	function = dtype and (not (1 shl (TYPEPOS_CONST + typeGetPtrCount( dtype ))))
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

dim shared as zstring ptr astnodename(0 to ...) => _
{ _
	_ '' Internal helper nodes
	@"group"     , _
	@"verblock"  , _
	@"veror"     , _
	@"verand"    , _
	@"divider"   , _
	@"scopeblock", _
	@"unknown"   , _
	@"renamelist", _
	_
	_ '' Script helper nodes
	@"declaredefines", _
	@"declareversions", _
	@"declarebool"  , _
	@"select"       , _
	@"case"         , _
	@"caseelse"     , _
	@"endselect"    , _
	@"file"         , _
	@"incdir"       , _
	@"filterout"    , _
	@"filterin"     , _
	@"inclib"       , _
	@"preinclude"   , _
	@"fbfrogpreinclude", _
	@"pragmaonce"   , _
	_
	_ '' Preprocessor directives
	@"ppinclude", _
	@"ppdefine" , _
	@"ppif"     , _
	@"ppelseif" , _
	@"ppelse"   , _
	@"ppendif"  , _
	@"pperror"  , _
	_
	_ '' Declarations/statements
	@"struct"     , _
	@"union"      , _
	@"enum"       , _
	@"typedef"    , _
	@"enumconst"  , _
	@"var"        , _
	@"field"      , _
	@"proc"       , _
	@"param"      , _
	@"array"      , _
	@"externbegin", _
	@"externend"  , _
	@"return"     , _
	_
	_ '' Expression atoms etc.
	@"sym"       , _
	@"macroparam", _
	@"consti"    , _
	@"constf"    , _
	@"text"      , _
	@"string"    , _
	@"char"      , _
	@"datatype"  , _
	@"ellipsis"  , _
	@"renamelistentry", _
	_
	_ '' BOPs
	@"clogor" , _
	@"clogand", _
	@"or"     , _
	@"xor"    , _
	@"and"    , _
	@"ceq"    , _
	@"cne"    , _
	@"clt"    , _
	@"cle"    , _
	@"cgt"    , _
	@"cge"    , _
	@"eq"     , _
	@"shl"    , _
	@"shr"    , _
	@"add"    , _
	@"sub"    , _
	@"mul"    , _
	@"div"    , _
	@"mod"    , _
	@"index"  , _
	@"member" , _
	@"memberderef", _
	@"strcat" , _
	_
	_ '' UOPs
	@"clognot"    , _
	@"not"        , _
	@"negate"     , _
	@"unaryplus"  , _
	@"cdefined"   , _
	@"defined"    , _
	@"addrof"     , _
	@"deref"      , _
	@"stringify"  , _
	@"sizeof"     , _
	@"cast"       , _
	_
	_ '' Special expressions
	@"iif"       , _
	@"ppmerge"   , _
	@"call"      , _
	@"structinit", _
	@"arrayinit" , _
	@"dimension"   _
}

#assert ubound( astnodename ) = ASTCLASS__COUNT - 1

private function astNew overload( ) as ASTNODE ptr
	function = callocate( sizeof( ASTNODE ) )
end function

function astNew overload( byval class_ as integer ) as ASTNODE ptr
	dim as ASTNODE ptr n = callocate( sizeof( ASTNODE ) )
	n->class = class_
	function = n
end function

function astNew overload( byval class_ as integer, byval text as zstring ptr ) as ASTNODE ptr
	var n = astNew( class_ )
	n->text = strDuplicate( text )
	function = n
end function

function astNew overload( byval class_ as integer, byval c1 as ASTNODE ptr, byval c2 as ASTNODE ptr ) as ASTNODE ptr
	var n = astNew( class_ )
	astAppend( n, c1 )
	astAppend( n, c2 )
	function = n
end function

function astNewPPDEFINE( byval id as zstring ptr ) as ASTNODE ptr
	var n = astNew( ASTCLASS_PPDEFINE, id )
	n->paramcount = -1
	function = n
end function

function astNewIIF( byval cond as ASTNODE ptr, byval l as ASTNODE ptr, byval r as ASTNODE ptr ) as ASTNODE ptr
	var n = astNew( ASTCLASS_IIF, l, r )
	n->expr = cond
	function = n
end function

function astNewGROUP overload( ) as ASTNODE ptr
	function = astNew( ASTCLASS_GROUP )
end function

function astNewGROUP overload( byval c1 as ASTNODE ptr, byval c2 as ASTNODE ptr ) as ASTNODE ptr
	var n = astNewGROUP( )
	astAppend( n, c1 )
	astAppend( n, c2 )
	function = n
end function

function astNewSYM( byval sym as ASTNODE ptr ) as ASTNODE ptr
	var n = astNew( ASTCLASS_SYM )
	n->expr = sym
	function = n
end function

sub astMoveChildren( byval d as ASTNODE ptr, byval s as ASTNODE ptr )
	d->head = s->head
	d->tail = s->tail
	s->head = NULL
	s->tail = NULL
end sub

function astCloneChildren( byval src as ASTNODE ptr ) as ASTNODE ptr
	var n = astNewGROUP( )
	var i = src->head
	while( i )
		astAppend( n, astClone( i ) )
		i = i->next
	wend
	function = n
end function

function astGroupContains( byval group as ASTNODE ptr, byval lookfor as ASTNODE ptr ) as integer
	var i = group->head
	while( i )
		if( astIsEqual( i, lookfor ) ) then
			return TRUE
		end if
		i = i->next
	wend
	function = FALSE
end function

function astGroupContainsAnyChildrenOf( byval l as ASTNODE ptr, byval r as ASTNODE ptr ) as integer
	var i = r->head
	while( i )
		if( astGroupContains( l, i ) ) then return TRUE
		i = i->next
	wend
end function

function astGroupContainsAllChildrenOf( byval l as ASTNODE ptr, byval r as ASTNODE ptr ) as integer
	var i = r->head
	while( i )
		if( astGroupContains( l, i ) = FALSE ) then exit function
		i = i->next
	wend
	function = TRUE
end function

private function astGroupsContainEqualChildren( byval l as ASTNODE ptr, byval r as ASTNODE ptr ) as integer
	function = astGroupContainsAllChildrenOf( l, r ) and astGroupContainsAllChildrenOf( r, l )
end function

#if __FB_DEBUG__
private function astIsChildOf _
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
#endif

sub astInsert _
	( _
		byval parent as ASTNODE ptr, _
		byval n as ASTNODE ptr, _
		byval ref as ASTNODE ptr, _
		byval unique as integer = FALSE _
	)

	if( n = NULL ) then exit sub

	assert( astIsChildOf( parent, n ) = FALSE )

	select case( n->class )
	'' If it's a GROUP, insert its children, and delete the GROUP itself
	case ASTCLASS_GROUP
		var i = n->head
		while( i )
			astInsert( parent, astClone( i ), ref, unique )
			i = i->next
		wend
		exit sub
	end select

	'' If requested, don't insert if it already exists in the list
	if( unique ) then
		var i = parent->head
		while( i )
			if( astIsEqual( i, n ) ) then
				exit sub
			end if
			i = i->next
		wend
	end if

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

sub astPrepend( byval parent as ASTNODE ptr, byval n as ASTNODE ptr )
	astInsert( parent, n, parent->head )
end sub

sub astAppend( byval parent as ASTNODE ptr, byval n as ASTNODE ptr )
	astInsert( parent, n, NULL )
end sub

function astRemove( byval parent as ASTNODE ptr, byval a as ASTNODE ptr ) as ASTNODE ptr
	assert( a )
	assert( astIsChildOf( parent, a ) )

	function = a->next

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

	a->next = NULL
	a->prev = NULL
end function

sub astRemoveChildren( byval parent as ASTNODE ptr )
	while( parent->head )
		astRemove( parent, parent->head )
	wend
end sub

function astReplace _
	( _
		byval parent as ASTNODE ptr, _
		byval old as ASTNODE ptr, _
		byval n as ASTNODE ptr _
	) as ASTNODE ptr
	assert( old )
	astInsert( parent, n, old )
	function = astRemove( parent, old )
end function

sub astSetText( byval n as ASTNODE ptr, byval text as zstring ptr )
	n->text = strDuplicate( text )
end sub

sub astRenameSymbol( byval n as ASTNODE ptr, byval newid as zstring ptr )
	if( n->alias = NULL ) then
		n->alias = n->text
	end if
	n->text = strDuplicate( newid )
end sub

function astGetOrigId( byval n as ASTNODE ptr ) as zstring ptr
	function = iif( n->alias, n->alias, n->text )
end function

private sub astCopyNode( byval d as ASTNODE ptr, byval s as ASTNODE ptr )
	d->class   = s->class
	d->attrib  = s->attrib
	d->text    = s->text
	d->alias   = s->alias
	d->dtype   = s->dtype
	d->subtype = s->subtype
	d->array   = s->array
	d->bits    = s->bits
	d->expr    = s->expr
	select case( s->class )
	case ASTCLASS_PPDEFINE : d->paramcount = s->paramcount
	case ASTCLASS_STRUCT, ASTCLASS_UNION : d->maxalign = s->maxalign
	end select
end sub

sub astCopy( byval d as ASTNODE ptr, byval s as ASTNODE ptr )
	astCopyNode( d, s )
	var i = s->head
	while( i )
		astAppend( d, astClone( i ) )
		i = i->next
	wend
end sub

'' astClone() but without children
function astCloneNode( byval n as ASTNODE ptr ) as ASTNODE ptr
	if( n = NULL ) then exit function
	var c = astNew( )
	astCopyNode( c, n )
	function = c
end function

function astClone( byval n as ASTNODE ptr ) as ASTNODE ptr
	if( n = NULL ) then exit function
	var c = astNew( )
	astCopy( c, n )
	function = c
end function

function astIsMergableBlock( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, ASTCLASS_RENAMELIST
		function = TRUE
	end select
end function

''
'' Check whether two ASTs are "equal".
''
'' For the purposes of merging, the rules are somewhat relaxed: two procedure
'' declarations with different calling conventions can be treated as equal if
'' they're both covered by an Extern block, because then we won't emit the
'' calling convention explicitly anyways, and the declarations look exactly the
'' same.
''
'' For VEROR and VERAND nodes the order of children does not matter; we treat
'' them as equal either way.
''
function astIsEqual _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr, _
		byval mode as integer _
	) as integer

	if( a = b ) then return TRUE
	if( (a = NULL) or (b = NULL) ) then exit function

	if( a->class <> b->class ) then exit function

	var aattrib = a->attrib
	var battrib = b->attrib

	if( mode = ASTISEQUAL_MERGE ) then
		'' If callconv is hidden on both sides, then ignore it
		if( (aattrib and ASTATTRIB_HIDECALLCONV) and (battrib and ASTATTRIB_HIDECALLCONV) ) then
			aattrib and= not (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)
			battrib and= not (ASTATTRIB_CDECL or ASTATTRIB_STDCALL)
		end if

		'' Other than that, we should ignore ASTATTRIB_HIDECALLCONV itself,
		'' allowing hFindCommonCallConvsOnMergedDecl() to handle cases where
		'' both sides have the same callconv but different hiding status.
		aattrib and= not ASTATTRIB_HIDECALLCONV
		battrib and= not ASTATTRIB_HIDECALLCONV

		'' Ignore DLLIMPORT on procedures for now, as we're not emitting it anyways
		if( a->class = ASTCLASS_PROC ) then
			aattrib and= not ASTATTRIB_DLLIMPORT
			battrib and= not ASTATTRIB_DLLIMPORT
		end if
	end if

	'' Some attributes could perhaps always be ignored because they don't cause declarations to really be different.
	'' However, OCT/HEX attributes mustn't be ignored because otherwise number literals with the same digits but
	'' a different base could incorrectly compare equal.

	if( aattrib <> battrib ) then exit function

	'' When comparing procedure signatures, parameter names don't matter
	if( (mode <> ASTISEQUAL_SIGNATURE) or (a->class <> ASTCLASS_PARAM) ) then
		if( (a->text <> NULL) <> (b->text <> NULL) ) then exit function
		if( a->text ) then
			if( *a->text <> *b->text ) then exit function
		end if

		if( (a->alias <> NULL) <> (b->alias <> NULL) ) then exit function
		if( a->alias ) then if( *a->alias <> *b->alias ) then exit function
	end if

	if( a->dtype <> b->dtype ) then exit function
	if( (mode = ASTISEQUAL_MERGE) and (typeGetDt( a->dtype ) = TYPE_UDT) ) then
		assert( a->subtype->text )
		assert( b->subtype->text )
		if( *a->subtype->text <> *b->subtype->text ) then exit function
	else
		if( astIsEqual( a->subtype, b->subtype, mode ) = FALSE ) then exit function
	end if
	if( astIsEqual( a->array, b->array, mode ) = FALSE ) then exit function
	if( astIsEqual( a->bits, b->bits, mode ) = FALSE ) then exit function

	if( a->class = ASTCLASS_SYM ) then
		if( *a->expr->text <> *b->expr->text ) then exit function
	else
		if( astIsEqual( a->expr, b->expr, mode ) = FALSE ) then exit function
	end if

	select case( a->class )
	case ASTCLASS_PPDEFINE
		if( a->paramcount <> b->paramcount ) then exit function

	case ASTCLASS_STRUCT, ASTCLASS_UNION
		if( a->maxalign <> b->maxalign ) then exit function

	case ASTCLASS_VEROR, ASTCLASS_VERAND
		return astGroupsContainEqualChildren( a, b )
	end select

	if( mode = ASTISEQUAL_MERGE ) then
		if( astIsMergableBlock( a ) ) then
			return TRUE
		end if
	end if

	'' Children
	a = a->head
	b = b->head
	while( (a <> NULL) and (b <> NULL) )
		if( astIsEqual( a, b, mode ) = FALSE ) then
			exit function
		end if
		a = a->next
		b = b->next
	wend

	'' Both a's and b's last child must be reached at the same time
	function = ((a = NULL) and (b = NULL))
end function

sub astReport _
	( _
		byval n as ASTNODE ptr, _
		byval message as zstring ptr, _
		byval more_context as integer _
	)

	print *message
	print astDumpPrettyDecl( n )

end sub

sub astOops _
	( _
		byval n as ASTNODE ptr, _
		byval message as zstring ptr, _
		byval more_context as integer _
	)
	astReport( n, message, more_context )
	end 1
end sub

function hGetFbNumberLiteralPrefix( byval attrib as integer ) as string
	if( attrib and ASTATTRIB_BIN ) then
		function = "&b"
	elseif( attrib and ASTATTRIB_OCT ) then
		function = "&o"
	elseif( attrib and ASTATTRIB_HEX ) then
		function = "&h"
	end if
end function

function astEvalConstiAsInt64( byval n as ASTNODE ptr ) as longint
	assert( astIsCONSTI( n ) )
	function = vallng( hGetFbNumberLiteralPrefix( n->attrib ) + *n->text )
end function

function astIsConst0( byval n as ASTNODE ptr ) as integer
	if( astIsCONSTI( n ) ) then
		function = (astEvalConstiAsInt64( n ) = 0)
	end if
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' AST dumping for pretty output and debugging

function astDumpPrettyClass( byval astclass as integer ) as string
	select case( astclass )
	case ASTCLASS_PPDEFINE : function = "#define"
	case ASTCLASS_ENUMCONST : function = "enum constant"
	case ASTCLASS_VAR   : function = "variable"
	case ASTCLASS_PROC  : function = "procedure"
	case else           : function = *astnodename(astclass)
	end select
end function

function astDumpPrettyDecl( byval n as ASTNODE ptr ) as string
	dim as string s

	if( n->text = NULL ) then
		s += "anonymous "
	end if

	s += astDumpPrettyClass( n->class )

	if( n->text ) then
		s += " " + strMakePrintable( *n->text )
	end if

	if( n->alias ) then
		s += " alias """ + strMakePrintable( *n->alias ) + """"
	end if

	if( n->class = ASTCLASS_PROC ) then
		s += "()"
	end if

	function = s
end function

function astDumpOne( byval n as ASTNODE ptr ) as string
	dim as string s

	if( n = NULL ) then
		return "<NULL>"
	end if

	's += "[" & hex( n ) & "] "
	s += *astnodename(n->class)

	#macro checkAttrib( a )
		if( n->attrib and ASTATTRIB_##a ) then s += " " + lcase( #a, 1 )
	#endmacro
	checkAttrib( LOCAL )
	checkAttrib( STATIC )
	checkAttrib( EXTERN )
	checkAttrib( OCT )
	checkAttrib( HEX )
	checkAttrib( CDECL )
	checkAttrib( STDCALL )
	checkAttrib( HIDECALLCONV )
	checkAttrib( NAMEOVERRIDDEN )
	checkAttrib( POISONED )
	checkAttrib( PACKED )
	checkAttrib( VARIADIC )
	checkAttrib( PARENTHESIZEDMACROPARAM )
	checkAttrib( GENERATEDID )
	checkAttrib( DLLIMPORT )
	checkAttrib( FILTEROUT )
	checkAttrib( BODYDEFINED )
	checkAttrib( USEBEFOREDEF )
	checkAttrib( DONTADDFWDREF )
	checkAttrib( RENAMED )

	if( n->text ) then
		s += " """ + strMakePrintable( *n->text ) + """"
	end if
	if( n->alias ) then
		s += " alias """ + strMakePrintable( *n->alias ) + """"
	end if

	if( n->dtype <> TYPE_NONE ) then
		s += " as " + emitType( n->dtype, NULL, TRUE )
	end if

	function = s
end function

sub astDump _
	( _
		byval n as ASTNODE ptr, _
		byval nestlevel as integer, _
		byref prefix as string _
	)

	nestlevel += 1

	var s = space( (nestlevel - 1) * 3 )

	if( n ) then
		if( len( prefix ) > 0 ) then
			s += prefix + ": "
		end if
		s += astDumpOne( n )
		print s

		if( n->subtype ) then
			if( typeGetDt( n->dtype ) = TYPE_PROC ) then
				astDump( n->subtype, nestlevel, "subtype" )
			else
				print space( nestlevel * 3 ) + "subtype -> " + astDumpPrettyDecl( n->subtype )
			end if
		end if

		if( n->array ) then astDump( n->array, nestlevel, "array" )

		if( n->class = ASTCLASS_SYM ) then
			print space( nestlevel * 3 ) + "expr -> " + astDumpPrettyDecl( n->expr )
		else
			if( n->expr ) then astDump( n->expr, nestlevel, "expr" )
		end if

		var child = n->head
		while( child )
			astDump( child, nestlevel )
			child = child->next
		wend
	else
		s += "<NULL>"
		print s
	end if

	nestlevel -= 1
end sub

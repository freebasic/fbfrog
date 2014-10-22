'' Higher level code transformations

#include once "fbfrog.bi"

function astOpsC2FB( byval n as ASTNODE ptr, byval is_bool_context as integer ) as ASTNODE ptr
	if( n = NULL ) then exit function

	select case( n->class )
	case ASTCLASS_CLOGNOT, ASTCLASS_CLOGOR, ASTCLASS_CLOGAND, _
	     ASTCLASS_CEQ, ASTCLASS_CNE, ASTCLASS_IIF
		var expr_is_bool_context = FALSE
		var l_is_bool_context = FALSE
		var r_is_bool_context = FALSE

		select case( n->class )
		'' ! operand is treated as bool
		case ASTCLASS_CLOGNOT
			l_is_bool_context = TRUE

		'' andalso/orelse operands are always treated as bools
		case ASTCLASS_CLOGOR, ASTCLASS_CLOGAND
			l_is_bool_context = TRUE
			r_is_bool_context = TRUE

		'' BOP operands may sometimes be in bool context:
		''    x = 0
		''    x <> 0
		'' (not x = -1 and x <> -1 because if checks check against 0 and <> 0,
		'' not 0 and -1)
		case ASTCLASS_CEQ, ASTCLASS_CNE
			if( astIsCONSTI( n->tail ) ) then
				l_is_bool_context = (astEvalConstiAsInt64( n->tail ) = 0)
			end if

		'' iif() condition always is treated as bool
		case ASTCLASS_IIF
			expr_is_bool_context = TRUE
		end select

		n->expr = astOpsC2FB( n->expr, expr_is_bool_context )
		if( n->head ) then
			astReplace( n, n->head, astOpsC2FB( astClone( n->head ), l_is_bool_context ) )
			if( n->head <> n->tail ) then
				assert( n->head->next = n->tail )
				astReplace( n, n->tail, astOpsC2FB( astClone( n->tail ), r_is_bool_context ) )
			end if
		end if

	case else
		var i = n->head
		while( i )
			i = astReplace( n, i, astOpsC2FB( astClone( i ), FALSE ) )
		wend
	end select

	select case( n->class )
	case ASTCLASS_CLOGNOT, ASTCLASS_CDEFINED, _
	     ASTCLASS_CLOGOR, ASTCLASS_CLOGAND, _
	     ASTCLASS_CEQ, ASTCLASS_CNE, _
	     ASTCLASS_CLT, ASTCLASS_CLE, _
	     ASTCLASS_CGT, ASTCLASS_CGE

		select case( n->class )
		case ASTCLASS_CLOGNOT
			'' Turn C's "!x" into FB's "x = 0"
			n->class = ASTCLASS_EQ
			var zero = astNew( ASTCLASS_CONSTI, "0" )
			astSetType( zero, TYPE_LONG, NULL )
			astAppend( n, zero )
		case ASTCLASS_CDEFINED : n->class = ASTCLASS_DEFINED
		case ASTCLASS_CLOGOR   : n->class = ASTCLASS_ORELSE
		case ASTCLASS_CLOGAND  : n->class = ASTCLASS_ANDALSO
		case ASTCLASS_CEQ      : n->class = ASTCLASS_EQ
		case ASTCLASS_CNE      : n->class = ASTCLASS_NE
		case ASTCLASS_CLT      : n->class = ASTCLASS_LT
		case ASTCLASS_CLE      : n->class = ASTCLASS_LE
		case ASTCLASS_CGT      : n->class = ASTCLASS_GT
		case ASTCLASS_CGE      : n->class = ASTCLASS_GE
		case else              : assert( FALSE )
		end select

		'' Turn -1|0 into 1|0 if the value may be used in math calculations etc.
		if( is_bool_context = FALSE ) then
			n = astNew( ASTCLASS_NEGATE, n )
		end if
	end select

	function = n
end function

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

private function astCountCallConv( byval n as ASTNODE ptr, byval callconv as integer ) as integer
	var count = 0

	if( n->class = ASTCLASS_PROC ) then
		if( n->attrib and callconv ) then
			if( (n->attrib and ASTATTRIB_FILTEROUT) = 0 ) then
				count += 1
			end if
		end if
	end if

	if( n->subtype ) then count += astCountCallConv( n->subtype, callconv )
	if( n->array   ) then count += astCountCallConv( n->array  , callconv )
	if( n->expr    ) then count += astCountCallConv( n->expr   , callconv )

	var i = n->head
	while( i )
		count += astCountCallConv( i, callconv )
		i = i->next
	wend

	function = count
end function

private function astFindMainCallConv( byval ast as ASTNODE ptr ) as integer
	var   cdeclcount = astCountCallConv( ast, ASTATTRIB_CDECL   )
	var stdcallcount = astCountCallConv( ast, ASTATTRIB_STDCALL )
	if( (cdeclcount = 0) and (stdcallcount = 0) ) then
		'' No procedures/function pointers at all, so no need for an
		'' Extern block to cover a calling convention
		function = -1
	elseif( stdcallcount > cdeclcount ) then
		'' Stdcall dominates
		function = ASTATTRIB_STDCALL
	else
		'' Cdecl dominates, or equal to amount of Stdcalls
		function = ASTATTRIB_CDECL
	end if
end function

'' Does the AST contain any declarations that need to be emitted with a
'' case-preserving ALIAS?
private function astHaveDeclsNeedingCaseAlias( byval n as ASTNODE ptr ) as integer
	if( (n->attrib and ASTATTRIB_FILTEROUT) = 0 ) then
		select case( n->class )
		case ASTCLASS_VAR
			return ((n->attrib and (ASTATTRIB_LOCAL or ASTATTRIB_STATIC)) = 0)
		case ASTCLASS_PROC
			return TRUE
		end select
	end if

	var i = n->head
	while( i )
		if( astHaveDeclsNeedingCaseAlias( i ) ) then
			return TRUE
		end if
		i = i->next
	wend
end function

private sub astHideCallConv( byval n as ASTNODE ptr, byval callconv as integer )
	if( n->class = ASTCLASS_PROC ) then
		if( n->attrib and callconv ) then
			n->attrib or= ASTATTRIB_HIDECALLCONV
		end if
	end if

	if( n->subtype ) then astHideCallConv( n->subtype, callconv )
	if( n->array   ) then astHideCallConv( n->array  , callconv )
	if( n->expr    ) then astHideCallConv( n->expr   , callconv )

	var i = n->head
	while( i )
		astHideCallConv( i, callconv )
		i = i->next
	wend
end sub

private sub astHideCaseAlias( byval n as ASTNODE ptr )
	'' The case-preserving ALIAS only needs to be hidden on toplevel
	'' declarations for which it would otherwise be emitted. No need to
	'' recurse into procptr subtypes, or expressions...
	n->attrib or= ASTATTRIB_HIDECASEALIAS

	var i = n->head
	while( i )
		astHideCaseAlias( i )
		i = i->next
	wend
end sub

private sub astWrapInExternBlock _
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

	'' Same for case-preserving ALIAS
	astHideCaseAlias( ast )

	assert( ast->class = ASTCLASS_GROUP )
	astPrepend( ast, astNew( ASTCLASS_EXTERNBLOCKBEGIN, externblock ) )
	astAppend( ast, astNew( ASTCLASS_EXTERNBLOCKEND ) )

end sub

sub astAutoExtern( byval ast as ASTNODE ptr, byval use_stdcallms as integer )
	var maincallconv = astFindMainCallConv( ast )
	if( (maincallconv >= 0) or astHaveDeclsNeedingCaseAlias( ast ) ) then
		astWrapInExternBlock( ast, maincallconv, use_stdcallms )
	end if
end sub

'' C array parameter? It's really just a pointer (the array is passed byref).
'' FB doesn't support C array parameters like that, so turn them into pointers:
''    int a[5]  ->  byval a as long ptr
sub hHandleArrayParam( byval n as ASTNODE ptr )
	assert( n->array )
	if( n->class = ASTCLASS_PARAM ) then
		astDelete( n->array )
		n->array = NULL
		n->dtype = typeAddrOf( n->dtype )
	end if
end sub

private sub hSolveOutArrayTypedefSubtypes _
	( _
		byval n as ASTNODE ptr, _
		byval typedef as ASTNODE ptr _
	)

	if( typeGetDtAndPtr( n->dtype ) = TYPE_UDT ) then
		if( n->subtype->class = ASTCLASS_ID ) then
			if( *n->subtype->text = *typedef->text ) then
				astSetType( n, typedef->dtype, typedef->subtype )
				if( n->array = NULL ) then
					n->array = astNew( ASTCLASS_ARRAY )
				end if
				astAppend( n->array, astCloneChildren( typedef->array ) )
				hHandleArrayParam( n )
				hHandlePlainCharAfterArrayStatusIsKnown( n )
			end if
		else
			hSolveOutArrayTypedefSubtypes( n->subtype, typedef )
		end if
	end if

	if( n->array ) then hSolveOutArrayTypedefSubtypes( n->array, typedef )
	if( n->expr  ) then hSolveOutArrayTypedefSubtypes( n->expr , typedef )

	var i = n->head
	while( i )
		hSolveOutArrayTypedefSubtypes( i, typedef )
		i = i->next
	wend

end sub

'' For each array typedef, remove it and update all uses
sub astSolveOutArrayTypedefs( byval n as ASTNODE ptr, byval ast as ASTNODE ptr )
	var i = n->head
	while( i )
		var nxt = i->next

		astSolveOutArrayTypedefs( i, ast )

		if( i->class = ASTCLASS_TYPEDEF ) then
			if( i->array ) then
				hSolveOutArrayTypedefSubtypes( ast, i )
				astRemove( n, i )
			end if
		end if

		i = nxt
	wend
end sub

private sub hExpandProcTypedef( byval n as ASTNODE ptr, byval typedef as ASTNODE ptr )
	'' variable/field/typedef declarations such as
	''    a *p1;
	'' become function pointers:
	''    int (*p1)(int);
	''
	'' This should preserve CONSTs:
	''    a * const p1;
	'' becomes:
	''    int (* const p1)(int);
	'' But as above, cv-qualifiers on the function type
	'' don't make sense and should be ignored, e.g.:
	''    a const * p1;

	assert( typeGetDtAndPtr( typedef->dtype ) = TYPE_PROC )
	astSetType( n, typeUnsetBaseConst( typeSetDt( n->dtype, TYPE_PROC ) ), typedef->subtype )
end sub

private function hSolveOutProcTypedefSubtype( byval n as ASTNODE ptr, byval typedef as ASTNODE ptr ) as integer
	'' Given a function typedef such as
	''    typedef int (a)(int);
	assert( typeGetDtAndPtr( typedef->dtype ) = TYPE_PROC )

	'' We want to update the given declaration which uses this typedef.
	assert( typeGetDt( n->dtype ) = TYPE_UDT )

	select case( n->class )
	case ASTCLASS_VAR, ASTCLASS_FIELD
		'' The var/field uses the typedef as data type, either plain,
		'' or as pointer to it. In the former case, the var/field itself
		'' becomes the function. In case it's just a pointer, it becomes
		'' a function pointer.

		if( typeGetPtrCount( n->dtype ) = 0 ) then
			var proc = typedef->subtype
			assert( proc->class = ASTCLASS_PROC )

			'' variable/field declarations such as
			''    a f1;
			'' must be turned into procedure declarations (solving out the typedef):
			''    int (f1)(int);
			''
			'' If there's a CONST on the function type, e.g.:
			''    typedef int (T)(int);
			''    T const f;
			'' it will be overwritten and lost, i.e. we ignore it.
			'' According to gcc -pedantic, "ISO C forbids qualified
			'' function types", and it seems like the C++ standard
			'' requires cv-qualifiers to be ignored in such cases.

			'' Function result type
			astSetType( n, proc->dtype, proc->subtype )

			'' Parameters
			assert( n->head = NULL )
			astAppend( n, astCloneChildren( proc ) )

			'' Attributes, e.g. calling convention
			n->attrib or= proc->attrib and (not ASTATTRIB_FILTEROUT)

			'' Turn into a procedure
			n->class = ASTCLASS_PROC
		else
			hExpandProcTypedef( n, typedef )
		end if

	'' It's also possible that the typedef is used as type in another
	'' typedef:
	''    typedef a b;
	'' In this case we want to expand 'a' in the 'b' typedef:
	''    typedef int (b)(int);
	'' and then later solve out 'b' in the same way everywhere it's used.
	case ASTCLASS_TYPEDEF
		hExpandProcTypedef( n, typedef )

	'' In other places (parameters, function results, casts, sizeof()) we
	'' can expand the function type (only) if the context is a pointer.
	'' It will then become a function pointer.
	''
	'' Having a plain function type as parameter or function result doesn't
	'' make sense though.
	case else
		if( typeGetPtrCount( n->dtype ) = 0 ) then
			exit function
		end if

		hExpandProcTypedef( n, typedef )
	end select

	function = TRUE
end function

private sub hSolveOutProcTypedefSubtypes _
	( _
		byval n as ASTNODE ptr, _
		byval typedef as ASTNODE ptr _
	)

	if( typeGetDt( n->dtype ) = TYPE_UDT ) then
		if( n->subtype->class = ASTCLASS_ID ) then
			if( *n->subtype->text = *typedef->text ) then
				if( hSolveOutProcTypedefSubtype( n, typedef ) = FALSE ) then
					oops( "can't solve out " + astDumpPrettyDecl( typedef ) + " in " + astDumpPrettyDecl( n ) )
				end if
			end if
		end if
	end if

	if( n->subtype ) then hSolveOutProcTypedefSubtypes( n->subtype, typedef )
	if( n->array ) then hSolveOutProcTypedefSubtypes( n->array, typedef )
	if( n->expr  ) then hSolveOutProcTypedefSubtypes( n->expr , typedef )

	var i = n->head
	while( i )
		hSolveOutProcTypedefSubtypes( i, typedef )
		i = i->next
	wend

end sub

sub astSolveOutProcTypedefs( byval n as ASTNODE ptr, byval ast as ASTNODE ptr )
	var i = n->head
	while( i )
		var nxt = i->next

		astSolveOutProcTypedefs( i, ast )

		if( i->class = ASTCLASS_TYPEDEF ) then
			if( typeGetDtAndPtr( i->dtype ) = TYPE_PROC ) then
				hSolveOutProcTypedefSubtypes( ast, i )
				astRemove( n, i )
			end if
		end if

		i = nxt
	wend
end sub

'' Handle declarations with plain char/zstring data type (not pointers).
'' If it's a char array, then it's probably supposed to be a string.
'' If it's just a char, then it's probably supposed to be a byte.
sub hHandlePlainCharAfterArrayStatusIsKnown( byval n as ASTNODE ptr )
	'' No plain char?
	if( typeGetDtAndPtr( n->dtype ) <> TYPE_ZSTRING ) then
		exit sub
	end if

	if( n->array ) then
		'' Use the last (inner-most) array dimension as the fixed-length string size
		var d = n->array->tail
		assert( d->class = ASTCLASS_DIMENSION )
		assert( d->expr )
		n->subtype = astClone( d->expr )
		astRemove( n->array, d )

		'' If no dimensions left, remove the array type entirely
		if( n->array->head = NULL ) then
			astDelete( n->array )
			n->array = NULL
		end if
	else
		'' Turn zstring into byte, but preserve CONSTs
		n->dtype = typeGetConst( n->dtype ) or TYPE_BYTE
	end if
end sub

''
'' Unscoping of nested declarations:
''
'' For example nested named structs or constants/#defines nested in structs need
'' to be moved to the toplevel, out of the parent struct, because FB does not
'' support this or would scope them inside the parent struct, unlike C.
''
''        struct UDT1 {
''            #define M1
''            struct UDT2 {
''                int a;
''            } a;
''            struct UDT3 {
''                int a;
''            };
''        };
''
'' becomes:
''
''        #define M1
''
''        struct UDT2 {
''            int a;
''        };
''
''        struct UDT3 {
''            int a;
''        };
''
''        struct UDT1 {
''            struct UDT2 a;
''        };
''
'' Since such nested structs aren't even scoped/namespaced in the parent in C,
'' we don't even have to worry about identifier conflicts.
''
function astUnscopeDeclsNestedInStructs( byval struct as ASTNODE ptr ) as ASTNODE ptr
	assert( (struct->class = ASTCLASS_STRUCT) or _
	        (struct->class = ASTCLASS_UNION) or _
	        (struct->class = ASTCLASS_ENUM) )
	var result = astNewGROUP( )

	var i = struct->head
	while( i )
		var nxt = i->next

		select case( i->class )
		case ASTCLASS_STRUCT, ASTCLASS_UNION
			if( i->text ) then
				astAppend( result, astUnscopeDeclsNestedInStructs( astClone( i ) ) )
				astRemove( struct, i )
			end if
		case ASTCLASS_PPDEFINE
			astAppend( result, astClone( i ) )
			astRemove( struct, i )
		end select

		i = nxt
	wend

	astAppend( result, struct )
	function = result
end function

''
'' Look for TYPEDEFs that have the given anon UDT as subtype. The first
'' TYPEDEF's id can become the anon UDT's id, and then that TYPEDEF can be
'' removed. All other TYPEDEFs need to be changed over from the old anon
'' subtype to the new id subtype.
''
'' For example:
''    typedef struct { ... } A, B, C;
'' is parsed into:
''    struct dummyid
''        ...
''    typedef A as dummyid
''    typedef B as dummyid
''    typedef C as dummyid
'' and should now be changed to:
''    struct A
''        ...
''    typedef B as A
''    typedef C as A
''
'' Not all cases can be solved out, for example:
''    typedef struct { ... } *A;
'' is parsed into:
''    struct dummyid
''        ...
''    typedef A as dummyid ptr
'' i.e. the typedef is a pointer to the anon struct, not an alias for it.
''
private sub hTryNameAnonUdtAfterFirstAliasTypedef _
	( _
		byval parent as ASTNODE ptr, _
		byval anon as ASTNODE ptr _
	)

	'' (Assuming that the parser will only insert typedefs using the anon id
	'' behind the anon UDT node, not in front of it...)

	'' 1. Find alias typedef
	dim as ASTNODE ptr aliastypedef
	var typedef = anon->next
	while( typedef )
		if( typedef->class <> ASTCLASS_TYPEDEF ) then
			exit while
		end if

		'' Must be a plain alias, can't be a pointer or non-UDT
		if( typeGetDtAndPtr( typedef->dtype ) = TYPE_UDT ) then
			if( typedef->subtype->class = ASTCLASS_TAGID ) then
				if( *typedef->subtype->text = *anon->text ) then
					aliastypedef = typedef
					exit while
				end if
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

		astReplaceSubtypes( typedef, ASTCLASS_TAGID, anon->text, ASTCLASS_ID, aliastypedef->text )

		typedef = typedef->next
	wend

	'' Rename the anon UDT to the alias typedef's id, now that its old
	'' dummyid isn't needed for comparison above anymore
	astSetText( anon, aliastypedef->text )
	assert( anon->attrib and ASTATTRIB_DUMMYID )
	anon->attrib and= not ASTATTRIB_DUMMYID

	astRemove( parent, aliastypedef )
end sub

sub astNameAnonUdtsAfterFirstAliasTypedef( byval n as ASTNODE ptr )
	var i = n->head
	while( i )

		'' Anon UDT?
		select case( i->class )
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			if( i->attrib and ASTATTRIB_DUMMYID ) then
				hTryNameAnonUdtAfterFirstAliasTypedef( n, i )
			end if
		end select

		i = i->next
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
'' Collect information about the declared/used state of struct/union/enum tag
'' ids, such that in the end we have a list of tag ids, and we can tell for
'' which ones a forward declaration must be emitted at the top:
''    a) it's used above the declaration
''    b) it's used without being declared at all
''
'' Declaration = struct/union/enum compound
'' Used = Used as data type somewhere
''
'' In such cases we need to add a typedef which will do the forward declaration,
'' because FB does not allow "implicit" forward references.
''
'' As a special case, if the first use is in a typedef, then we don't need to
'' add an extra typedef for it. The existing typedef will already declare the
'' forward reference.
''
'' We don't ever need to add forward declarations for "unused" tag ids - the
'' ones that only appear in declarations.
''

const STATE_DECLARED			= 1 shl 0
const STATE_USED_ABOVE_DECL		= 1 shl 1
const STATE_USED			= 1 shl 2
const STATE_USED_FIRST_IN_TYPEDEF	= 1 shl 3
const STATE_FILTEROUT			= 1 shl 4

private function hUsedButNotDeclared( byval state as integer ) as integer
	function = ((state and (STATE_DECLARED or STATE_USED)) = STATE_USED)
end function

'' Record a tag id into the list and hashtb, if it doesn't exist yet,
'' and update used/declared state
private sub hOnTagId _
	( _
		byval list as ASTNODE ptr, _
		byval hashtb as THASH ptr, _
		byval id as zstring ptr, _
		byval addstate as integer, _
		byval is_in_typedef as integer, _
		byval has_filterout as integer _
	)

	var hash = hashHash( id )
	var item = hashLookup( hashtb, id, hash )

	'' Already exists?
	if( item->s ) then
		var state = cint( item->data )

		'' Adding a declaration?
		if( addstate = STATE_DECLARED ) then
			'' Already used, but not declared yet?
			if( hUsedButNotDeclared( state ) ) then
				state or= STATE_USED_ABOVE_DECL
			end if
		end if

		state or= addstate

		item->data = cptr( any ptr, state )
	else
		'' New tag id.

		'' Recording a use (not a declaration) as first appearance?
		if( addstate = STATE_USED ) then
			if( is_in_typedef ) then
				addstate or= STATE_USED_FIRST_IN_TYPEDEF
			end if
		end if

		if( has_filterout ) then
			addstate or= STATE_FILTEROUT
		end if

		hashAdd( hashtb, item, hash, id, cptr( any ptr, addstate ) )

		astAppend( list, astNewID( id ) )
	end if

end sub

'' Walk through the code from top to bottom and record tag ids and corresponding
'' state into the list and hashtb.
private sub hCollectTagIds( byval n as ASTNODE ptr, byval list as ASTNODE ptr, byval hashtb as THASH ptr )
	select case( n->class )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		'' Not anonymous?
		if( n->text ) then
			hOnTagId( list, hashtb, n->text, STATE_DECLARED, FALSE, ((n->attrib and ASTATTRIB_FILTEROUT) <> 0) )
		end if
	end select

	if( n->subtype ) then
		if( n->subtype->class = ASTCLASS_TAGID ) then
			hOnTagId( list, hashtb, n->subtype->text, STATE_USED, (n->class = ASTCLASS_TYPEDEF), ((n->attrib and ASTATTRIB_FILTEROUT) <> 0) )
		else
			hCollectTagIds( n->subtype, list, hashtb )
		end if
	end if

	if( n->array ) then hCollectTagIds( n->array, list, hashtb )
	if( n->expr  ) then hCollectTagIds( n->expr , list, hashtb )

	var i = n->head
	while( i )
		hCollectTagIds( i, list, hashtb )
		i = i->next
	wend
end sub

private sub hRenameTagDecls _
	( _
		byval n as ASTNODE ptr, _
		byval oldid as zstring ptr, _
		byval newid as zstring ptr _
	)

	select case( n->class )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		if( n->text ) then
			if( *n->text = *oldid ) then
				astSetText( n, newid )
			end if
		end if
	end select

	var i = n->head
	while( i )
		hRenameTagDecls( i, oldid, newid )
		i = i->next
	wend

end sub

private function hAddForwardTypedef( byval decllist as ASTNODE ptr, byval tagid as zstring ptr, byval add_filterout as integer ) as string
	var forwardid = *tagid + "_"
	var typedef = astNew( ASTCLASS_TYPEDEF, tagid )
	astSetType( typedef, TYPE_UDT, astNewID( forwardid ) )
	if( add_filterout ) then
		astSetAttribOnAll( typedef, ASTATTRIB_FILTEROUT )
	end if
	astAppend( decllist, typedef )
	function = forwardid
end function

private sub hConsiderTagId _
	( _
		byval decllist as ASTNODE ptr, _
		byval ast as ASTNODE ptr, _
		byval tagid as zstring ptr, _
		byval state as integer _
	)

	'' Tag id used before its declaration?
	if( state and STATE_USED_ABOVE_DECL ) then
		'' First use is in a typedef (and declaration follows later)?
		if( state and STATE_USED_FIRST_IN_TYPEDEF ) then
			'' Nothing to do - the typedef already declares the forward reference.
			'' Since the declaration exists, astFixIds() will fix name conflicts
			'' based on that.
		else
			'' Rename the struct and add a forward declaration for it using the old id.
			'' astFixIds() will take care of fixing name conflicts involving the new
			'' struct id, if any.
			hRenameTagDecls( ast, tagid, hAddForwardTypedef( decllist, tagid, ((state and STATE_FILTEROUT) <> 0) ) )
		end if

	'' Tag id used only (and no declaration exists)?
	elseif( hUsedButNotDeclared( state ) ) then
		'' Add a forward typedef for this id and a dummy declaration
		'' for the forward id, so that astFixIds() will take care of fixing
		'' name conflicts involving the forward id, if any. The dummy
		'' declaration won't be emitted though.
		var forwardid = hAddForwardTypedef( decllist, tagid, ((state and STATE_FILTEROUT) <> 0) )

		var dummydecl = astNew( ASTCLASS_STRUCT, forwardid )
		dummydecl->attrib or= ASTATTRIB_FILTEROUT
		astAppend( decllist, dummydecl )
	end if

end sub

sub astAddForwardDeclsForUndeclaredTagIds( byval ast as ASTNODE ptr )
	dim hashtb as THASH
	hashInit( @hashtb, 4, FALSE )
	var tagidlist = astNewGROUP( )

	hCollectTagIds( ast, tagidlist, @hashtb )

	'' For each tag id: Create a forward declaration, if needed, and adjust
	'' the AST, if needed.
	var decllist = astNewGROUP( )
	var tagid = tagidlist->head
	while( tagid )

		var id = tagid->text
		var item = hashLookup( @hashtb, id, hashHash( id ) )
		assert( item->s )
		hConsiderTagId( decllist, ast, id, cint( item->data ) )

		tagid = tagid->next
	wend

	'' Insert the forward declarations at the top of the AST, in the same
	'' order they were found.
	astPrepend( ast, decllist )

	astDelete( tagidlist )
	hashEnd( @hashtb )
end sub

''
'' Remove all declarations marked with ASTATTRIB_FILTEROUT.
''
'' We should only ever filter out whole declarations, never remove parts of a
'' declaration (e.g. some fields from a struct). It's possible that some parts
'' came from an #included file which should be filtered out, but doing that
'' would break the code.
''
sub astFilterOut( byval code as ASTNODE ptr )
	var i = code->head
	while( i )
		var nxt = i->next
		if( i->attrib and ASTATTRIB_FILTEROUT ) then
			astRemove( code, i )
		end if
		i = nxt
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

''
'' C allows unnamed structs to be nested directly in other structs, and nested
'' unnamed unions directly in unions, but FB only allows unnamed structs nested
'' inside unions, and unnamed unions inside structs. Thus we need to insert the
'' missing union/struct in between in such cases:
''
''    struct
''        struct
''            field
''
'' must be converted to:
''
''    struct
''        union
''            struct
''                field
''
'' Nested unnamed structs can make a difference for field layout/alignment so it
'' shouldn't be removed. And unions of course shouldn't be removed either
'' because they clearly affect field layout.
''
sub astMakeNestedUnnamedStructsFbCompatible( byval n as ASTNODE ptr )
	var i = n->head
	while( i )
		var nxt = i->next

		astMakeNestedUnnamedStructsFbCompatible( i )

		select case( n->class )
		case ASTCLASS_STRUCT, ASTCLASS_UNION
			if( i->class = n->class ) then
				dim as integer newastclass
				if( i->class = ASTCLASS_STRUCT ) then
					newastclass = ASTCLASS_UNION
				else
					assert( i->class = ASTCLASS_UNION )
					newastclass = ASTCLASS_STRUCT
				end if
				var newstruct = astNew( newastclass, astClone( i ) )
				if( i->attrib and ASTATTRIB_FILTEROUT ) then
					newstruct->attrib or= ASTATTRIB_FILTEROUT
				end if
				nxt = astReplace( n, i, newstruct )
			end if
		end select

		i = nxt
	wend
end sub

'' Removes typedefs of the form "typedef struct T T;" which aren't needed in FB.
sub astRemoveRedundantTypedefs( byval n as ASTNODE ptr, byval ast as ASTNODE ptr )
	var i = n->head
	while( i )
		var nxt = i->next

		astRemoveRedundantTypedefs( i, ast )

		if( i->class = ASTCLASS_TYPEDEF ) then
			if( typeGetDtAndPtr( i->dtype ) = TYPE_UDT ) then
				if( i->subtype->class = ASTCLASS_TAGID ) then
					var typedef = *i->text
					var struct = *i->subtype->text
					if( typedef = struct ) then
						astReplaceSubtypes( ast, ASTCLASS_ID, typedef, ASTCLASS_TAGID, struct )
						astRemove( n, i )
					end if
				end if
			end if
		end if

		i = nxt
	wend
end sub

'' Checks whether an expression is simple enough that it could be used in an
'' FB constant declaration (const FOO = <...>).
''
'' Thus, it shouldn't contain any function calls, addrof/deref operations, or
'' preprocessor operations, etc., but only a selected set of known-to-be-safe
'' math operations & co.
private function hIsSimpleConstantExpression( byval n as ASTNODE ptr ) as integer
	if( n = NULL ) then return TRUE

	select case( n->class )
	'' Atoms
	case ASTCLASS_CONSTI, ASTCLASS_CONSTF

	'' UOPs
	case ASTCLASS_NOT, ASTCLASS_NEGATE, ASTCLASS_SIZEOF, ASTCLASS_CAST
		if( hIsSimpleConstantExpression( n->head ) = FALSE ) then exit function

	'' BOPs
	case ASTCLASS_ORELSE, ASTCLASS_ANDALSO, _
	     ASTCLASS_OR, ASTCLASS_XOR, ASTCLASS_AND, _
	     ASTCLASS_EQ, ASTCLASS_NE, _
	     ASTCLASS_LT, ASTCLASS_LE, _
	     ASTCLASS_GT, ASTCLASS_GE, _
	     ASTCLASS_SHL, ASTCLASS_SHR, _
	     ASTCLASS_ADD, ASTCLASS_SUB, _
	     ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD, _
	     ASTCLASS_STRCAT
		if( hIsSimpleConstantExpression( n->head ) = FALSE ) then exit function
		if( hIsSimpleConstantExpression( n->tail ) = FALSE ) then exit function

	'' IIF
	case ASTCLASS_IIF
		if( hIsSimpleConstantExpression( n->expr ) = FALSE ) then exit function
		if( hIsSimpleConstantExpression( n->head ) = FALSE ) then exit function
		if( hIsSimpleConstantExpression( n->tail ) = FALSE ) then exit function

	case else
		exit function
	end select

	function = TRUE
end function

'' Turns simple #defines into constants, where it seems possible.
sub astTurnDefinesIntoConstants( byval code as ASTNODE ptr )
	var i = code->head
	while( i )

		if( i->class = ASTCLASS_PPDEFINE ) then
			'' Object-like macro?
			if( i->paramcount < 0 ) then
				'' Has a body?
				if( i->expr ) then
					'' Body is a simple expression?
					if( hIsSimpleConstantExpression( i->expr ) ) then
						i->class = ASTCLASS_CONST
					end if
				end if
			end if
		end if

		i = i->next
	wend
end sub

private function hPreferRenaming( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_PPDEFINE, ASTCLASS_CONST
		function = TRUE
	end select
end function

'' If two symbols are conflicting, one of them must be renamed. Certain types
'' of symbols are preferably renamed. (e.g. renaming a constant is preferred
'' over renaming a procedure). If conflicting with an FB keyword, the symbol
'' must always be renamed though.
private function hDecideWhichSymbolToRename _
	( _
		byval first as ASTNODE ptr, _
		byval other as ASTNODE ptr _
	) as ASTNODE ptr

	'' Conflicting with FB keyword or id given via astFixIdsAddReservedId()?
	if( first = NULL ) then
		return other
	end if

	'' Parameters are the easiest to rename because renaming them doesn't
	'' make a difference (in prototypes)
	if( other->class = ASTCLASS_PARAM ) then return other
	if( first->class = ASTCLASS_PARAM ) then return first

	'' Prefer renaming #defines/constants over others
	if( hPreferRenaming( other ) ) then return other
	if( hPreferRenaming( first ) ) then return first

	'' Fallback to renaming the symbol that appeared later
	function = other
end function

private sub hCheckId _
	( _
		byval h as THASH ptr, _
		byval n as ASTNODE ptr, _
		byval add_here as integer _
	)

	assert( n->text )
	var id = ucase( *n->text )

	var hash = hashHash( id )
	var item = hashLookup( h, id, hash )
	if( item->s ) then
		dim as ASTNODE ptr first = item->data
		hDecideWhichSymbolToRename( first, n )->attrib or= ASTATTRIB_NEEDRENAME
	elseif( add_here ) then
		hashAdd( h, item, hash, id, n )
	end if

end sub

declare sub hFixIdsInScope _
	( _
		byval reservedids as THASH ptr, _
		byval code as ASTNODE ptr, _
		byval renamelist as ASTNODE ptr _
	)

private sub hWalkAndCheckIds _
	( _
		byval reservedids as THASH ptr, _
		byval types as THASH ptr, _
		byval globals as THASH ptr, _
		byval n as ASTNODE ptr, _
		byval renamelist as ASTNODE ptr _
	)

	var i = n->head
	while( i )

		if( i->subtype ) then
			if( i->subtype->class = ASTCLASS_PROC ) then
				'' Process procptr subtype parameters recursively (nested scope),
				'' with the #defines found so far.
				'' Don't build a renamelist here, because for parameters it's useless anyways.
				var nestedrenamelist = astNew( ASTCLASS_RENAMELIST )
				hFixIdsInScope( reservedids, i->subtype, nestedrenamelist )
				astDelete( nestedrenamelist )
			end if
		end if

		select case( i->class )
		case ASTCLASS_PPDEFINE
			hCheckId( @fbkeywordhash, i, FALSE )
			hCheckId( reservedids, i, TRUE )
			hCheckId( types, i, FALSE )
			hCheckId( globals, i, FALSE )

		case ASTCLASS_PROC
			hCheckId( @fbkeywordhash, i, FALSE )
			hCheckId( reservedids, i, FALSE )
			hCheckId( globals, i, TRUE )

			'' Process parameters recursively (nested scope),
			'' with the #defines found so far.
			'' Don't build a renamelist here, because for parameters it's useless anyways.
			var nestedrenamelist = astNew( ASTCLASS_RENAMELIST )
			hFixIdsInScope( reservedids, i, nestedrenamelist )
			astDelete( nestedrenamelist )

		case ASTCLASS_VAR, ASTCLASS_CONST
			hCheckId( @fbkeywordhash, i, FALSE )
			hCheckId( reservedids, i, FALSE )
			hCheckId( globals, i, TRUE )

		case ASTCLASS_FIELD
			hCheckId( reservedids, i, FALSE )
			hCheckId( globals, i, TRUE )

			'' Fields can be named after FB keywords, but not '_'
			'' Since we're not checking the keyword hash table here,
			' we must check for '_' manually.
			if( i->text[0] = asc( "_" ) ) then
				if( i->text[1] = 0 ) then
					hDecideWhichSymbolToRename( NULL, i )->attrib or= ASTATTRIB_NEEDRENAME
				end if
			end if

		case ASTCLASS_PARAM
			'' Don't check anonymous params
			if( i->text ) then
				hCheckId( @fbkeywordhash, i, FALSE )
				hCheckId( reservedids, i, FALSE )
				hCheckId( globals, i, TRUE )
			end if

		case ASTCLASS_STRUCT, ASTCLASS_UNION
			'' Not anonymous?
			if( i->text ) then
				hCheckId( @fbkeywordhash, i, FALSE )
				hCheckId( reservedids, i, FALSE )
				hCheckId( types, i, TRUE )

				'' Process fields recursively (nested scope),
				'' with the #defines found so far.
				var nestedrenamelist = astNew( ASTCLASS_RENAMELIST, "inside " + astDumpPrettyDecl( i ) + ":" )
				hFixIdsInScope( reservedids, i, nestedrenamelist )
				if( nestedrenamelist->head ) then
					astAppend( renamelist, nestedrenamelist )
				else
					astDelete( nestedrenamelist )
				end if
			else
				'' Anonymous struct/union: Process the fields
				'' recursively, they belong to this scope too.
				hWalkAndCheckIds( reservedids, types, globals, i, renamelist )
			end if

		case ASTCLASS_TYPEDEF
			hCheckId( @fbkeywordhash, i, FALSE )
			hCheckId( reservedids, i, FALSE )
			hCheckId( types, i, TRUE )

		case ASTCLASS_ENUM
			'' Check enum's id (unless it's an anonymous enum): It
			'' belongs to the type namespace.
			if( i->text ) then
				hCheckId( @fbkeywordhash, i, FALSE )
				hCheckId( reservedids, i, FALSE )
				hCheckId( types, i, TRUE )
			end if

			'' Check enum's constants: They belong to this scope
			'' too, regardless of whether the enum is named or
			'' anonymous.
			hWalkAndCheckIds( reservedids, types, globals, i, renamelist )

		end select

		i = i->next
	wend
end sub

private sub hReplaceCalls _
	( _
		byval n as ASTNODE ptr, _
		byval oldid as zstring ptr, _
		byval newid as zstring ptr _
	)

	if( n = NULL ) then exit sub

	select case( n->class )
	case ASTCLASS_CALL, ASTCLASS_ID
		if( *n->text = *oldid ) then
			astSetText( n, newid )
		end if
	end select

	if( n->subtype ) then hReplaceCalls( n->subtype, oldid, newid )
	if( n->array   ) then hReplaceCalls( n->array  , oldid, newid )
	if( n->expr    ) then hReplaceCalls( n->expr   , oldid, newid )

	var i = n->head
	while( i )
		hReplaceCalls( i, oldid, newid )
		i = i->next
	wend

end sub

sub astReplaceSubtypes _
	( _
		byval n as ASTNODE ptr, _
		byval oldclass as integer, _
		byval oldid as zstring ptr, _
		byval newclass as integer, _
		byval newid as zstring ptr _
	)

	if( n->subtype ) then
		if( n->subtype->class = oldclass ) then
			if( *n->subtype->text = *oldid ) then
				astSetText( n->subtype, newid )
				n->subtype->class = newclass
			end if
		else
			astReplaceSubtypes( n->subtype, oldclass, oldid, newclass, newid )
		end if
	end if

	if( n->array ) then astReplaceSubtypes( n->array, oldclass, oldid, newclass, newid )
	if( n->expr  ) then astReplaceSubtypes( n->expr , oldclass, oldid, newclass, newid )

	var i = n->head
	while( i )
		astReplaceSubtypes( i, oldclass, oldid, newclass, newid )
		i = i->next
	wend

end sub

private sub hRenameSymbol _
	( _
		byval reservedids as THASH ptr, _
		byval types as THASH ptr, _
		byval globals as THASH ptr, _
		byval n as ASTNODE ptr, _
		byval code as ASTNODE ptr, _
		byval renamelist as ASTNODE ptr _
	)

	dim as string oldid, newid, hashid

	'' Build a new name by appending _ underscores, as long as needed to
	'' find an identifier that's not yet used in the namespace corresponding
	'' to the symbol. (otherwise renaming could cause more conflicts)
	assert( n->text )
	oldid = *n->text
	newid = oldid

	dim as integer exists
	do
		newid += "_"

		hashid = ucase( newid )
		var hash = hashHash( hashid )

		exists = hashContains( @fbkeywordhash, hashid, hash )

		select case( n->class )
		case ASTCLASS_PPDEFINE
			exists or= hashContains( reservedids, hashid, hash )
			exists or= hashContains( types      , hashid, hash )
			exists or= hashContains( globals    , hashid, hash )

		case ASTCLASS_PROC, ASTCLASS_PARAM, _
		     ASTCLASS_VAR, ASTCLASS_CONST, ASTCLASS_FIELD
			exists or= hashContains( reservedids, hashid, hash )
			exists or= hashContains( globals    , hashid, hash )

		case ASTCLASS_STRUCT, ASTCLASS_UNION, _
		     ASTCLASS_TYPEDEF, ASTCLASS_ENUM
			exists or= hashContains( reservedids, hashid, hash )
			exists or= hashContains( types      , hashid, hash )

		case else
			assert( FALSE )
		end select
	loop while( exists )

	'' Adjust all nodes using the original symbol name to use the new one
	'' Renaming a procedure? Should update all CALL expressions.
	'' Renaming a type? Should update all references to it.
	'' (chances are this is the right thing to do, because headers won't
	'' usually contain duplicates amongst types/globals internally; it's
	'' just the case-insensitivity and FB keywords that cause problems)
	select case( n->class )
	case ASTCLASS_PPDEFINE, ASTCLASS_PROC, ASTCLASS_PARAM, _
	     ASTCLASS_VAR, ASTCLASS_CONST, ASTCLASS_FIELD
		hReplaceCalls( code, n->text, newid )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		astReplaceSubtypes( code, ASTCLASS_TAGID, n->text, ASTCLASS_TAGID, newid )
	case ASTCLASS_TYPEDEF
		astReplaceSubtypes( code, ASTCLASS_ID, n->text, ASTCLASS_ID, newid )
	case else
		assert( FALSE )
	end select

	'' Update the symbol and add it to the hash table
	astRenameSymbol( n, newid )
	select case( n->class )
	case ASTCLASS_PPDEFINE
		hashAddOverwrite( reservedids, hashid, n )
	case ASTCLASS_PROC, ASTCLASS_PARAM, _
	     ASTCLASS_VAR, ASTCLASS_CONST, ASTCLASS_FIELD
		hashAddOverwrite( globals, hashid, n )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, ASTCLASS_TYPEDEF
		hashAddOverwrite( types, hashid, n )
	case else
		assert( FALSE )
	end select

	'' Don't produce renamelist entries for declarations that are being
	'' filtered out
	if( n->attrib and ASTATTRIB_FILTEROUT ) then
		exit sub
	end if

	'' Build the textual entry for the name list, for example:
	''    define FOO => FOO_
	'' making it clear which symbol was renamed, and how.
	var text = astDumpPrettyClass( n->class ) + " " + oldid
	text += " => " + newid
	astAppend( renamelist, astNewTEXT( text ) )

end sub

private function hIsScopelessBlock( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_ENUM
		function = TRUE
	case ASTCLASS_STRUCT, ASTCLASS_UNION
		function = (n->text = NULL)
	end select
end function

private sub hWalkAndRenameSymbols _
	( _
		byval reservedids as THASH ptr, _
		byval types as THASH ptr, _
		byval globals as THASH ptr, _
		byval n as ASTNODE ptr, _
		byval code as ASTNODE ptr, _
		byval renamelist as ASTNODE ptr _
	)

	var i = n->head
	while( i )

		if( i->attrib and ASTATTRIB_NEEDRENAME ) then
			hRenameSymbol( reservedids, types, globals, i, code, renamelist )
		end if

		''
		'' Recursively process nested symbols that still belong to this
		'' scope though. (happens with enum consts and fields of
		'' anonymous structs/unions, see also hWalkAndCheckIds())
		''
		'' Fields of named structs/unions and parameters should be
		'' ignored here though: They're treated as separate scopes and
		'' are supposed to be handled by their hFixIdsInScope() calls.
		''
		if( hIsScopelessBlock( i ) ) then
			hWalkAndRenameSymbols( reservedids, types, globals, i, code, renamelist )
		end if

		i = i->next
	wend

end sub

private sub hFixIdsInScope _
	( _
		byval reservedids as THASH ptr, _
		byval code as ASTNODE ptr, _
		byval renamelist as ASTNODE ptr _
	)

	'' Scope-specific namespaces for types and vars/procs/consts/fields/params
	dim as THASH types, globals
	hashInit( @types, 2, TRUE )
	hashInit( @globals, 2, TRUE )

	'' 1. Walk through symbols top-down, much like a C compiler, and mark
	''    those that need renaming with ASTATTRIB_NEEDRENAME.
	hWalkAndCheckIds( reservedids, @types, @globals, code, renamelist )

	'' 2. Rename all marked symbols. Now that all symbols are known, we can
	''    generate new names for the symbols that need renaming without
	''    introducing more conflicts.
	hWalkAndRenameSymbols( reservedids, @types, @globals, code, code, renamelist )

	hashEnd( @globals )
	hashEnd( @types )

end sub

'' Namespace for the "globally" reserved identifiers, i.e. #defines and anything
'' given via the -reservedid command line option.
'' This doesn't include FB keywords, or global procedures/variables, because
'' those are not globally reserved identifiers: for example, they can be reused
'' by fields.
dim shared reservedids as THASH

sub astFixIdsInit( )
	hashInit( @reservedids, 2, TRUE )
end sub

sub astFixIdsAddReservedId( byval id as zstring ptr )
	'' Ucase'ing like hCheckId() to make the check case-insensitive
	hashAddOverwrite( @reservedids, ucase( *id, 1 ), NULL )
end sub

sub astFixIds( byval code as ASTNODE ptr )
	var renamelist = astNew( ASTCLASS_RENAMELIST, "The following symbols have been renamed:" )
	hFixIdsInScope( @reservedids, code, renamelist )
	if( renamelist->head ) then
		astPrepend( code, renamelist )
	else
		astDelete( renamelist )
	end if
	hashEnd( @reservedids )
end sub

function astUsesDtype( byval n as ASTNODE ptr, byval dtype as integer ) as integer
	if( typeGetDt( n->dtype ) = dtype ) then return TRUE

	if( n->subtype ) then if( astUsesDtype( n->subtype, dtype ) ) then return TRUE
	if( n->array   ) then if( astUsesDtype( n->array  , dtype ) ) then return TRUE
	if( n->expr    ) then if( astUsesDtype( n->expr   , dtype ) ) then return TRUE

	var i = n->head
	while( i )
		if( astUsesDtype( i, dtype ) ) then return TRUE
		i = i->next
	wend

	function = FALSE
end function

sub astMergeDIVIDERs( byval n as ASTNODE ptr )
	assert( n->class = ASTCLASS_GROUP )

	var child = n->head
	while( child )
		var nxt = child->next

		if( nxt ) then
			if( (child->class = ASTCLASS_DIVIDER) and _
			    (  nxt->class = ASTCLASS_DIVIDER) ) then
				astRemove( n, child )
			end if
		end if

		child = nxt
	wend
end sub

private function hIsPpIfBegin( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_PPIF, ASTCLASS_PPELSEIF, ASTCLASS_PPELSE
		function = TRUE
	end select
end function

private function hIsPpIfEnd( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_PPELSEIF, ASTCLASS_PPELSE, ASTCLASS_PPENDIF
		function = TRUE
	end select
end function

private function hIsCompound( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, ASTCLASS_UNKNOWN
		function = TRUE
	case ASTCLASS_PROC
		'' Procedure with body?
		function = (n->expr <> NULL)
	end select
end function

private function hShouldSeparate _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr _
	) as integer

	if( (a->class = ASTCLASS_DIVIDER) or _
	    (b->class = ASTCLASS_DIVIDER) ) then
		exit function
	end if

	if( hIsPpIfBegin( a ) and hIsPpIfEnd( b ) ) then
		exit function
	end if

	function = (a->class <> b->class) or _
	           hIsCompound( a ) or hIsCompound( b )
end function

''
'' Insert DIVIDERs between statements of different kind, e.g. all #defines in
'' a row shouldn't be divided, but a #define should be divided from a typedef.
'' Structs/unions/enums should always be separated by a divider because they're
'' compounds and normally span multiple lines themselves.
''
sub astAutoAddDividers( byval code as ASTNODE ptr )
	var i = code->head
	while( i )
		var nxt = i->next

		if( i->class <> ASTCLASS_RENAMELIST ) then
			astAutoAddDividers( i )
		end if

		if( nxt ) then
			if( hShouldSeparate( i, nxt ) ) then
				astInsert( code, astNew( ASTCLASS_DIVIDER ), nxt )
			end if
		end if

		i = nxt
	wend
end sub

sub astPrependMaybeWithDivider( byval group as ASTNODE ptr, byval n as ASTNODE ptr )
	if( group->head andalso hShouldSeparate( n, group->head ) ) then
		astPrepend( group, astNew( ASTCLASS_DIVIDER ) )
	end if
	astPrepend( group, n )
end sub

function astCountDecls( byval code as ASTNODE ptr ) as integer
	var count = 0

	var i = code->head
	while( i )

		select case( i->class )
		case ASTCLASS_DIVIDER, ASTCLASS_PPINCLUDE, ASTCLASS_PPENDIF, _
		     ASTCLASS_EXTERNBLOCKBEGIN, ASTCLASS_EXTERNBLOCKEND, _
		     ASTCLASS_INCLIB, ASTCLASS_PRAGMAONCE, _
		     ASTCLASS_UNKNOWN, ASTCLASS_RENAMELIST

		case ASTCLASS_PPIF, ASTCLASS_PPELSEIF, ASTCLASS_PPELSE, _
		     ASTCLASS_GROUP
			count += astCountDecls( i )

		case else
			count += 1
		end select

		i = i->next
	wend

	function = count
end function

function astCountUnknowns( byval code as ASTNODE ptr ) as integer
	var count = 0
	var i = code->head
	while( i )
		select case( i->class )
		case ASTCLASS_UNKNOWN
			count += 1
		case ASTCLASS_PPIF, ASTCLASS_PPELSEIF, ASTCLASS_PPELSE, _
		     ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, _
		     ASTCLASS_GROUP
			count += astCountUnknowns( i )
		case ASTCLASS_PROC
			if( i->expr ) then
				count += astCountUnknowns( i->expr )
			end if
		end select
		i = i->next
	wend
	function = count
end function

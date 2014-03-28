'' Higher level code transformations

#include once "fbfrog.bi"

private function hIsFoldableExpr( byval n as ASTNODE ptr ) as integer
	function = ((n->class >= ASTCLASS_CLOGOR) and _
	            (n->class <= ASTCLASS_IIF)) or _
	           (n->class = ASTCLASS_DIMENSION)
end function

private function hCleanExpr _
	( _
		byval n as ASTNODE ptr, _
		byval is_bool_context as integer _
	) as ASTNODE ptr

	if( n ) then
		if( hIsFoldableExpr( n ) ) then
			function = astFold( astOpsC2FB( n ), is_bool_context )
		else
			astCleanUpExpressions( n )
			function = n
		end if
	end if

end function

'' C expressions should be converted to FB, and folded (this generates pretty
'' FB expressions while still preserving the values/behaviour of the original
'' C expression)
sub astCleanUpExpressions( byval n as ASTNODE ptr )
	var is_bool_context = (n->class = ASTCLASS_PPDEFINE)  '' bold assumption

	n->expr = hCleanExpr( n->expr, is_bool_context )
	n->array = hCleanExpr( n->array, FALSE )
	n->subtype = hCleanExpr( n->subtype, FALSE )
	n->bits = hCleanExpr( n->bits, FALSE )

	'' Node that uses the children list to hold expressions?
	select case( n->class )
	case ASTCLASS_SCOPEBLOCK, ASTCLASS_ARRAY, _
	     ASTCLASS_PPMERGE, ASTCLASS_CALL, ASTCLASS_STRUCTINIT, ASTCLASS_ARRAYINIT
		var i = n->head
		while( i )
			i = astReplace( n, i, hCleanExpr( astClone( i ), FALSE ) )
		wend
	case else
		var i = n->head
		while( i )
			astCleanUpExpressions( i )
			i = i->next
		wend
	end select
end sub

'' For arrays with struct initializer, turn that into an array initializer
'' (in such cases the initializer parser couldn't do the disambiguation)
sub astTurnStructInitIntoArrayInit( byval n as ASTNODE ptr )
	var i = n->head
	while( i )

		select case( i->class )
		case ASTCLASS_VAR, ASTCLASS_STATICVAR, ASTCLASS_EXTERNVAR
			if( (i->expr <> NULL) and (i->array <> NULL) ) then
				if( i->expr->class = ASTCLASS_STRUCTINIT ) then
					i->expr->class = ASTCLASS_ARRAYINIT
				end if
			end if
		end select

		i = i->next
	wend
end sub

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

'' This isn't done by the C parser already because it has to handle calling
'' convention attributes amongst others, and using a default would require it
'' to remove the default if it will add another one, etc. Doing it here
'' afterwards just is easier because all that's needed is to fill the gaps.
sub astMakeProcsDefaultToCdecl( byval n as ASTNODE ptr )
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
	select case( n->class )
	case ASTCLASS_VAR, ASTCLASS_EXTERNVAR
		return TRUE
	case ASTCLASS_PROC
		return TRUE
	end select

	var child = n->head
	while( child )
		if( astHaveDeclsNeedingCaseAlias( child ) ) then
			return TRUE
		end if
		child = child->next
	wend
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

private sub astHideCaseAlias( byval n as ASTNODE ptr )
	'' The case-preserving ALIAS only needs to be hidden on toplevel
	'' declarations for which it would otherwise be emitted. No need to
	'' recurse into procptr subtypes, or expressions...
	n->attrib or= ASTATTRIB_HIDECASEALIAS

	var child = n->head
	while( child )
		astHideCaseAlias( child )
		child = child->next
	wend
end sub

private sub astWrapInExternBlock _
	( _
		byval ast as ASTNODE ptr, _
		byval externcallconv as integer, _
		byval use_stdcallms as integer, _
		byval whitespace as integer _
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
	if( whitespace ) then
		astPrepend( ast, astNew( ASTCLASS_DIVIDER ) )
	end if
	astPrepend( ast, astNew( ASTCLASS_EXTERNBLOCKBEGIN, externblock ) )
	if( whitespace ) then
		astAppend( ast, astNew( ASTCLASS_DIVIDER ) )
	end if
	astAppend( ast, astNew( ASTCLASS_EXTERNBLOCKEND ) )

end sub

sub astAutoExtern _
	( _
		byval ast as ASTNODE ptr, _
		byval use_stdcallms as integer, _
		byval whitespace as integer _
	)

	var maincallconv = astFindMainCallConv( ast )
	if( (maincallconv >= 0) or astHaveDeclsNeedingCaseAlias( ast ) ) then
		astWrapInExternBlock( ast, maincallconv, use_stdcallms, whitespace )
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
				astCloneAppendChildren( n->array, typedef->array )
			end if
		else
			hSolveOutArrayTypedefSubtypes( n->subtype, typedef )
		end if
	end if

	if( n->array ) then hSolveOutArrayTypedefSubtypes( n->array, typedef )
	if( n->expr  ) then hSolveOutArrayTypedefSubtypes( n->expr , typedef )
	if( n->l     ) then hSolveOutArrayTypedefSubtypes( n->l    , typedef )
	if( n->r     ) then hSolveOutArrayTypedefSubtypes( n->r    , typedef )

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

private sub hSolveOutProcTypedefSubtypes _
	( _
		byval n as ASTNODE ptr, _
		byval typedef as ASTNODE ptr _
	)

	if( typeGetDtAndPtr( n->dtype ) = TYPE_UDT ) then
		if( n->subtype->class = ASTCLASS_ID ) then
			if( *n->subtype->text = *typedef->text ) then
				select case( n->class )
				case ASTCLASS_VAR, ASTCLASS_STATICVAR, ASTCLASS_EXTERNVAR, ASTCLASS_FIELD
					assert( typeGetDtAndPtr( typedef->dtype ) = TYPE_PROC )
					var proc = typedef->subtype
					assert( proc->class = ASTCLASS_PROC )

					'' Copy function result type
					astSetType( n, proc->dtype, proc->subtype )

					'' Copy over the parameters
					assert( n->head = NULL )
					astCloneAppendChildren( n, proc )

					'' Copy attributes (especially calling convention)
					n->attrib or= proc->attrib

					'' Turn into a procedure
					n->class = ASTCLASS_PROC
				case else
					oops( "can't solve out " + astDumpPrettyDecl( typedef ) + " in " + astDumpPrettyDecl( n ) )
				end select
			end if
		else
			hSolveOutProcTypedefSubtypes( n->subtype, typedef )
		end if
	end if

	if( n->array ) then hSolveOutProcTypedefSubtypes( n->array, typedef )
	if( n->expr  ) then hSolveOutProcTypedefSubtypes( n->expr , typedef )
	if( n->l     ) then hSolveOutProcTypedefSubtypes( n->l    , typedef )
	if( n->r     ) then hSolveOutProcTypedefSubtypes( n->r    , typedef )

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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
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

private function hExtractNestedStructsForUnscoping( byval struct as ASTNODE ptr ) as ASTNODE ptr
	var result = astNewGROUP( )

	var i = struct->head
	while( i )
		var nxt = i->next

		select case( i->class )
		case ASTCLASS_STRUCT, ASTCLASS_UNION
			if( i->text ) then
				astCloneAppend( result, i )
				astRemove( struct, i )
			end if
		case ASTCLASS_PPDEFINE
			astCloneAppend( result, i )
			astRemove( struct, i )
		end select

		i = nxt
	wend

	function = result
end function

sub astUnscopeDeclsNestedInStructs( byval n as ASTNODE ptr )
	var struct = n->head
	while( struct )

		'' Recursively
		astUnscopeDeclsNestedInStructs( struct )

		select case( struct->class )
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			astInsert( n, hExtractNestedStructsForUnscoping( struct ), struct )
		end select

		struct = struct->next
	wend
end sub

''
'' Look for TYPEDEFs that have the given anon UDT as subtype. The first
'' TYPEDEF's id can become the anon UDT's id, and then that TYPEDEF can be
'' removed. All other TYPEDEFs need to be changed over from the old anon
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
	'' __fbfrog_anon* isn't need for comparison above anymore
	astSetText( anon, aliastypedef->text )

	astRemove( parent, aliastypedef )
end sub

sub astNameAnonUdtsAfterFirstAliasTypedef( byval n as ASTNODE ptr )
	var i = n->head
	while( i )

		'' Anon UDT?
		select case( i->class )
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			if( strStartsWith( *i->text, DUMMYID_PREFIX ) ) then
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

const STATE_DECLARED		= 1 shl 0
const STATE_USED_ABOVE_DECL	= 1 shl 1
const STATE_USED		= 1 shl 2

private function hUsedButNotDeclared( byval state as integer ) as integer
	function = ((state and (STATE_DECLARED or STATE_USED)) = STATE_USED)
end function

private sub hOnTagId _
	( _
		byval hashtb as THASH ptr, _
		byval id as zstring ptr, _
		byval addstate as integer _
	)

	var hash = hashHash( id )
	var item = hashLookup( hashtb, id, hash )
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
		hashAdd( hashtb, item, hash, id, cptr( any ptr, addstate ) )
	end if
end sub

private sub hCollectTagIds( byval n as ASTNODE ptr, byval hashtb as THASH ptr )
	select case( n->class )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		if( n->text ) then
			hOnTagId( hashtb, n->text, STATE_DECLARED )
		end if
	end select

	if( n->subtype ) then
		if( n->subtype->class = ASTCLASS_TAGID ) then
			hOnTagId( hashtb, n->subtype->text, STATE_USED )
		else
			hCollectTagIds( n->subtype, hashtb )
		end if
	end if

	if( n->array ) then hCollectTagIds( n->array, hashtb )
	if( n->expr  ) then hCollectTagIds( n->expr , hashtb )
	if( n->l     ) then hCollectTagIds( n->l    , hashtb )
	if( n->r     ) then hCollectTagIds( n->r    , hashtb )

	var i = n->head
	while( i )
		hCollectTagIds( i, hashtb )
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

sub astAddForwardDeclsForUndeclaredTagIds( byval ast as ASTNODE ptr )
	dim hashtb as THASH
	hashInit( @hashtb, 4, FALSE )

	hCollectTagIds( ast, @hashtb )

	'' For each undeclared tag id...
	for i as integer = 0 to hashtb.room-1
		var item = hashtb.items + i
		if( item->s ) then
			'' No declaration seen by hCollectTagIds() for this one?
			var state = cint( item->data )
			if( hUsedButNotDeclared( state ) ) then
				'' Add a forward typedef for this id and a dummy declaration
				'' for the forward id, so that astFixIds() will take care of fixing
				'' name conflicts involving the forward id, if any. The dummy
				'' declaration won't be emitted though.
				var forwardid = *item->s + "_"

				var typedef = astNew( ASTCLASS_TYPEDEF, item->s )
				astSetType( typedef, TYPE_UDT, astNewID( forwardid ) )
				astPrepend( ast, typedef )

				var dummydecl = astNew( ASTCLASS_STRUCT, forwardid )
				dummydecl->attrib or= ASTATTRIB_DONTEMIT
				astPrepend( ast, dummydecl )

			'' or it was found below the first use?
			elseif( state and STATE_USED_ABOVE_DECL ) then
				'' Rename the struct and add a forward declaration for it using the
				'' old id. astFixIds() will take care of fixing name conflicts involving
				'' the new struct id, if any.
				var forwardid = *item->s + "_"

				var typedef = astNew( ASTCLASS_TYPEDEF, item->s )
				astSetType( typedef, TYPE_UDT, astNewID( forwardid ) )
				astPrepend( ast, typedef )

				hRenameTagDecls( ast, item->s, forwardid )
			end if
		end if
	next

	hashEnd( @hashtb )
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
				nxt = astReplace( n, i, astNew( newastclass, astClone( i ) ) )
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
		if( hIsSimpleConstantExpression( n->l ) = FALSE ) then exit function

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
		if( hIsSimpleConstantExpression( n->l ) = FALSE ) then exit function
		if( hIsSimpleConstantExpression( n->r ) = FALSE ) then exit function

	'' IIF
	case ASTCLASS_IIF
		if( hIsSimpleConstantExpression( n->expr ) = FALSE ) then exit function
		if( hIsSimpleConstantExpression( n->l ) = FALSE ) then exit function
		if( hIsSimpleConstantExpression( n->r ) = FALSE ) then exit function

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
						i->class = ASTCLASS_CONSTANT
					end if
				end if
			end if
		end if

		i = i->next
	wend
end sub

private function hPreferRenaming( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_PPDEFINE, ASTCLASS_CONSTANT, ASTCLASS_ENUMCONST
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

	'' Conflicting with FB keyword?
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

#if 0
private function hIsTypedefOrStruct( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, ASTCLASS_TYPEDEF
		function = TRUE
	end select
end function
#endif

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

		if( frog.verbose ) then
			print "name conflict: " + astDumpPrettyDecl( n ) + ", " + _
				iif( first, astDumpPrettyDecl( first ), "FB keyword" )
		end if

#if 0
		if( first ) then
			if( hIsTypedefOrStruct( first ) and hIsTypedefOrStruct( n ) ) then
				'' Exact same id, even same capitalization?
				if( *first->text = *n->text ) then
					print "error: Suspicious name conflict between " + _
						astDumpPrettyDecl( first ) + " and " + _
						astDumpPrettyDecl( n )
					print "Please use -renametypedef or -renametag to fix this."
					end 1
				end if
			end if
		end if
#endif

		hDecideWhichSymbolToRename( first, n )->attrib or= ASTATTRIB_NEEDRENAME
	elseif( add_here ) then
		hashAdd( h, item, hash, id, n )
	end if

end sub

declare sub hFixIdsInScope _
	( _
		byval defines as THASH ptr, _
		byval code as ASTNODE ptr _
	)

private sub hWalkAndCheckIds _
	( _
		byval defines as THASH ptr, _
		byval types as THASH ptr, _
		byval globals as THASH ptr, _
		byval n as ASTNODE ptr _
	)

	var i = n->head
	while( i )

		if( i->subtype ) then
			if( i->subtype->class = ASTCLASS_PROC ) then
				'' Process procptr subtype parameters recursively (nested scope),
				'' with the #defines found so far.
				hFixIdsInScope( defines, i->subtype )
			end if
		end if

		select case( i->class )
		case ASTCLASS_PPDEFINE
			hCheckId( @fbkeywordhash, i, FALSE )
			hCheckId( defines, i, TRUE )
			hCheckId( types, i, FALSE )
			hCheckId( globals, i, FALSE )

		case ASTCLASS_PROC
			hCheckId( @fbkeywordhash, i, FALSE )
			hCheckId( defines, i, FALSE )
			hCheckId( globals, i, TRUE )

			'' Process parameters recursively (nested scope),
			'' with the #defines found so far.
			hFixIdsInScope( defines, i )

		case ASTCLASS_VAR, ASTCLASS_EXTERNVAR, ASTCLASS_STATICVAR, _
		     ASTCLASS_CONSTANT, ASTCLASS_ENUMCONST, ASTCLASS_FIELD
			hCheckId( @fbkeywordhash, i, FALSE )
			hCheckId( defines, i, FALSE )
			hCheckId( globals, i, TRUE )

		case ASTCLASS_PARAM
			'' Don't check anonymous params
			if( i->text ) then
				hCheckId( @fbkeywordhash, i, FALSE )
				hCheckId( defines, i, FALSE )
				hCheckId( globals, i, TRUE )
			end if

		case ASTCLASS_STRUCT, ASTCLASS_UNION
			'' Not anonymous?
			if( i->text ) then
				hCheckId( @fbkeywordhash, i, FALSE )
				hCheckId( defines, i, FALSE )
				hCheckId( types, i, TRUE )

				'' Process fields recursively (nested scope),
				'' with the #defines found so far.
				hFixIdsInScope( defines, i )
			else
				'' Anonymous struct/union: Process the fields
				'' recursively, they belong to this scope too.
				hWalkAndCheckIds( defines, types, globals, i )
			end if

		case ASTCLASS_TYPEDEF
			hCheckId( @fbkeywordhash, i, FALSE )
			hCheckId( defines, i, FALSE )
			hCheckId( types, i, TRUE )

		case ASTCLASS_ENUM
			'' Check enum's id (unless it's an anonymous enum): It
			'' belongs to the type namespace.
			if( i->text ) then
				hCheckId( @fbkeywordhash, i, FALSE )
				hCheckId( defines, i, FALSE )
				hCheckId( types, i, TRUE )
			end if

			'' Check enum's constants: They belong to this scope
			'' too, regardless of whether the enum is named or
			'' anonymous.
			hWalkAndCheckIds( defines, types, globals, i )

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
	if( n->l       ) then hReplaceCalls( n->l      , oldid, newid )
	if( n->r       ) then hReplaceCalls( n->r      , oldid, newid )

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
	if( n->l     ) then astReplaceSubtypes( n->l    , oldclass, oldid, newclass, newid )
	if( n->r     ) then astReplaceSubtypes( n->r    , oldclass, oldid, newclass, newid )

	var i = n->head
	while( i )
		astReplaceSubtypes( i, oldclass, oldid, newclass, newid )
		i = i->next
	wend

end sub

private sub hRenameSymbol _
	( _
		byval defines as THASH ptr, _
		byval types as THASH ptr, _
		byval globals as THASH ptr, _
		byval n as ASTNODE ptr, _
		byval code as ASTNODE ptr _
	)

	dim as string newid, hashid

	'' Build a new name by appending _ underscores, as long as needed to
	'' find an identifier that's not yet used in the namespace corresponding
	'' to the symbol. (otherwise renaming could cause more conflicts)
	assert( n->text )
	newid = *n->text

	dim as integer exists
	do
		newid += "_"

		hashid = ucase( newid )
		var hash = hashHash( hashid )

		exists = hashContains( @fbkeywordhash, hashid, hash )

		select case( n->class )
		case ASTCLASS_PPDEFINE
			exists or= hashContains( defines , hashid, hash )
			exists or= hashContains( types   , hashid, hash )
			exists or= hashContains( globals , hashid, hash )

		case ASTCLASS_PROC, ASTCLASS_PARAM, _
		     ASTCLASS_VAR, ASTCLASS_EXTERNVAR, ASTCLASS_STATICVAR, _
		     ASTCLASS_CONSTANT, ASTCLASS_ENUMCONST, ASTCLASS_FIELD
			exists or= hashContains( defines , hashid, hash )
			exists or= hashContains( globals , hashid, hash )

		case ASTCLASS_STRUCT, ASTCLASS_UNION, _
		     ASTCLASS_TYPEDEF, ASTCLASS_ENUM
			exists or= hashContains( defines , hashid, hash )
			exists or= hashContains( types   , hashid, hash )

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
	     ASTCLASS_VAR, ASTCLASS_EXTERNVAR, ASTCLASS_STATICVAR, _
	     ASTCLASS_CONSTANT, ASTCLASS_ENUMCONST, ASTCLASS_FIELD
		hReplaceCalls( code, n->text, newid )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		astReplaceSubtypes( code, ASTCLASS_TAGID, n->text, ASTCLASS_TAGID, newid )
	case ASTCLASS_TYPEDEF
		astReplaceSubtypes( code, ASTCLASS_ID, n->text, ASTCLASS_ID, newid )
	case else
		assert( FALSE )
	end select

	var origdump = astDumpPrettyDecl( n )

	'' Update the symbol and add it to the hash table
	astRenameSymbol( n, newid )
	select case( n->class )
	case ASTCLASS_PPDEFINE
		hashAddOverwrite( defines, hashid, n )
	case ASTCLASS_PROC, ASTCLASS_PARAM, _
	     ASTCLASS_VAR, ASTCLASS_EXTERNVAR, ASTCLASS_STATICVAR, _
	     ASTCLASS_CONSTANT,  ASTCLASS_ENUMCONST, ASTCLASS_FIELD
		hashAddOverwrite( globals, hashid, n )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, ASTCLASS_TYPEDEF
		hashAddOverwrite( types, hashid, n )
	case else
		assert( FALSE )
	end select

	if( frog.verbose ) then
		print "renaming " + origdump + " to " + astDumpPrettyDecl( n )
	end if

end sub

private sub hWalkAndRenameSymbols _
	( _
		byval defines as THASH ptr, _
		byval types as THASH ptr, _
		byval globals as THASH ptr, _
		byval n as ASTNODE ptr, _
		byval code as ASTNODE ptr _
	)

	var i = n->head
	while( i )

		if( i->attrib and ASTATTRIB_NEEDRENAME ) then
			hRenameSymbol( defines, types, globals, i, code )
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
		select case( i->class )
		case ASTCLASS_ENUM
			hWalkAndRenameSymbols( defines, types, globals, i, code )
		case ASTCLASS_STRUCT, ASTCLASS_UNION
			if( i->text = NULL ) then
				hWalkAndRenameSymbols( defines, types, globals, i, code )
			end if
		end select

		i = i->next
	wend

end sub

private sub hFixIdsInScope _
	( _
		byval defines as THASH ptr, _
		byval code as ASTNODE ptr _
	)

	'' Scope-specific namespaces for types and vars/procs/consts/fields/params
	dim as THASH types, globals
	hashInit( @types, 10, TRUE )
	hashInit( @globals, 10, TRUE )

	'' 1. Walk through symbols top-down, much like a C compiler, and mark
	''    those that need renaming with ASTATTRIB_NEEDRENAME.
	hWalkAndCheckIds( defines, @types, @globals, code )

	'' 2. Rename all marked symbols. Now that all symbols are known, we can
	''    generate new names for the symbols that need renaming without
	''    introducing more conflicts.
	hWalkAndRenameSymbols( defines, @types, @globals, code, code )

	hashEnd( @globals )
	hashEnd( @types )

end sub

sub astFixIds( byval code as ASTNODE ptr )
	'' Just one #define namespace, re-used also for nested scopes like
	'' struct bodies/parameter lists.
	dim as THASH defines
	hashInit( @defines, 10, TRUE )

	hFixIdsInScope( @defines, code )

	hashEnd( @defines )
end sub

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
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, ASTCLASS_APPENDBI
		function = TRUE
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

private function hSkipDontEmits( byval n as ASTNODE ptr ) as ASTNODE ptr
	while( n )
		if( (n->attrib and ASTATTRIB_DONTEMIT) = 0 ) then
			exit while
		end if
		n = n->next
	wend
	function = n
end function

''
'' Insert DIVIDERs between statements of different kind, e.g. all #defines in
'' a row shouldn't be divided, but a #define should be divided from a typedef.
'' Structs/unions/enums should always be separated by a divider because they're
'' compounds and normally span multiple lines themselves.
''
sub astAutoAddDividers( byval code as ASTNODE ptr )
	var i = hSkipDontEmits( code->head )
	while( i )
		var nxt = hSkipDontEmits( i->next )

		astAutoAddDividers( i )

		if( nxt ) then
			if( hShouldSeparate( i, nxt ) ) then
				astInsert( code, astNew( ASTCLASS_DIVIDER ), nxt )
			end if
		end if

		i = nxt
	wend
end sub

function astCountDecls( byval code as ASTNODE ptr ) as integer
	var count = 0

	var i = code->head
	while( i )

		select case( i->class )
		case ASTCLASS_DIVIDER, ASTCLASS_PPINCLUDE, ASTCLASS_PPENDIF, _
		     ASTCLASS_EXTERNBLOCKBEGIN, ASTCLASS_EXTERNBLOCKEND

		case ASTCLASS_PPIF, ASTCLASS_PPELSEIF, ASTCLASS_PPELSE
			count += astCountDecls( i )

		case else
			if( (i->attrib and ASTATTRIB_DONTEMIT) = 0 ) then
				count += 1
			end if
		end select

		i = i->next
	wend

	function = count
end function

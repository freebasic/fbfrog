'' Higher level code transformations

#include once "fbfrog.bi"

namespace hl
	'' Used by both typedef expansion and forward declaration addition passes
	'' hlExpandSpecialTypedefs: data = typedef ASTNODE, needs freeing
	'' hlCollectForwardUses/hlAddForwardDecls: data = hl.types array index
	dim shared typehash as THASH

	'' data = new name, needs freeing
	dim shared renamejobs as THASH

	'' Used by forward declaration addition pass
	type TYPENODE
		id		as zstring ptr  '' Type name
		definition	as ASTNODE ptr
		forwarduse	as integer
		first_used_filtered_out as integer
	end type
	dim shared types as TYPENODE ptr
	dim shared as integer typecount, typeroom

	'' Used by Extern block addition pass
	dim shared as integer need_extern, stdcalls, cdecls
	dim shared as integer mostusedcallconv

	'' Used by dtype use determination passes
	dim shared as integer uses_clong, uses_clongdouble, uses_wchar_t
end namespace

private sub hlApplyRemoveProcOptions( byval ast as ASTNODE ptr )
	var i = ast->head
	while( i )
		var nxt = i->next

		if( i->class = ASTCLASS_PROC ) then
			if( hashContains( @frog.idopt(OPT_REMOVEPROC), i->text, hashHash( i->text ) ) ) then
				astRemove( ast, i )
			end if
		end if

		i = nxt
	wend
end sub

private sub hApplyRenameOption( byval opt as integer, byval n as ASTNODE ptr )
	dim as ASTNODE ptr renameinfo = hashLookupDataOrNull( @frog.renameopt(opt), n->text )
	if( renameinfo ) then
		assert( *n->text = *renameinfo->alias )
		astSetText( n, renameinfo->text )
	end if
end sub

private function hlApplyRenameOption( byval n as ASTNODE ptr ) as integer
	if( typeGetDt( n->dtype ) = TYPE_UDT ) then
		assert( astIsTEXT( n->subtype ) )
		if( n->subtype->attrib and ASTATTRIB_TAGID ) then
			hApplyRenameOption( OPT_RENAMETAG, n->subtype )
		else
			hApplyRenameOption( OPT_RENAMETYPEDEF, n->subtype )
		end if
	end if

	select case( n->class )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		hApplyRenameOption( OPT_RENAMETAG, n )
	case ASTCLASS_TYPEDEF
		hApplyRenameOption( OPT_RENAMETYPEDEF, n )
	end select

	function = TRUE
end function

private sub hOopsCantExpandTypedef( byval typedef as ASTNODE ptr, byval n as ASTNODE ptr )
	oops( "can't solve out " + astDumpPrettyDecl( typedef ) + " in " + astDumpPrettyDecl( n ) )
end sub

private sub hExpandArrayTypedef( byval typedef as ASTNODE ptr, byval n as ASTNODE ptr )
	'' Pointer to array?
	if( typeGetPtrCount( n->dtype ) > 0 ) then
		'' FB doesn't support pointers to arrays either, but we can drop the array type,
		'' such that the pointer only points to "single array element". That's pretty much
		'' the best translation we can do here, and better than nothing...
		astSetType( n, typeExpand( n->dtype, typedef->dtype ), typedef->subtype )
		if( n->dtype = TYPE_NONE ) then
			hOopsCantExpandTypedef( typedef, n )
		end if
		exit sub
	end if

	'' Expand the array typedef
	astSetType( n, typeGetConst( n->dtype ) or typedef->dtype, typedef->subtype )
	if( n->array = NULL ) then
		n->array = astNew( ASTCLASS_ARRAY )
	end if
	astAppend( n->array, astCloneChildren( typedef->array ) )
end sub

''
'' Expand the function typedef used in the given declaration.
''
'' For example, given a declaration using a function typedef like this one:
''    typedef int (F)(int);
''
'' we want to expand the typedef into the declaration:
''    F f;             =>    int f1(int);            (what looked like a var really is a proc)
''    F *p;            =>    int (*p)(int);          (procptr var)
''    void f(F *p);    =>    void f(int (*p)(int));  (procptr param)
''    typedef F G;     =>    typedef int (G)(int);   (and then later we process G too)
''
'' because FB doesn't support function typedefs.
''
'' If there's a CONST on the function type, e.g.:
''    typedef int (T)(int);
''    T const f;
'' it will be overwritten and lost, i.e. we ignore it. According to
'' gcc -pedantic, "ISO C forbids qualified function types", and it seems like
'' the C++ standard requires cv-qualifiers to be ignored in such cases.
''
'' When expanding into a pointer, some of the existing CONSTs there need to be
'' preserved though:
''    a * const p1;
'' becomes:
''    int (* const p1)(int);
'' But as above, cv-qualifiers on the function type don't make sense and should
'' be ignored, e.g. in case of:
''    a const * p1;
''
private sub hExpandProcTypedef( byval typedef as ASTNODE ptr, byval n as ASTNODE ptr )
	select case( n->class )
	case ASTCLASS_VAR, ASTCLASS_FIELD
		'' Not a pointer?
		if( typeGetPtrCount( n->dtype ) = 0 ) then
			'' This is ok for vars/fields; they just become procedures themselves.
			'' (although with fields that can only really happen in C++ code)
			var proc = typedef->subtype
			assert( proc->class = ASTCLASS_PROC )

			'' Copy over the function result type (overwriting any previous cv-qualifiers),
			'' parameters, and callconv attributes
			n->class = ASTCLASS_PROC
			astSetType( n, proc->dtype, proc->subtype )
			assert( n->head = NULL )
			astAppend( n, astCloneChildren( proc ) )
			n->attrib or= proc->attrib

			exit sub
		end if

		'' var/field; it's a pointer to the function type; ok

	case ASTCLASS_TYPEDEF, ASTCLASS_PARAM
		'' Expanding typedef in another typedef; always ok
		'' Expanding into param, also ok (it becomes a function pointer;
		'' handled later in hlFixFunctionParameters())

	case else
		'' Anywhere else (function results, casts, sizeof):
		'' must be a pointer to the function type, or we can't expand the typedef
		'' TODO: check whether we can do better - do function types in
		'' casts or sizeof automatically become function pointers in C?
		if( typeGetPtrCount( n->dtype ) = 0 ) then
			hOopsCantExpandTypedef( typedef, n )
		end if
	end select

	'' Insert the typedef's type, overwriting the use of the typedef
	assert( typeGetDtAndPtr( typedef->dtype ) = TYPE_PROC )
	astSetType( n, typeUnsetBaseConst( typeSetDt( n->dtype, TYPE_PROC ) ), typedef->subtype )
end sub

private sub hlExpandSpecialTypedefs( byval n as ASTNODE ptr )
	'' Declaration using one of the special typedefs?
	if( typeGetDt( n->dtype ) = TYPE_UDT ) then
		assert( astIsTEXT( n->subtype ) )
		dim as ASTNODE ptr typedef = hashLookupDataOrNull( @hl.typehash, n->subtype->text )
		if( typedef ) then
			'' Expand the typedef into the declaration.
			'' Since typedefs are also processed this way here, we can be sure that
			'' typedefs themselves are already fully expanded when reaching another
			'' declaration using one of these typedefs. Thus, expanding a typedef
			'' doesn't cause a need for further expansions.
			if( typedef->array ) then
				hExpandArrayTypedef( typedef, n )
			else
				var typedefdtype = typedef->dtype
				select case( typeGetDtAndPtr( typedefdtype ) )
				case TYPE_PROC
					hExpandProcTypedef( typedef, n )
				case TYPE_ZSTRING, TYPE_WSTRING
					astSetType( n, typeExpand( n->dtype, typedefdtype ), NULL )
					assert( n->dtype <> TYPE_NONE )
					assert( n->subtype = NULL )
				end select
			end if
		end if
	end if

	if( n->subtype ) then hlExpandSpecialTypedefs( n->subtype )
	if( n->array   ) then hlExpandSpecialTypedefs( n->array   )
	if( n->expr    ) then hlExpandSpecialTypedefs( n->expr    )

	var i = n->head
	while( i )
		var nxt = i->next

		hlExpandSpecialTypedefs( i )

		if( i->class = ASTCLASS_TYPEDEF ) then
			var dtype = typeGetDtAndPtr( i->dtype )
			var is_array_or_proc = (i->array <> NULL) or (dtype = TYPE_PROC)
			var is_string = (dtype = TYPE_ZSTRING) or (dtype = TYPE_WSTRING)

			if( is_array_or_proc or is_string ) then
				if( is_array_or_proc ) then
					'' Array/proc typedefs can't be preserved for FB;
					'' remove from the AST, hl.typehash will free it later.
					astUnlink( n, i )
				else
					'' Others can be preserved - make a copy for the hl.typehash
					'' so we we'll free that instead of the node in the AST later.
					i = astClone( i )
				end if

				'' TODO: handle duplicate typedefs? (perhaps warn about them?)
				hashAddOverwrite( @hl.typehash, i->text, i )
			end if
		end if

		i = nxt
	wend
end sub

private function hlFixSpecialParameters( byval n as ASTNODE ptr ) as integer
	if( n->class = ASTCLASS_PARAM ) then
		select case( typeGetDtAndPtr( n->dtype ) )
		'' Remap "byval as jmp_buf" to "byval as jmp_buf ptr"
		'' jmp_buf is defined as array type in C, and arrays are passed byref in C.
		'' Since FB's crt/setjmp.bi defines jmp_buf as a simple UDT, we have to handle
		'' the "byref" part manually. Strictly speaking fbfrog should produce "byref as jmp_buf",
		'' but traditionally FB's headers always used a "byval as jmp_buf ptr"...
		case TYPE_UDT
			assert( astIsTEXT( n->subtype ) )
			if( *n->subtype->text = "jmp_buf" ) then
				n->dtype = typeAddrOf( n->dtype )
			end if

		'' Function type? It's really just a function pointer
		case TYPE_PROC
			n->dtype = typeAddrOf( n->dtype )

		end select

		'' C array parameter? It's really just a pointer (the array is passed byref).
		'' FB doesn't support C array parameters like that, so turn them into pointers:
		''    int a[5]  ->  byval a as long ptr
		if( n->array ) then
			astDelete( n->array )
			n->array = NULL
			if( typeGetPtrCount( n->dtype ) = TYPEMAX_PTR ) then
				oops( "too many pointers on " + astDumpPrettyDecl( n ) )
			end if
			n->dtype = typeAddrOf( n->dtype )
		end if
	end if
	function = TRUE
end function

''
'' Handle declarations with plain zstring/wstring type (originally char/wchar_t).
''
'' If it's a char pointer or array, then it's probably supposed to be a string,
'' and we can leave it as-is (zstring/wstring). If it's just a char, then it's
'' probably supposed to be a byte (or wchar_t) though.
''
'' FB doesn't allow "foo as zstring" - it must be a pointer or fixed-length string,
'' except in typedefs. Typedefs are a special case - char/wchar_t means byte/wchar_t
'' or zstring/wstring depending on where they're used. Because of this we expand
'' these typedefs like array/proc typedefs (see hlExpandSpecialTypedefs). To allow
'' this expansion to work, we keep the zstring/wstring type on the typedefs.
''
private function hlFixCharWchar( byval n as ASTNODE ptr ) as integer
	if( n->class <> ASTCLASS_STRING ) then
		select case( typeGetDtAndPtr( n->dtype ) )
		case TYPE_ZSTRING, TYPE_WSTRING
			if( n->array ) then
				'' Use the last (inner-most) array dimension as the fixed-length string size
				var d = n->array->tail
				assert( d->class = ASTCLASS_DIMENSION )
				assert( d->expr )
				assert( n->subtype = NULL )
				n->subtype = astClone( d->expr )
				astRemove( n->array, d )

				'' If no dimensions left, remove the array type entirely
				if( n->array->head = NULL ) then
					n->array = NULL
				end if
			elseif( n->class <> ASTCLASS_TYPEDEF ) then
				'' Turn zstring/wstring into byte/wchar_t, but preserve CONSTs
				if( typeGetDtAndPtr( n->dtype ) = TYPE_ZSTRING ) then
					n->dtype = typeGetConst( n->dtype ) or TYPE_BYTE
				else
					n->dtype = typeGetConst( n->dtype ) or TYPE_WCHAR_T
				end if
			end if
		end select
	end if
	function = TRUE
end function

private sub hlRenameJobsBegin( )
	hashInit( @hl.renamejobs, 6, TRUE )
end sub

private function hlRenameJobAdd( byval oldid as zstring ptr, byval newid as zstring ptr ) as integer
	newid = strDuplicate( newid )

	var hash = hashHash( oldid )
	var item = hashLookup( @hl.renamejobs, oldid, hash )
	if( item->s ) then
		'' Allow overwriting duplicate typedefs (if both oldid/newid are the same),
		'' but not if the newid differs (then it's a separate typedef which has to be
		'' preserved - we can't rename A => C when already renaming A => B).
		dim as zstring ptr prevnewid = item->data
		if( *prevnewid <> *newid ) then
			return FALSE
		end if

		'' Free existing newid if there already is a renamejob for this oldid,
		'' then the new newid can be stored
		deallocate( item->data )
		item->data = newid
	else
		hashAdd( @hl.renamejobs, item, hash, oldid, newid )
	end if

	function = TRUE
end function

private sub hMaybeApplyRenameJob( byval n as ASTNODE ptr )
	'' Is there a renamejob for this oldid?
	dim as zstring ptr newid = hashLookupDataOrNull( @hl.renamejobs, n->text )
	if( newid ) then
		'' Change the id if needed
		if( *n->text <> *newid ) then
			astSetText( n, newid )
		end if
		'' Remove attributes even if id strictly speaking doesn't change
		n->attrib and= not (ASTATTRIB_TAGID or ASTATTRIB_GENERATEDID)
	end if
end sub

'' Check all type names (definitions and references): If there is a rename job
'' for an id, then change it to use the newid.
private function hlApplyRenameJobs( byval n as ASTNODE ptr ) as integer
	if( typeGetDt( n->dtype ) = TYPE_UDT ) then
		assert( astIsTEXT( n->subtype ) )
		hMaybeApplyRenameJob( n->subtype )
	end if

	select case( n->class )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, ASTCLASS_TYPEDEF
		if( n->text ) then
			hMaybeApplyRenameJob( n )
		end if
	end select

	function = TRUE
end function

private sub hlRenameJobsEnd( byval ast as ASTNODE ptr )
	astVisit( ast, @hlApplyRenameJobs )
	scope
		'' Free the newids
		for i as integer = 0 to hl.renamejobs.room - 1
			var item = hl.renamejobs.items + i
			if( item->s ) then
				deallocate( item->data )
			end if
		next
	end scope
	hashEnd( @hl.renamejobs )
end sub

private sub hlSolveOutTagIds( byval n as ASTNODE ptr )
	var i = n->head
	while( i )
		var nxt = i->next

		if( i->class = ASTCLASS_TYPEDEF ) then
			if( i->dtype = TYPE_UDT ) then
				assert( astIsTEXT( i->subtype ) )
				'if( i->subtype->attrib and ASTATTRIB_TAGID ) then
				if( i->subtype->attrib and ASTATTRIB_GENERATEDID ) then
					var newid = i->text
					var oldid = i->subtype->text
					if( hlRenameJobAdd( oldid, newid ) ) then
						astRemove( n, i )
					end if
				end if
			end if
		end if

		i = nxt
	wend
end sub

'' Remove redundant typedefs where the typedef and the type have the same,
'' for example:
''    typedef struct A A;
''    typedef struct b B;  (FB is case-insensitive)
'' but not if there are pointers or CONSTs involved.
private sub hlRemoveSameIdTypedefs( byval n as ASTNODE ptr )
	var i = n->head
	while( i )
		var nxt = i->next

		if( i->class = ASTCLASS_TYPEDEF ) then
			if( i->dtype = TYPE_UDT ) then
				assert( astIsTEXT( i->subtype ) )
				var newid = i->text
				var oldid = i->subtype->text
				if( ucase( *oldid, 1 ) = ucase( *newid, 1 ) ) then
					if( hlRenameJobAdd( oldid, newid ) ) then
						astRemove( n, i )
					end if
				end if
			end if
		end if

		i = nxt
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
'' Auto-add forward declarations for types that are used before/without being defined.
''
'' This is definitely good for types that are defined by the binding.
'' However, sometimes headers use types without defining them:
''  a) types from other headers, e.g. "struct time" or "time_t" or "FILE"
''      => shouldn't add forward declarations for these
''  b) opaque types
''      => should add forward declarations for these
'' but it's hard to tell the difference between a) and b).
'' TODO: Hard-code some names for commonly used "external" types and then don't
'' add forward declarations for them unless they're defined in the binding? Add
'' command-line option for specifying whether types are external or not, or
'' whether a forward declaration needs to be added for a certain type name?
''
'' In C, forward references are only possible with tags, not typedefs, but we
'' don't need to care about this. In our AST and in FB all types are the same.
''
'' Sometimes C headers already contain "forward declarations" of the form:
''    typedef struct A B;
'' If a type is only forward-referenced in such typedefs, then we don't need to
'' add yet-another forward declaration, because these typedefs act as the
'' forward declaration already.
''

'' Return = index into hl.types array
private function hLookupOrAddType( byval id as zstring ptr ) as integer
	var hash = hashHash( id )
	var item = hashLookup( @hl.typehash, id, hash )
	if( item->s = NULL ) then
		'' Append to hl.types array
		if( hl.typeroom = hl.typecount ) then
			if( hl.typeroom = 0 ) then
				hl.typeroom = 512
			else
				hl.typeroom *= 2
			end if
			hl.types = reallocate( hl.types, hl.typeroom * sizeof( *hl.types ) )
		end if
		with( hl.types[hl.typecount] )
			.id = id
			.definition = NULL
			.forwarduse = FALSE
			.first_used_filtered_out = FALSE
		end with
		hashAdd( @hl.typehash, item, hash, id, cptr( any ptr, hl.typecount ) )
		function = hl.typecount
		hl.typecount += 1
	else
		function = cint( item->data )
	end if
end function

private sub hMarkTypeAsDefined( byval n as ASTNODE ptr )
	var i = hLookupOrAddType( n->text )
	hl.types[i].definition = n
end sub

private sub hMaybeMarkTypeAsForwardUsed( byval id as zstring ptr, byval filterout as integer )
	var i = hLookupOrAddType( id )
	var typ = hl.types + i

	'' Type not defined yet? First use?
	if( (typ->definition = NULL) and (not typ->forwarduse) ) then
		typ->forwarduse = TRUE
		typ->first_used_filtered_out = filterout
	end if
end sub

private function hlCollectForwardUses( byval n as ASTNODE ptr ) as integer
	'' If this is an UDT definition (tag body or typedef), register the type as "defined"
	'' (before scanning the struct fields for potential forward-references, so that "recursive"
	'' references to this UDT aren't seen as forward references)
	select case( n->class )
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, ASTCLASS_TYPEDEF
		'' Named? (enums can be anonymous)
		if( n->text ) then
			hMarkTypeAsDefined( n )
		end if
	end select

	'' Collect forward references from everything except typedefs
	if( n->class <> ASTCLASS_TYPEDEF ) then
		if( typeGetDt( n->dtype ) = TYPE_UDT ) then
			assert( astIsTEXT( n->subtype ) )
			hMaybeMarkTypeAsForwardUsed( n->subtype->text, ((n->attrib and ASTATTRIB_FILTEROUT) <> 0) )
		end if
	end if

	'' Don't visit #define bodies - type references in them don't count as
	'' forward references.
	'' TODO: still add forward declarations for types that are only
	'' referenced from #defines (probably rare in practice though)
	function = (n->class <> ASTCLASS_PPDEFINE)
end function

'' Add forward decls for forward-referenced types that also are defined, so we
'' can be sure that they're from this binding and not from another header.
private sub hlAddForwardDecls( byval n as ASTNODE ptr )
	for i as integer = hl.typecount - 1 to 0 step -1
		var typ = hl.types + i
		if( typ->forwarduse and (typ->definition <> NULL) ) then
			typ->definition->attrib or= ASTATTRIB_FORWARDDECLARED
			if( typ->first_used_filtered_out = FALSE ) then
				astPrepend( n, astNew( ASTCLASS_FORWARDDECL, typ->id ) )
			end if
		end if
	next
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Remove all declarations marked with ASTATTRIB_FILTEROUT.
'' We should only ever filter out whole declarations, never remove parts of a
'' declaration. Hence no recursion here - only visiting the toplevel constructs.
sub hlFilterOut( byval n as ASTNODE ptr )
	var i = n->head
	while( i )
		var nxt = i->next
		if( i->attrib and ASTATTRIB_FILTEROUT ) then
			astRemove( n, i )
		end if
		i = nxt
	wend
end sub

private function hlCountCallConvs( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_PROC
		hl.need_extern = TRUE
		if( n->attrib and ASTATTRIB_STDCALL ) then
			hl.stdcalls += 1
		else
			assert( n->attrib and ASTATTRIB_CDECL )
			hl.cdecls += 1
		end if
	case ASTCLASS_VAR
		hl.need_extern or= ((n->attrib and ASTATTRIB_EXTERN) <> 0) or _
		                   ((n->attrib and ASTATTRIB_STATIC) = 0)
	end select
	'' Don't count code in macro bodies
	function = (n->class <> ASTCLASS_PPDEFINE)
end function

private function hlHideCallConv( byval n as ASTNODE ptr ) as integer
	if( n->class = ASTCLASS_PROC ) then
		if( n->attrib and hl.mostusedcallconv ) then
			n->attrib or= ASTATTRIB_HIDECALLCONV
		end if
	end if
	'' Don't hide callconvs in macro bodies, otherwise they could
	'' end up using the wrong callconv if expanded outside the
	'' header's Extern block.
	function = (n->class <> ASTCLASS_PPDEFINE)
end function

private function hlSearchSpecialDtypes( byval n as ASTNODE ptr ) as integer
	select case( typeGetDt( n->dtype ) )
	case TYPE_CLONG, TYPE_CULONG
		hl.uses_clong = TRUE
	case TYPE_CLONGDOUBLE
		hl.uses_clongdouble = TRUE
	case TYPE_WCHAR_T
		hl.uses_wchar_t = TRUE
	end select
	function = TRUE
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

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
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
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

		astAutoAddDividers( i )

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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub highlevelWork( byval ast as ASTNODE ptr, byval directincludes as ASTNODE ptr )
	hl.need_extern = FALSE
	hl.stdcalls = 0
	hl.cdecls = 0
	hl.mostusedcallconv = 0
	hl.uses_clong = FALSE
	hl.uses_clongdouble = FALSE
	hl.uses_wchar_t = FALSE

	'' Apply -removeproc options, if any
	if( frog.idopt(OPT_REMOVEPROC).count > 0 ) then
		hlApplyRemoveProcOptions( ast )
	end if

	'' Apply -renametag/-renametypedef options, if any
	if( (frog.renameopt(OPT_RENAMETAG).count + frog.renameopt(OPT_RENAMETYPEDEF).count) > 0 ) then
		astVisit( ast, @hlApplyRenameOption )
	end if

	'' Solve out special typedefs
	''  * array/function typedefs, which aren't supported in FB
	''  * char/wchar_t typedefs, which become zstring/wstring in some
	''    places, but byte/wchar_t in others (i.e. it depends on context,
	''    which is why the typedef needs to be expanded)
	'' TODO: If possible, turn function typedefs into function pointer
	'' typedefs instead of solving them out
	hashInit( @hl.typehash, 8, FALSE )
	hlExpandSpecialTypedefs( ast )
	scope
		'' Free the collected typedefs which were unlinked from the main AST
		for i as integer = 0 to hl.typehash.room - 1
			var item = hl.typehash.items + i
			if( item->s ) then
				astDelete( item->data )
			end if
		next
	end scope
	hashEnd( @hl.typehash )

	'' Fix up parameters with certain special types:
	''   function => function pointer
	''   array => pointer to the element type
	''   jmp_buf => jmp_buf ptr (jmp_buf is an array type in C)
	astVisit( ast, @hlFixSpecialParameters )

	'' The C parser turned char/wchar_t into zstring/wstring.
	'' Now turn zstring/wstring into byte/wchar_t where it's obviously
	'' not a string.
	astVisit( ast, @hlFixCharWchar )

	''
	'' Solve out various tags/typedefs
	''
	'' 1. Solve out tag ids (both explicitly given or auto-generated ones)
	'' if there is a typedef, but only once per tag id. (we don't want to
	'' lose additional typedefs). Auto-generated tag ids should be solved
	'' out where possible, since they're not part of the original API.
	'' Furthermore, C headers often use tag ids for internal purposes only,
	'' or specify them but don't use them, and programs are supposed to use
	'' the typedefs instead.
	''
	'' This can already take care of some exact/case-alias typedefs, but not
	'' necessarily all, e.g.:
	''      struct tagid A
	''      typedef a as tagid A
	''      typedef A as tagid A
	''  =>
	''      struct a
	''      typedef A as a
	''
	'' This can produce exact/case-alias typedefs:
	''      struct tagid A
	''      typedef b as tagid A
	''      typedef B as tagid A
	''  =>
	''      struct b
	''      typedef B as b
	''
	'' TODO: currently only auto-generated tag ids are solved out -
	'' enable solving out of normal tag ids too.
	''
	hlRenameJobsBegin( )
	hlSolveOutTagIds( ast )
	hlRenameJobsEnd( ast )

	'' 2. Solve out remaining exact/case-alias typedefs - they're useless
	'' and/or impossible in FB, because it's case-insensitive and doesn't
	'' have separate tag/typedef namespaces.
	hlRenameJobsBegin( )
	hlRemoveSameIdTypedefs( ast )
	hlRenameJobsEnd( ast )

	''
	'' Auto-add forward declarations for types (any UDT or typedef)
	''
	'' Even though typedefs can't be forward-referenced in C, the situation
	'' can occur in the fbfrog AST (e.g. due to tags/typedefs being solved
	'' out above), and it's possible in FB too...
	''
	'' In order to avoid adding false-positive forward decls for types like
	'' FILE/jmp_buf/time_t and tags such as "struct tm", we only add forward
	'' decls if we also have the corresponding definition in the AST. While
	'' we may miss some needed forward decls this way, having to add those
	'' to the binding manually seems better than maintaining a list of types
	'' and tags for which fbfrog should never ever add forward decls.
	''
	hashInit( @hl.typehash, 8, FALSE )
	hl.types = NULL
	hl.typecount = 0
	hl.typeroom = 0
	astVisit( ast, @hlCollectForwardUses )
	hlAddForwardDecls( ast )
	hashEnd( @hl.typehash )
	deallocate( hl.types )

	'' Remove declarations marked by filterout
	hlFilterOut( ast )

	'' Add Extern block
	''  * to preserve identifiers of global variables/procedures
	''  * to cover the most-used calling convention
	astVisit( ast, @hlCountCallConvs )
	if( hl.need_extern ) then
		if( hl.stdcalls > hl.cdecls ) then
			hl.mostusedcallconv = ASTATTRIB_STDCALL
		else
			hl.mostusedcallconv = ASTATTRIB_CDECL
		end if

		'' Remove the calling convention from all procdecls,
		'' the Extern block will take over
		astVisit( ast, @hlHideCallConv )

		var externblock = @"C"
		if( hl.mostusedcallconv = ASTATTRIB_STDCALL ) then
			if( frog.windowsms ) then
				externblock = @"Windows-MS"
			else
				externblock = @"Windows"
			end if
		end if

		assert( ast->class = ASTCLASS_GROUP )
		astPrepend( ast, astNew( ASTCLASS_EXTERNBLOCKBEGIN, externblock ) )
		astAppend( ast, astNew( ASTCLASS_EXTERNBLOCKEND ) )
	end if

	'' Add #includes for "crt/long[double].bi" and "crt/wchar.bi" if the
	'' binding uses the clong[double]/wchar_t types
	astVisit( ast, @hlSearchSpecialDtypes )
	if( hl.uses_clongdouble ) then
		astPrepend( directincludes, astNew( ASTCLASS_PPINCLUDE, "crt/longdouble.bi" ) )
	end if
	if( hl.uses_clong ) then
		astPrepend( directincludes, astNew( ASTCLASS_PPINCLUDE, "crt/long.bi" ) )
	end if
	if( hl.uses_wchar_t ) then
		var wcharbi = astNew( ASTCLASS_PPINCLUDE, "crt/wchar.bi" )
		if( astGroupContains( directincludes, wcharbi ) ) then
			astDelete( wcharbi )
		else
			astPrepend( directincludes, wcharbi )
		end if
	end if

	'' Prepend the direct #includes (if any), outside the EXTERN block
	astPrependMaybeWithDivider( ast, directincludes )
end sub

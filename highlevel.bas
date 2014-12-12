'' Higher level code transformations

#include once "fbfrog.bi"

function astLookupMacroParam _
	( _
		byval macro as ASTNODE ptr, _
		byval id as zstring ptr, _
		byref macroparam as ASTNODE ptr _
	) as integer

	var index = 0

	assert( macro->class = ASTCLASS_PPDEFINE )

	var param = macro->head
	while( param )

		assert( param->class = ASTCLASS_MACROPARAM )
		if( *param->text = *id ) then
			macroparam = param
			return index
		end if

		index += 1
		param = param->next
	wend

	function = -1
end function

private sub astHideCallConv( byval n as ASTNODE ptr, byval callconv as integer )
	if( n->class = ASTCLASS_PROC ) then
		if( n->attrib and callconv ) then
			n->attrib or= ASTATTRIB_HIDECALLCONV
		end if
	end if

	if( typeGetDt( n->dtype ) = TYPE_PROC ) then
		astHideCallConv( n->subtype, callconv )
	end if
	if( n->array ) then astHideCallConv( n->array, callconv )

	select case( n->class )
	case ASTCLASS_PPDEFINE
		'' Don't hide callconvs in macro bodies, otherwise they could
		'' end up using the wrong callconv if expanded outside the
		'' header's Extern block.
	case ASTCLASS_SYM
		'' Don't visit the nodes behind symbol references,
		'' that could cause infinite recursion
	case else
		if( n->expr ) then astHideCallConv( n->expr, callconv )
	end select

	var i = n->head
	while( i )
		astHideCallConv( i, callconv )
		i = i->next
	wend
end sub

sub astWrapInExternBlock( byval ast as ASTNODE ptr, byval callconv as integer )
	var externblock = @"C"
	if( callconv = ASTATTRIB_STDCALL ) then
		if( frog.windowsms ) then
			externblock = @"Windows-MS"
		else
			externblock = @"Windows"
		end if
	end if

	'' Remove the calling convention from all procdecls, the Extern block
	'' will take over
	astHideCallConv( ast, callconv )

	assert( ast->class = ASTCLASS_GROUP )
	astPrepend( ast, astNew( ASTCLASS_EXTERNBLOCKBEGIN, externblock ) )
	astAppend( ast, astNew( ASTCLASS_EXTERNBLOCKEND ) )
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

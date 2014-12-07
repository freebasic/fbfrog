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

private function hPreferRenaming( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_PPDEFINE, ASTCLASS_ENUMCONST
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

		case ASTCLASS_VAR, ASTCLASS_ENUMCONST
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

private sub hRenameSymbol _
	( _
		byval reservedids as THASH ptr, _
		byval types as THASH ptr, _
		byval globals as THASH ptr, _
		byval n as ASTNODE ptr, _
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
		     ASTCLASS_VAR, ASTCLASS_ENUMCONST, ASTCLASS_FIELD
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

	'' Update the symbol and add it to the hash table
	astRenameSymbol( n, newid )
	select case( n->class )
	case ASTCLASS_PPDEFINE
		hashAddOverwrite( reservedids, hashid, n )
	case ASTCLASS_PROC, ASTCLASS_PARAM, _
	     ASTCLASS_VAR, ASTCLASS_ENUMCONST, ASTCLASS_FIELD
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
		byval renamelist as ASTNODE ptr _
	)

	var i = n->head
	while( i )

		if( i->attrib and ASTATTRIB_NEEDRENAME ) then
			hRenameSymbol( reservedids, types, globals, i, renamelist )
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
			hWalkAndRenameSymbols( reservedids, types, globals, i, renamelist )
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
	'' If at toplevel, process the tags
	if( code = api->ast ) then
		for i as integer = 0 to api->tagcount - 1
			var n = api->tags[i]
			if( (n->attrib and ASTATTRIB_BODYDEFINED) = 0 ) then
				hCheckId( @fbkeywordhash, n, FALSE )
				hCheckId( reservedids, n, FALSE )
				hCheckId( @types, n, TRUE )
			end if
		next
	end if
	hWalkAndCheckIds( reservedids, @types, @globals, code, renamelist )

	'' 2. Rename all marked symbols. Now that all symbols are known, we can
	''    generate new names for the symbols that need renaming without
	''    introducing more conflicts.
	if( code = api->ast ) then
		for i as integer = 0 to api->tagcount - 1
			var n = api->tags[i]
			if( (n->attrib and ASTATTRIB_BODYDEFINED) = 0 ) then
				if( n->attrib and ASTATTRIB_NEEDRENAME ) then
					hRenameSymbol( reservedids, @types, @globals, n, renamelist )
				end if
			end if
		next
	end if
	hWalkAndRenameSymbols( reservedids, @types, @globals, code, renamelist )

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
	end if
	hashEnd( @reservedids )
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

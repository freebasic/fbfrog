#include once "parser.bi"

private function parseNestedStructBegin( byval x as integer ) as integer
	dim as integer begin = any

	'' {STRUCT|UNION} '{'
	select case( tkGet( x ) )
	case KW_STRUCT, KW_UNION

	case else
		return x
	end select

	begin = x
	x = hSkip( x )

	'' '{'
	if( tkGet( x ) <> TK_LBRACE ) then
		return begin
	end if

	tkSetMark( MARK_STRUCT, begin, x )

	function = hSkip( x )
end function

private function parseNestedStructEnd _
	( _
		byval x as integer, _
		byval toplevelopening as integer _
	) as integer

	dim as integer begin = any, opening = any

	begin = x

	'' '}'
	if( tkGet( x ) <> TK_RBRACE ) then
		return begin
	end if

	'' Find the opening '{', to determine whether this is a struct
	'' or a union. Bail out if there is no matching nested struct/union
	'' begin.
	opening = hFindParentheses( x, -1 )
	if( (opening = x) or (opening <= toplevelopening) ) then
		return begin
	end if
	opening = hSkipRev( opening )
	assert( tkMark( opening ) = MARK_STRUCT )

	'' '}'
	assert( tkGet( x ) = TK_RBRACE )
	x = hSkip( x )

	'' [id]
	if( tkGet( x ) = TK_ID ) then
		x = hSkip( x )
	end if

	'' ';'
	if( tkGet( x ) <> TK_SEMI ) then
		return begin
	end if

	tkSetMark( iif( (tkGet( opening ) = KW_STRUCT), _
				MARK_ENDSTRUCT, MARK_ENDUNION ), begin, x )

	function = hSkip( x )
end function

function parseStruct( byval x as integer ) as integer
	dim as integer begin = any, compoundkw = any, level = any, old = any, _
		toplevelopening = any, structend = any

	'' [TYPEDEF] {STRUCT|UNION|ENUM} [id] '{'
	'' ...
	'' '}' [no-type-multdecl] ';'

	begin = x

	'' [TYPEDEF]
	if( tkGet(x) = KW_TYPEDEF) then
		x = hSkip( x )
	end if

	'' {STRUCT|UNION|ENUM}
	compoundkw = tkGet( x )
	select case( compoundkw )
	case KW_ENUM, KW_STRUCT, KW_UNION

	case else
		return begin
	end select

	x = hSkip( x )

	'' [id]
	if( tkGet( x ) = TK_ID ) then
		x = hSkip(x)
	end if

	'' '{'
	if( tkGet( x ) <> TK_LBRACE ) then
		return begin
	end if

	tkSetMark( MARK_STRUCT, begin, x )

	toplevelopening = x
	x = hSkip(x)

	'' Body: Struct fields/enum constants, nested structs/unions,
	'' possibly intermixed with PP directives.
	level = 0
	do
		old = x

		if( compoundkw = KW_ENUM ) then
			x = parseEnumconst( x, FALSE )
		else
			x = parseNestedStructBegin( x )
			if( x > old ) then
				level += 1
			end if

			x = parseMultdecl( x, x, DECL_FIELD )
		end if

		'' '}'?
		select case( tkGet( x ) )
		case TK_RBRACE
			if( level > 0 ) then
				x = parseNestedStructEnd( x, toplevelopening )
				level -= 1
			else
				exit do
			end if

		case TK_EOF
			return begin
		end select

		x = parsePPDirective( x, FALSE )

		if( x = old ) then
			'' Ok, there's something weird inside this struct/enum
			'' body that the PP directive/enumconst/field parsers
			'' didn't recognize. Ignore this and try to parse
			'' other fields etc.
			''return begin
			if( compoundkw = KW_ENUM ) then
				x = parseEnumconst( x, TRUE )
			else
				x = parseUnknown( x )
			end if
		end if
	loop

	'' '}'
	structend = x
	assert( tkGet( x ) = TK_RBRACE )
	x = hSkip( x )

	'' If this was a typedef-to-struct-block, there will be a multdecl
	'' following now...
	if( tkGet( begin ) = KW_TYPEDEF ) then
		x = parseMultdecl( x, begin, DECL_TYPEDEFSTRUCTBLOCK )
	end if

	'' ';'
	if( tkGet( x ) <> TK_SEMI ) then
		return begin
	end if

	select case( compoundkw )
	case KW_ENUM
		compoundkw = MARK_ENDENUM
	case KW_STRUCT
		compoundkw = MARK_ENDSTRUCT
	case else
		compoundkw = MARK_ENDUNION
	end select

	tkSetMark( compoundkw, structend, x )

	function = hSkip( x )
end function

function parseExternBegin( byval x as integer ) as integer
	dim as integer begin = any

	'' EXTERN "C" '{'

	if( tkGet( x ) <> KW_EXTERN ) then
		return x
	end if

	begin = x
	x = hSkip( x )

	'' "C"
	if( tkGet( x ) <> TK_STRING ) then
		return begin
	end if
	x = hSkip( x )

	'' '{'
	if( tkGet( x ) <> TK_LBRACE ) then
		return begin
	end if

	tkSetMark( MARK_EXTERN, begin, x )

	'' EXTERN parsing is done here, so the content is parsed from the
	'' toplevel loop.
	function = hSkip( x )
end function

function parseExternEnd( byval x as integer ) as integer
	dim as integer opening = any, mark = any

	'' '}'
	if( tkGet( x ) <> TK_RBRACE ) then
		return x
	end if

	'' Check whether this '}' belongs to an 'extern "C" {'
	opening = hFindParentheses( x, -1 )
	if( opening = x ) then
		return x
	end if

	mark = tkMark( opening )
	if( mark <> MARK_EXTERN ) then
		return x
	end if

	tkSetMark( MARK_ENDEXTERN, x, x )

	function = hSkip( x )
end function

function translateCompoundEnd _
	( _
		byval x as integer, _
		byval compoundkw as integer _
	) as integer

	'' '}'                  ->    END EXTERN
	'' '}' [multdecl] ';'   ->    END {TYPE|ENUM|UNION}
	'' Where multdecl can be something like <A, *PA, (*FPA)()> etc. in
	'' the case of <typedef struct {} multdecl ;>. See also the struct
	'' translator. Here this just needs to be removed.

	'' Remove '}'
	assert( tkGet( x ) = TK_RBRACE )

	if( compoundkw = KW_EXTERN ) then
		tkRemove( x, x )
	else
		tkRemove( x, hFindToken( x, TK_SEMI ) )
	end if

	tkInsert( x, KW_END, NULL )
	x += 1
	tkInsert( x, compoundkw, NULL )
	x += 1

	if( hIsWhitespaceUntilEol( hSkipRev( x ) + 1 ) = FALSE ) then
		x = hInsertStatementSeparator( x )
	end if

	function = x
end function

function translateStruct( byval x as integer ) as integer
	dim as integer is_typedef = any, compoundkw = any, structid = any, _
		typedefbegin = any, y = any, spacebegin = any

	'' structs/enums beginnings are translated here.
	'' To support typedefs to struct blocks (which might also be
	'' anonymous) the ending is searched to retrieve the typedef id
	'' from there.
	''
	'' struct T { ... };               ->    type T : ... : end type
	'' Note: any occurences of <struct T> will be translated to just <T>
	'' by the type translator, so this just works.
	''
	'' typedef struct { ... } TT;      ->    type TT : ... : end type
	''
	'' typedef struct T { ... } TT;    ->    type T : ... : end type
	''                                       type as T TT
	'' Afterall both identifiers might be needed.
	''
	'' typedef struct A { ... } A;     ->    type A : ... : end type
	''
	'' All in all this means:
	'' If the struct has an id, then use it for the type. If it has no
	'' id, then move the one at the end of the declaration to the front.
	'' If there is none there either, insert a TODO instead.
	'' If both struct and typedef have an identifier, but they are
	'' different, then insert a <typedef struct structid typedefid;>
	'' after the struct end, which will then be handled by the typedef
	'' translator.
	''
	'' Same for enums/unions. And this is also used to translate nested
	'' structs/unions. Note: FB doesn't support non-anonymous nested
	'' structs.
	''    struct { } id;               ->    type id /' TODO '/ : ...

	'' [TYPEDEF]
	is_typedef = FALSE
	if( tkGet( x ) = KW_TYPEDEF ) then
		tkRemove( x, hSkip( x ) - 1 )
		is_typedef = TRUE
	end if

	compoundkw = tkGet( x )
	if( compoundkw = KW_STRUCT ) then
		'' STRUCT -> TYPE
		tkRemove( x, x )
		tkInsert( x, KW_TYPE, NULL )
	else
		assert( (compoundkw = KW_ENUM) or (compoundkw = KW_UNION) )
	end if

	x = hSkip( x )

	'' ['id']
	structid = x
	if( tkGet( x ) = TK_ID ) then
		x = hSkip( x )
	end if

	'' '{'
	assert( tkGet( x ) = TK_LBRACE )

	'' Typedef-to-struct-block split up hack:
	''    typedef struct T { ... } a, *b, (*c)();
	'' must be translated to:
	''    type T : ... : end type : typedef struct T a, *b, (*c)();
	'' so then the normal typedef parser can handle the typedef.
	'' (Having something like <typedef struct { ... } FOO, *PFOO;>
	'' is quite common, so this is important)
	''
	'' However, there also is this common use case with an
	'' anonymous struct:
	''    typedef struct { ... } T;
	'' And that is usually best translated to:
	''    type T : ... : end type
	''
	'' But of course this can only be done if the typedef is just
	'' a plain simple identifier like 'T'. If it has pointers,
	'' commas + multiple identifiers or even procedure pointers,
	'' then the typedef must be split off. If the struct is
	'' anonymous then, a fake identifer has to be used (though
	'' choosing that id is the user's job, we just insert a TODO).
	''
	'' And there also is this use case (named nested union/struct):
	''    struct { } foo;
	'' Which isn't supported in FB, but we can still translate it
	'' to translate the contained fields too, and add a TODO.

	'' 1. Find the '}'
	'' 2. Check whether there is a single identifier.
	''    If the struct is anonymous, copy that identifier to the
	''    front.
	''    Otherwise, do the typedef split up hack.

	y = hFindParentheses( x, 1 )

	'' '}'
	assert( tkGet( y ) = TK_RBRACE )
	select case( compoundkw )
	case KW_ENUM
		assert( tkMark( y ) = MARK_ENDENUM )
	case KW_STRUCT
		assert( tkMark( y ) = MARK_ENDSTRUCT )
	case else
		assert( tkMark( y ) = MARK_ENDUNION )
	end select
	y = hSkip( y )

	'' <id ';'> and struct is anonymous?
	if( (tkGet( y )          =  TK_ID   ) and _
	    (tkGet( hSkip( y ) ) =  TK_SEMI ) and _
	    (tkGet( structid )   <> TK_ID   )       ) then

		'' Add TODO for <struct { } id;>
		if( is_typedef = FALSE ) then
			tkInsert( structid, TK_TODO, "not supported in FB" )
			structid += 1
			x += 1
			y += 1
		end if

		'' Copy the trailing id to the front.
		tkCopy( structid, y, y )
		x += 1

	elseif( is_typedef ) then
		'' Do the split up hack.
		'' If the struct is anonymous, we must fake an id,
		'' it's needed for the typedef we want to split off.
		'' (need to declare a typedef to /something/)

		if( tkGet( structid ) <> TK_ID ) then
			tkInsert( structid, TK_TODO, "faked id" )
			structid += 1
			tkInsert( structid, TK_ID, "FAKE" )

			'' Inserting at the top moves everything down..
			x += 3
			y += 3
		end if

		'' Turn this:
		''    ... a, *b, (*c)();
		'' into:
		''    ... ; typedef struct structid a, *b, (*c)();

		'' Add a new ';' and mark it as same as '}'
		tkInsert( y, TK_SEMI, NULL )
		tkSetMark( tkMark( hSkipRev( y ) ), y, y )
		y += 1

		typedefbegin = y

		tkInsert( y, KW_TYPEDEF, NULL )
		y += 1
		tkInsert( y, compoundkw, NULL )
		y += 1
		tkCopy( y, structid, structid )
		y += 1

		'' Skip over the existing multdecl, until ';'
		while( tkGet( y ) <> TK_SEMI )
			assert( tkGet( y ) <> TK_EOF )
			y = hSkip( y )
		wend

		'' Ensure it's translated as typedef
		tkSetMark( MARK_TYPEDEF, typedefbegin, y )
	end if

	'' Remove the '{'. If there is an EOL following, insert a ':'
	'' statement separator.
	assert( tkGet( x ) = TK_LBRACE )
	spacebegin = hSkipRev( x ) + 1
	tkRemove( spacebegin, x )
	x -= x - spacebegin

	if( hIsWhitespaceUntilEol( x ) = FALSE ) then
		x = hInsertStatementSeparator( x )
	end if

	function = x
end function

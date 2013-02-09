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

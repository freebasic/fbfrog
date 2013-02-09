#include once "parser.bi"

function parseEnumconst _
	( _
		byval x as integer, _
		byval is_unknown as integer _
	) as integer

	dim as integer begin = any, skip_expression = any, level = any

	begin = x
	skip_expression = is_unknown

	if( is_unknown = FALSE ) then
		'' id
		if( tkGet( x ) <> TK_ID ) then
			return begin
		end if
		x = hSkip( x )

		select case( tkGet( x ) )
		case TK_EQ
			'' ['=']
			skip_expression = TRUE
			x = hSkip( x )
		case TK_LPAREN
			'' '('
			'' This allows function-like macros like <FOO(...)>
			'' in place of constants in an enum, at least the
			'' libcurl headers make extensive use of that...
			'' The '(...)' skipping is covered by the same code
			'' that skips over the '= ...' expressions.
			skip_expression = TRUE
		end select
	end if

	if( skip_expression ) then
		'' Skip until ',' or '}'
		level = 0
		do
			select case( tkGet( x ) )
			case TK_LPAREN
				level += 1
			case TK_RPAREN
				level -= 1
			case TK_COMMA
				if( level = 0 ) then
					exit do
				end if
			case TK_RBRACE, TK_EOF, TK_HASH
				'' Note: '#' (PP directives) not allowed in
				'' expressions in FB, this can't be translated.
				exit do
			end select
			x = hSkip( x )
		loop
	end if

	select case( tkGet( x ) )
	case TK_COMMA
		'' Treat the comma as part of the constant declaration
		x = hSkip( x )
	case TK_RBRACE

	case else
		if( is_unknown = FALSE ) then
			return begin
		end if
	end select

	'' Mark the constant declaration
	tkSetMark( iif( is_unknown, MARK_UNKNOWNENUMCONST, MARK_ENUMCONST ), _
			begin, hSkipRev( x ) )

	function = x
end function

private function parseBaseType( byval x as integer ) as integer
	dim as integer old = any

	old = x

	'' [CONST]
	x = hSkipOptional( x, KW_CONST )

	select case( tkGet( x ) )
	case KW_ENUM, KW_STRUCT, KW_UNION
		'' {ENUM | STRUCT | UNION} id
		x = hSkip( x )

		'' id
		if( tkGet( x ) <> TK_ID ) then
			return old
		end if
		x = hSkip( x )

		'' [CONST]
		x = hSkipOptional( x, KW_CONST )

		return x

	case TK_ID
		'' Just a single id
		x = hSkip( x )

		'' [CONST]
		x = hSkipOptional( x, KW_CONST )

		return x
	end select

	'' [SIGNED | UNSIGNED]
	select case( tkGet( x ) )
	case KW_SIGNED, KW_UNSIGNED
		x = hSkip( x )
	end select

	'' [ VOID | CHAR | FLOAT | DOUBLE | INT
	'' | SHORT [INT]
	'' | LONG [LONG] [INT]
	'' ]
	select case( tkGet( x ) )
	case KW_VOID, KW_CHAR, KW_FLOAT, KW_DOUBLE, KW_INT
		x = hSkip( x )

	case KW_SHORT
		x = hSkip( x )

		'' [INT]
		x = hSkipOptional( x, KW_INT )

	case KW_LONG
		x = hSkip( x )

		'' [LONG]
		x = hSkipOptional( x, KW_LONG )

		'' [INT]
		x = hSkipOptional( x, KW_INT )

	end select

	'' [CONST]
	x = hSkipOptional( x, KW_CONST )

	'' In case of no type keyword at all, x = old
	function = x
end function

private function parsePtrs( byval x as integer ) as integer
	'' Pointers: ('*')*
	while( tkGet( x ) = TK_STAR )
		x = hSkip( x )

		'' [CONST] (behind the '*')
		x = hSkipOptional( x, KW_CONST )
	wend
	function = x
end function

function parseMultdecl _
	( _
		byval x as integer, _
		byval begin as integer, _
		byval decl as integer _
	) as integer

	dim as integer typebegin = any, is_procptr = any, old = any, mark = any

	'' Generic 'type *a, **b;' parsing,
	'' used for vardecls/fielddecls/procdecls/params...
	''
	'' type '*'* var (',' '*'* var)* ';'
	''
	'' Where var can be:
	'' a plain id: var = id
	'' a procptr:  var = '(' '*'+ id ')' '(' params ')'
	'' a procdecl: var = id '(' params ')'

	'' No type hack for the typedef-to-struct-block parser:
	'' its type is the struct block, which it already parsed...
	if( decl <> DECL_TYPEDEFSTRUCTBLOCK ) then
		'' type
		typebegin = x
		x = parseBaseType( x )
		if( x = typebegin ) then
			return begin
		end if
	end if

	'' var (',' var)*
	do
		'' '*'*
		x = parsePtrs( x )

		'' '('?
		is_procptr = FALSE
		if( tkGet( x ) = TK_LPAREN ) then
			is_procptr = TRUE
			x = hSkip( x )

			'' '*'
			if( tkGet( x ) <> TK_STAR ) then
				return begin
			end if
			x = hSkip( x )
		end if

		'' id (must be there except for params)
		if( tkGet( x ) = TK_ID ) then
			x = hSkip( x )
		elseif( decl <> DECL_PARAM ) then
			return begin
		end if

		if( is_procptr ) then
			'' ')'
			if( tkGet( x ) <> TK_RPAREN ) then
				return begin
			end if
			x = hSkip( x )
		end if

		'' Check for '(' params ')'
		if( tkGet( x ) = TK_LPAREN ) then
			'' Note: typedef to procdecl can't be translated,
			'' so it's disallowed here. If there are procdecls
			'' in fields, then fine, that's C++ stuff, but oh well.
			'' Note: procptrs always have params.
			if( (is_procptr = FALSE) and _
			    ((decl = DECL_TYPEDEF) or _
			     (decl = DECL_TYPEDEFSTRUCTBLOCK)) ) then
				return begin
			end if

			'' '('
			x = hSkip( x )

			'' Just '(void)'?
			if( (tkGet( x ) = KW_VOID) and (tkGet( hSkip( x ) ) = TK_RPAREN)) then
				'' VOID
				x = hSkip( x )
			else
				'' [ type [id]  (',' type [id] )*  [ ',' '...' ] ]
				'' Note: The following isn't even doing that much
				'' syntax verification at all, but it's ok, it's not
				'' a compiler afterall.
				do
					select case( tkGet( x ) )
					case TK_RPAREN
						exit do
					case TK_EOF
						return begin
					case TK_COMMA, TK_ELLIPSIS
						'' Let ',' and '...' pass
						x = hSkip( x )
					case else
						old = x
						x = parseMultdecl( x, x, DECL_PARAM )
						if( x = old ) then
							return begin
						end if
					end select
				loop
			end if

			'' ')'
			if( tkGet( x ) <> TK_RPAREN ) then
				return begin
			end if
			x = hSkip( x )
		end if

		'' Everything can have a comma and more identifiers,
		'' except for params.
		if( (decl = DECL_PARAM) or (tkGet( x ) <> TK_COMMA) ) then
			exit do
		end if
		x = hSkip( x )
	loop

	select case( decl )
	case DECL_FIELD, DECL_TYPEDEF, DECL_TOP
		'' ';'
		if( tkGet( x ) <> TK_SEMI ) then
			return begin
		end if

		mark = MARK_TOPDECL
		select case( decl )
		case DECL_FIELD
			mark = MARK_FIELDDECL
		case DECL_TYPEDEF
			mark = MARK_TYPEDEF
		end select

		tkSetMark( mark, begin, x )
		x = hSkip( x )

	end select

	function = x
end function

function parseTopdeclOrTypedef( byval x as integer ) as integer
	dim as integer begin = any, decl = any, multdecl = any

	'' Toplevel declarations: global variables (including procedure
	'' pointers), procedure declarations, and also typedefs, since they
	'' are almost the same (parse_multdecl() disallows typedefs to
	'' procdecls, because those aren't possible FB).
	''    int a, *b, (*c)(), f(), *g();
	''    ...
	''
	'' [TYPEDEF|EXTERN|STATIC] multdecl

	begin = x
	decl = DECL_TOP

	select case( tkGet( x ) )
	case TK_ID
		'' Allow an id at the front, if it represents a calling
		'' convention, a dllexport declspec or is empty...
		if( frogAddDefine( tkText( x ), 0 ) ) then
			x = hSkip( x )
		end if
	case KW_EXTERN, KW_STATIC
		x = hSkip( x )
	case KW_TYPEDEF
		decl = DECL_TYPEDEF
		x = hSkip( x )
	end select

	multdecl = x
	x = parseMultdecl( x, begin, decl )
	if( x = multdecl ) then
		return begin
	end if

	function = x
end function

#include once "fbfrog.bi"

declare function cStructCompound( byval x as integer ) as integer
declare function cMultDecl( byval x as integer, byval decl as integer ) as integer

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function cFindEOL( byval x as integer ) as integer
	while( tkIsStmtSep( x ) = FALSE )
		x += 1
	wend
	function = x
end function

private function cFindClosingParen( byval x as integer ) as integer
	dim as integer level = any, opening = any, closing = any

	opening = tkGet( x )
	level = 0
	select case( opening )
	case TK_LBRACE
		closing = TK_RBRACE
	case TK_LBRACKET
		closing = TK_RBRACKET
	case TK_LPAREN
		closing = TK_RPAREN
	case else
		return x
	end select

	do
		x += 1

		select case( tkGet( x ) )
		case opening
			level += 1

		case closing
			if( level = 0 ) then
				exit do
			end if

			level -= 1

		case -1
			exit do

		end select
	loop

	function = x
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Remove all comments unless they're at EOL
sub cPurgeInlineComments( )
	dim as integer x = any

	x = 0
	do
		select case( tkGet( x ) )
		case TK_COMMENT
			if( tkIsStmtSep( x + 1 ) = FALSE ) then
				tkRemove( x, x )
				x -= 1
			end if

		case TK_EOF
			exit do
		end select

		x += 1
	loop
end sub

private function cPPDirective( byval x as integer ) as integer
	dim as integer begin = any

	'' '#'
	begin = x
	x += 1

	select case( tkGet( x ) )
	'' DEFINE Identifier ['(' ParameterList ')'] Body Eol .
	case KW_DEFINE
		'' DEFINE
		x += 1

		'' Identifier?
		if( tkGet( x ) <> TK_ID ) then
			return begin
		end if

		tkInsert( begin, TK_PPDEFINEBEGIN, tkGetText( x ) )
		begin += 1
		tkRemove( begin, x )

		'' '(' and not separated from the Identifier with spaces?
		'' TODO

		'' Body
		x = cFindEOL( begin )
		tkInsert( x, TK_PPDEFINEEND )
		x += 1

	case KW_INCLUDE
		'' INCLUDE
		x += 1

		if( tkGet( x ) <> TK_STRING ) then
			return begin
		end if

		tkInsert( begin, TK_PPINCLUDE, tkGetText( x ) )
		begin += 1
		tkRemove( begin, cFindEOL( begin ) )
		x = begin

	case else
		return begin
	end select

	function = x
end function

sub cPPDirectives( )
	dim as integer x = any, old = any

	x = 0
	do
		old = x

		select case( tkGet( x ) )
		'' '#'?
		case TK_HASH
			'' BOL?
			if( tkIsStmtSep( x - 1 ) ) then
				x = cPPDirective( x )
				if( x <> old ) then
					x -= 1
				end if
			end if

		case TK_EOF
			exit do
		end select

		x += 1
	loop
end sub

'' After PP directives were parsed, EOLs can be removed completely
sub cPurgeEOLs( )
	dim as integer x = any

	x = 0
	do
		select case( tkGet( x ) )
		case TK_EOL
			tkRemove( x, x )
			x -= 1

		case TK_EOF
			exit do
		end select

		x += 1
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function cStructBody( byval x as integer ) as integer
	dim as integer old = any

	do
		old = x

		x = cMultDecl( x, TK_FIELD )
		x = cStructCompound( x )

		'' '}'?
		select case( tkGet( x ) )
		case TK_RBRACE, TK_EOF
			exit do
		end select

		if( old = x ) then
			x += 1
		end if
	loop

	function = x
end function

private function cStructCompound( byval x as integer ) as integer
	dim as integer begin = any
	dim as zstring ptr id = any

	'' {STRUCT|UNION} [Identifier] '{'
	'' ...
	'' '}'
	begin = x

	'' {STRUCT|UNION}
	select case( tkGet( x ) )
	case KW_STRUCT, KW_UNION

	case else
		return begin
	end select
	x += 1

	'' [Identifier]
	if( tkGet( x ) = TK_ID ) then
		id = tkGetText( x )
		x += 1
	else
		id = NULL
	end if

	'' '{'
	if( tkGet( x ) <> TK_LBRACE ) then
		return begin
	end if

	'' STRUCT|UNION [Identifier] '{'   ->   STRUCTBEGIN
	tkInsert( begin, TK_STRUCTBEGIN, id )
	begin += 1
	x += 1
	tkRemove( begin, x )
	x = begin

	begin = cStructBody( begin )
	x = begin

	'' ['}']
	if( tkGet( x ) = TK_RBRACE ) then
		x += 1
	end if

	'' [';']
	if( tkGet( x ) = TK_SEMI ) then
		x += 1
	end if

	'' ['}' ';']   ->   STRUCTEND
	tkInsert( begin, TK_STRUCTEND )
	begin += 1
	x += 1
	if( x <> begin ) then
		tkRemove( begin, x - 1 )
	end if

	function = begin
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function cConstMod _
	( _
		byval x as integer, _
		byref dtype as integer _
	) as integer

	'' [CONST]
	if( tkGet( x ) = KW_CONST ) then
		dtype = typeSetIsConst( dtype )
		x += 1
	end if

	function = x
end function

private function cBaseType _
	( _
		byval x as integer, _
		byref dtype as integer, _
		byref subtype as string _
	) as integer

	dim as integer begin = any, sign = any

	dtype = TYPE_NONE
	subtype = ""

	begin = x
	sign = 0

	'' (CONST|SIGNED|UNSIGNED)*
	do
		'' [CONST]
		x = cConstMod( x, dtype )

		'' [SIGNED | UNSIGNED]
		select case( tkGet( x ) )
		case KW_SIGNED
			sign = -1
			x += 1
		case KW_UNSIGNED
			sign = 1
			x += 1
		case else
			exit do
		end select
	loop

	select case( tkGet( x ) )
	case KW_ENUM, KW_STRUCT, KW_UNION
		'' {ENUM|STRUCT|UNION}
		x += 1

		'' Identifier
		if( tkGet( x ) <> TK_ID ) then
			return FALSE
		end if

		dtype = typeSetDt( dtype, TYPE_UDT )
		subtype = *tkGetText( x )
		x += 1

	case TK_ID
		'' Identifier
		dtype = typeSetDt( dtype, TYPE_UDT )
		subtype = *tkGetText( x )
		x += 1

	case KW_VOID
		dtype = typeSetDt( dtype, TYPE_ANY )
		x += 1

	case KW_CHAR
		select case( sign )
		case -1
			dtype = typeSetDt( dtype, TYPE_BYTE )
		case 1
			dtype = typeSetDt( dtype, TYPE_UBYTE )
		case else
			dtype = typeSetDt( dtype, TYPE_ZSTRING )
		end select
		x += 1

	case KW_FLOAT
		dtype = typeSetDt( dtype, TYPE_SINGLE )
		x += 1

	case KW_DOUBLE
		dtype = typeSetDt( dtype, TYPE_DOUBLE )
		x += 1

	case KW_SHORT
		if( sign > 0 ) then
			dtype = typeSetDt( dtype, TYPE_USHORT )
		else
			dtype = typeSetDt( dtype, TYPE_SHORT )
		end if
		x += 1

		'' [INT]
		if( tkGet( x ) = KW_INT ) then
			x += 1
		end if

	case KW_INT
		if( sign > 0 ) then
			dtype = typeSetDt( dtype, TYPE_ULONG )
		else
			dtype = typeSetDt( dtype, TYPE_LONG )
		end if
		x += 1

	case KW_LONG
		if( sign > 0 ) then
			dtype = typeSetDt( dtype, TYPE_ULONG )
		else
			dtype = typeSetDt( dtype, TYPE_LONG )
		end if
		x += 1

		'' [LONG]
		if( tkGet( x ) = KW_LONG ) then
			if( sign > 0 ) then
				dtype = typeSetDt( dtype, TYPE_ULONGINT )
			else
				dtype = typeSetDt( dtype, TYPE_LONGINT )
			end if
			x += 1
		end if

		'' [INT]
		if( tkGet( x ) = KW_INT ) then
			x += 1
		end if

	case else
		return FALSE
	end select

	'' [CONST]
	x = cConstMod( x, dtype )

	tkRemove( begin, x - 1 )
	function = TRUE
end function

private function cPtrCount _
	( _
		byval x as integer, _
		byref dtype as integer _
	) as integer

	'' Pointers: ('*')*
	while( tkGet( x ) = TK_STAR )
		dtype = typeAddrOf( dtype )
		x += 1

		'' [CONST]
		x = cConstMod( x, dtype )
	wend

	function = x
end function

function cDeclElement _
	( _
		byval x as integer, _
		byval decl as integer, _
		byval basedtype as integer, _
		byref basesubtype as string _
	) as integer

	dim as integer dtype = any, begin = any, elementtk = any
	dim as zstring ptr id = any

	'' plain id: PtrCount Identifier
	'' procptr:  PtrCount '(' '*' Identifier ')' '(' ParamList ')'
	'' proto:    PtrCount Identifier '(' ParamList ')'
	begin = x
	dtype = basedtype

	'' PtrCount
	x = cPtrCount( x, dtype )

#if 0
	'' '('?
	is_procptr = FALSE
	if( tkGet( x ) = TK_LPAREN ) then
		x += 1

		'' '*'
		if( tkGet( x ) <> TK_STAR ) then
			return begin
		end if
		x += 1

		is_procptr = TRUE
	end if
#endif

	'' Identifier (must be there except for params)
	if( tkGet( x ) = TK_ID ) then
		id = tkGetText( x )
		x += 1
	else
		if( decl <> TK_PARAM ) then
			return begin
		end if

		id = NULL
	end if

#if 0
	if( is_procptr ) then
		'' ')'
		if( tkGet( x ) <> TK_RPAREN ) then
			return begin
		end if
		x += 1
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
#endif

	tkInsert( begin, decl, id )
	tkSetType( begin, dtype, basesubtype )
	begin += 1
	tkRemove( begin, x )
	x = begin

	function = x
end function

'' Generic 'type *a, **b;' parsing, used for vars/fields/protos/params
'' ("multiple declaration" syntax)
''    int i;
''    int a, b, c;
''    int *a, ***b, c;
''    int f(void);
''    int (*procptr)(void);
function cMultDecl( byval x as integer, byval decl as integer ) as integer
	dim as integer dtype = any, begin = any, old = any
	dim as string subtype

	'' BaseType DeclElement (',' DeclElement)* [';']
	begin = x

	'' BaseType
	if( cBaseType( x, dtype, subtype ) = FALSE ) then
		return begin
	end if

	'' ... (',' ...)*
	do
		old = x
		x = cDeclElement( x, decl, dtype, subtype )

		if( x = old ) then
			return begin
		end if

		'' Everything can have a comma and more identifiers,
		'' except for params.
		select case( decl )
		case TK_PARAM, TK_PARAMPROCPTR
			exit do
		end select

		'' ','?
		if( tkGet( x ) <> TK_COMMA ) then
			exit do
		end if
		tkRemove( x, x )
	loop

	'' Everything except params must end with a ';'
	select case( decl )
	case TK_PARAM, TK_PARAMPROCPTR

	case else
		'' ';'
		if( tkGet( x ) <> TK_SEMI ) then
			return begin
		end if
		tkRemove( x, x )
	end select

	function = x
end function

'' Global variable/procedure declarations
private function cGlobalDecl( byval x as integer ) as integer
	dim as integer begin = any, old = any, decl = any

	'' [EXTERN|STATIC] MultDecl
	begin = x

	select case( tkGet( x ) )
	case KW_EXTERN
		decl = TK_EXTERNGLOBAL
		x += 1
	case KW_STATIC
		decl = TK_STATICGLOBAL
		x += 1
	case else
		decl = TK_GLOBAL
	end select

	old = x
	x = cMultDecl( x, decl )
	if( x = old ) then
		return begin
	end if

	function = x
end function

sub cToplevel( )
	dim as integer x = any, old = any

	x = 0
	do
		old = x

		x = cStructCompound( x )
		x = cGlobalDecl( x )

		if( tkGet( x ) = TK_EOF ) then
			exit do
		end if

		if( x = old ) then
			x += 1
		end if
	loop
end sub

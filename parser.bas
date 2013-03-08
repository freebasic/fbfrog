'' C parsing passes

#include once "fbfrog.bi"

declare function cStructCompound( byval x as integer ) as integer
declare function cMultDecl _
	( _
		byval x as integer, _
		byval decl as integer _
	) as integer

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function ppSkip( byval x as integer ) as integer
	dim as integer y = any

	do
		x += 1

		select case( tkGet( x ) )
		case TK_SPACE, TK_COMMENT, TK_LINECOMMENT

		case TK_BEGIN
			do
				x += 1
			loop while( tkGet( x ) <> TK_END )

		'' Escaped EOLs don't end PP directives, though normal EOLs do
		'' '\' [Space] EOL
		case TK_BACKSLASH
			y = x

			do
				y += 1
			loop while( tkGet( y ) = TK_SPACE )

			if( tkGet( y ) <> TK_EOL ) then
				exit do
			end if
			x = y

		case else
			exit do
		end select
	loop

	function = x
end function

private function cSkip( byval x as integer ) as integer
	do
		x += 1

		select case( tkGet( x ) )
		case TK_SPACE, TK_COMMENT, TK_LINECOMMENT, TK_EOL

		case TK_BEGIN
			do
				x += 1
			loop while( tkGet( x ) <> TK_END )

		case else
			exit do
		end select
	loop

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
		x = cSkip( x )

		select case( tkGet( x ) )
		case opening
			level += 1

		case closing
			if( level = 0 ) then
				exit do
			end if

			level -= 1

		case TK_EOF
			exit do

		end select
	loop

	function = x
end function

function cSkipStatement( byval x as integer ) as integer
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_SEMI
			x = cSkip( x )
			exit do

		case TK_LPAREN, TK_LBRACKET, TK_LBRACE
			x = cFindClosingParen( x )

		case else
			if( tkIsStmtSep( x ) ) then
				exit do
			end if

			x = cSkip( x )
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

private function ppSkipToEOL( byval x as integer ) as integer
	do
		x = ppSkip( x )

		select case( tkGet( x ) )
		case TK_EOL, TK_EOF
			exit do
		end select
	loop

	function = x
end function

private function ppDirective( byval x as integer ) as integer
	dim as integer begin = any, filename = any

	'' '#'
	begin = x
	x = ppSkip( x )

	select case( tkGet( x ) )
	'' DEFINE Identifier ['(' ParameterList ')'] Body Eol .
	case KW_DEFINE
		'' DEFINE
		x = ppSkip( x )

		'' Identifier?
		if( tkGet( x ) <> TK_ID ) then
			return begin
		end if

		tkInsert( begin, TK_PPDEFINE, tkGetText( x ) )
		begin += 1
		tkInsert( begin, TK_BEGIN )
		begin += 1
		tkRemove( begin, ppSkip( x ) - 1 )
		x = begin

		'' '(' and not separated from the Identifier with spaces?
		'' TODO

		x = ppSkipToEOL( x )

		tkInsert( x - 1, TK_END )
		x += 1

	case KW_INCLUDE
		'' INCLUDE
		x = ppSkip( x )

		'' "..."
		if( tkGet( x ) <> TK_STRING ) then
			return begin
		end if
		filename = x
		x = ppSkip( x )

		if( tkGet( x ) <> TK_EOL ) then
			return begin
		end if

		tkInsert( begin, TK_PPINCLUDE, tkGetText( filename ) )
		begin += 1
		x += 1
		tkRemove( begin, x )
		x = begin

	case else
		return begin
	end select

	function = x
end function

private function ppUnknown( byval x as integer ) as integer
	tkInsert( x, TK_TODO, "unknown PP directive (sorry)" )
	x += 1
	tkInsert( x, TK_BEGIN )
	x += 1

	x = ppSkipToEOL( x )
	tkInsert( x, TK_END )
	x += 1

	function = ppSkip( x )
end function

sub cPPDirectives( )
	dim as integer x = any, old = any

	x = ppSkip( -1 )
	do
		select case( tkGet( x ) )
		'' '#'?
		case TK_HASH
			'' BOL?
			if( tkIsStmtSep( x - 1 ) ) then
				old = x
				x = ppDirective( x )
				if( x = old ) then
					x = ppUnknown( x )
				end if
			else
				x = ppSkip( ppSkipToEOL( x ) )
			end if

		case TK_EOF
			exit do

		case else
			x = ppSkip( ppSkipToEOL( x ) )
		end select
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub cInsertDividers( )
	dim as integer x = any, y = any

	x = ppSkip( -1 )
	while( tkGet( x ) <> TK_EOF )
		if( (tkGet( x           ) = TK_EOL) and _
		    (tkGet( ppSkip( x ) ) = TK_EOL) ) then
			y = x
			while( tkGet( y ) = TK_EOL )
				y = ppSkip( y )
			wend
			tkRemove( x, y - 1 )
			tkInsert( x, TK_DIVIDER )
			x += 1
		else
			x = ppSkip( x )
		end if
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' (MultDecl{Field} | StructCompound)*
private function cStructBody( byval x as integer ) as integer
	dim as integer old = any

	do
		old = x
		x = cMultDecl( x, TK_FIELD )
		x = cStructCompound( x )
	loop while( old <> x )

	function = x
end function

'' {STRUCT|UNION} [Identifier] '{' StructBody '}'
private function cStructCompound( byval x as integer ) as integer
	dim as integer begin = any
	dim as string id

	begin = x

	'' {STRUCT|UNION}
	select case( tkGet( x ) )
	case KW_STRUCT, KW_UNION

	case else
		return begin
	end select
	x = cSkip( x )

	'' [Identifier]
	if( tkGet( x ) = TK_ID ) then
		id = *tkGetText( x )
		x = cSkip( x )
	end if

	'' '{'
	if( tkGet( x ) <> TK_LBRACE ) then
		return begin
	end if
	x = cSkip( x )

	tkRemove( begin, x - 1 )
	tkInsert( begin, TK_STRUCT, id )
	begin += 1
	x = begin

	tkInsert( x, TK_BEGIN )
	x += 1

	x = cStructBody( x )

	tkInsert( x, TK_END )
	x += 1

	begin = x

	'' '}'
	if( tkGet( x ) = TK_RBRACE ) then
		x = cSkip( x )
	end if

	'' ';'
	if( tkGet( x ) = TK_SEMI ) then
		x = cSkip( x )
	end if

	tkRemove( begin, x - 1 )
	x = begin

	function = x
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' (CONST)*
private function cConstMod _
	( _
		byval x as integer, _
		byref dtype as integer _
	) as integer

	while( tkGet( x ) = KW_CONST )
		dtype = typeSetIsConst( dtype )
		x = cSkip( x )
	wend

	function = x
end function

private function cBaseType _
	( _
		byval x as integer, _
		byref dtype as integer, _
		byref subtype as string _
	) as integer

	dim as integer sign = any, begin = any

	begin = x
	dtype = TYPE_NONE
	subtype = ""
	sign = 0

	'' [CONST]
	x = cConstMod( x, dtype )

	'' [SIGNED|UNSIGNED]
	select case( tkGet( x ) )
	case KW_SIGNED
		sign = -1
		x = cSkip( x )
	case KW_UNSIGNED
		sign = 1
		x = cSkip( x )
	end select

	select case( tkGet( x ) )
	case KW_ENUM, KW_STRUCT, KW_UNION
		if( sign <> 0 ) then
			return begin
		end if

		'' {ENUM|STRUCT|UNION}
		x = cSkip( x )

		'' Identifier
		if( tkGet( x ) <> TK_ID ) then
			return begin
		end if
		dtype = typeSetDt( dtype, TYPE_UDT )
		subtype = *tkGetText( x )
		x = cSkip( x )

	case TK_ID
		if( sign <> 0 ) then
			return begin
		end if

		'' Identifier
		dtype = typeSetDt( dtype, TYPE_UDT )
		subtype = *tkGetText( x )
		x = cSkip( x )

	case KW_VOID
		if( sign <> 0 ) then
			return begin
		end if

		x = cSkip( x )
		dtype = typeSetDt( dtype, TYPE_ANY )

	case KW_CHAR
		x = cSkip( x )
		select case( sign )
		case -1
			dtype = typeSetDt( dtype, TYPE_BYTE )
		case 1
			dtype = typeSetDt( dtype, TYPE_UBYTE )
		case else
			dtype = typeSetDt( dtype, TYPE_ZSTRING )
		end select

	case KW_FLOAT
		x = cSkip( x )
		dtype = typeSetDt( dtype, TYPE_SINGLE )

	case KW_DOUBLE
		x = cSkip( x )
		dtype = typeSetDt( dtype, TYPE_DOUBLE )

	case KW_SHORT
		x = cSkip( x )
		if( sign > 0 ) then
			dtype = typeSetDt( dtype, TYPE_USHORT )
		else
			dtype = typeSetDt( dtype, TYPE_SHORT )
		end if

		'' [INT]
		if( tkGet( x ) = KW_INT ) then
			x = cSkip( x )
		end if

	case KW_INT
		x = cSkip( x )
		if( sign > 0 ) then
			dtype = typeSetDt( dtype, TYPE_ULONG )
		else
			dtype = typeSetDt( dtype, TYPE_LONG )
		end if

	case KW_LONG
		x = cSkip( x )
		if( sign > 0 ) then
			dtype = typeSetDt( dtype, TYPE_ULONG )
		else
			dtype = typeSetDt( dtype, TYPE_LONG )
		end if

		'' [LONG]
		if( tkGet( x ) = KW_LONG ) then
			x = cSkip( x )
			if( sign > 0 ) then
				dtype = typeSetDt( dtype, TYPE_ULONGINT )
			else
				dtype = typeSetDt( dtype, TYPE_LONGINT )
			end if
		end if

		'' [INT]
		if( tkGet( x ) = KW_INT ) then
			x = cSkip( x )
		end if

	case else
		return begin
	end select

	'' [CONST]
	x = cConstMod( x, dtype )

	function = x
end function

private function cPtrCount _
	( _
		byval x as integer, _
		byref dtype as integer _
	) as integer

	'' Pointers: ('*')*
	while( tkGet( x ) = TK_STAR )
		dtype = typeAddrOf( dtype )
		x = cSkip( x )

		'' [CONST]
		x = cConstMod( x, dtype )
	wend

	function = x
end function

''    '...' | MultDecl{Param}
private function cParamDecl( byval x as integer ) as integer
	'' '...'?
	if( tkGet( x ) = TK_ELLIPSIS ) then
		tkRemove( x, x )
		tkInsert( x, TK_PARAMVARARG )
		x = cSkip( x )
	else
		x = cMultDecl( x, TK_PARAM )
	end if

	function = x
end function

'' [ParamDecl (',' ParamDecl)*]
private function cParamDeclList( byval x as integer ) as integer
	do
		x = cParamDecl( x )

		'' ','?
		if( tkGet( x ) <> TK_COMMA ) then
			exit do
		end if
		tkRemove( x, cSkip( x ) - 1 )
	loop

	function = x
end function

'' plain id: PtrCount Identifier
'' procptr:  PtrCount '(' '*' Identifier ')' '(' ParamList ')'
'' proto:    PtrCount Identifier '(' ParamList ')'
private function cDeclElement _
	( _
		byval x as integer, _
		byval decl as integer, _
		byval basedtype as integer, _
		byref basesubtype as string _
	) as integer

	dim as integer begin = any
	dim as integer dtype = any, is_procptr = any, has_params = any
	dim as string id

	begin = x
	dtype = basedtype

	'' PtrCount
	x = cPtrCount( x, dtype )

	'' '('?
	if( tkGet( x ) = TK_LPAREN ) then
		x = cSkip( x )

		'' '*'
		if( tkGet( x ) <> TK_STAR ) then
			return begin
		end if
		x = cSkip( x )

		select case( decl )
		case TK_GLOBAL
			decl = TK_GLOBALPROCPTR
		case TK_EXTERNGLOBAL
			decl = TK_EXTERNGLOBALPROCPTR
		case TK_STATICGLOBAL
			decl = TK_STATICGLOBALPROCPTR
		case TK_FIELD
			decl = TK_FIELDPROCPTR
		case TK_PARAM
			decl = TK_PARAMPROCPTR
		case else
			return begin
		end select
		is_procptr = TRUE
	else
		is_procptr = FALSE
	end if

	'' Identifier (must be there except for params)
	if( tkGet( x ) = TK_ID ) then
		id = *tkGetText( x )
		x = cSkip( x )
	else
		select case( decl )
		case TK_PARAM, TK_PARAMPROCPTR, TK_PARAMVARARG

		case else
			return begin
		end select
	end if

	if( is_procptr ) then
		'' ')'
		if( tkGet( x ) <> TK_RPAREN ) then
			return begin
		end if
		x = cSkip( x )
	end if

	'' '('? (procedure parameters)
	if( tkGet( x ) = TK_LPAREN ) then
		x = cSkip( x )

		select case( decl )
		case TK_GLOBAL, TK_EXTERNGLOBAL, TK_STATICGLOBAL, TK_FIELD
			decl = TK_PROC
		case TK_GLOBALPROCPTR, TK_EXTERNGLOBALPROCPTR, TK_STATICGLOBALPROCPTR, _
		     TK_PARAMPROCPTR, TK_FIELDPROCPTR

		case else
			return begin
		end select

		has_params = TRUE
	else
		'' If it's a function pointer there must also be a parameter list
		if( is_procptr ) then
			return begin
		end if
		has_params = FALSE
	end if

	tkRemove( begin, x - 1 )
	tkInsert( begin, decl, id )
	tkSetType( begin, dtype, basesubtype )
	begin += 1
	x = begin

	if( has_params ) then
		'' Just '(void)'?
		if( (tkGet( x ) = KW_VOID) and (tkGet( cSkip( x ) ) = TK_RPAREN) ) then
			'' VOID
			tkRemove( x, cSkip( x ) - 1 )
		'' Not just '()'?
		elseif( tkGet( x ) <> TK_RPAREN ) then
			tkInsert( x, TK_BEGIN )
			x += 1

			x = cParamDeclList( x )

			tkInsert( x, TK_END )
			x += 1
		end if

		'' ')'
		if( tkGet( x ) = TK_RPAREN ) then
			tkRemove( x, cSkip( x ) - 1 )
		end if
	end if

	function = x
end function

''
'' Generic 'type *a, **b;' parsing, used for vars/fields/protos/params
'' ("multiple declaration" syntax)
''    int i;
''    int a, b, c;
''    int *a, ***b, c;
''    int f(void);
''    int (*procptr)(void);
''
''    BaseType DeclElement (',' DeclElement)* [';']
''
private function cMultDecl _
	( _
		byval x as integer, _
		byval decl as integer _
	) as integer

	dim as integer begin = any, old = any, dtype = any
	dim as integer typebegin = any, typeend = any
	dim as string subtype

	begin = x

	'' BaseType
	typebegin = x
	x = cBaseType( x, dtype, subtype )
	if( typebegin = x ) then
		return begin
	end if
	typeend = x

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
		case TK_PARAM, TK_PARAMPROCPTR, TK_PARAMVARARG
			exit do
		end select

		'' ','?
		if( tkGet( x ) <> TK_COMMA ) then
			exit do
		end if
		x = cSkip( x )
	loop

	'' Everything except params must end with a ';'
	select case( decl )
	case TK_PARAM, TK_PARAMPROCPTR, TK_PARAMVARARG

	case else
		'' ';'
		if( tkGet( x ) <> TK_SEMI ) then
			return begin
		end if
		tkRemove( x, cSkip( x ) - 1 )
	end select

	tkRemove( typebegin, typeend - 1 )
	x -= typeend - typebegin

	function = x
end function

'' Global variable/procedure declarations
''    [EXTERN|STATIC] MultDecl
private function cGlobalDecl( byval x as integer ) as integer
	dim as integer decl = any

	select case( tkGet( x ) )
	case KW_EXTERN
		decl = TK_EXTERNGLOBAL
		tkRemove( x, cSkip( x ) - 1 )

	case KW_STATIC
		decl = TK_STATICGLOBAL
		tkRemove( x, cSkip( x ) - 1 )

	case else
		decl = TK_GLOBAL
	end select

	function = cMultDecl( x, decl )
end function

private function cSemiColon( byval x as integer ) as integer
	if( tkGet( x ) = TK_SEMI ) then
		tkRemove( x, cSkip( x ) - 1 )

		'' A token must be inserted to ensure the x position moves forward
		tkInsert( x, TK_NOP )
		x += 1
	end if
	function = x
end function

private function cUnknown( byval x as integer ) as integer
	tkInsert( x, TK_TODO, "unknown construct (sorry)" )
	x += 1
	tkInsert( x, TK_BEGIN )
	x += 1

	x = cSkipStatement( x )
	tkInsert( x, TK_END )
	x += 1

	function = cSkip( x )
end function

sub cToplevel( )
	dim as integer x = any, old = any

	x = cSkip( -1 )
	do
		old = x

		x = cStructCompound( x )
		x = cGlobalDecl( x )
		x = cSemiColon( x )

		if( x = old ) then
			if( tkGet( x ) = TK_EOF ) then
				exit do
			end if

			if( tkIsStmtSep( x ) ) then
				x = cSkip( x )
			else
				x = cUnknown( x )
			end if
		end if
	loop
end sub

''
'' C parsing
''
'' cToplevel() parses the content of the tk buffer without modifying it,
'' and returns the resulting AST.
''

#include once "fbfrog.bi"

type PARSERSTUFF
	x		as integer
	pass		as integer

	tempidcount	as integer
end type

dim shared as PARSERSTUFF parse

enum
	DECL_VAR = 0
	DECL_EXTERNVAR
	DECL_STATICVAR
	DECL_FIELD
	DECL_PARAM
	DECL_TYPEDEF
end enum

declare function cStructCompound( ) as ASTNODE ptr
declare function cIdList _
	( _
		byval decl as integer, _
		byval basedtype as integer, _
		byval basesubtype as ASTNODE ptr _
	) as ASTNODE ptr
declare function cMultDecl( byval decl as integer ) as ASTNODE ptr

private function hMakeTempId( ) as string
	parse.tempidcount += 1
	function = "__fbfrog_AnonStruct" & parse.tempidcount
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function hSkipFromTo _
	( _
		byval x as integer, _
		byval fromtk as integer, _
		byval totk as integer, _
		byval delta as integer _
	) as integer

	dim as integer level = any

	assert( tkGet( x ) = fromtk )

	level = 0
	do
		x += delta

		assert( tkGet( x ) <> TK_EOF )

		select case( tkGet( x ) )
		case fromtk
			level += 1

		case totk
			if( level = 0 ) then
				exit do
			end if
			level -= 1

		end select
	loop

	function = x
end function

function cSkip( byval x as integer ) as integer
	do
		x += 1

		select case( tkGet( x ) )
		case TK_SPACE, TK_COMMENT, TK_EOL

		case TK_BEGIN
			x = hSkipFromTo( x, TK_BEGIN, TK_END, 1 )

		case else
			exit do
		end select
	loop

	function = x
end function

function cSkipRev( byval x as integer ) as integer
	do
		x -= 1

		select case( tkGet( x ) )
		case TK_SPACE, TK_COMMENT, TK_EOL

		case TK_END
			x = hSkipFromTo( x, TK_END, TK_BEGIN, -1 )

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
			x -= 1
			exit do

		case TK_AST, TK_DIVIDER
			x = cSkipRev( x )
			exit do

		end select
	loop

	function = x
end function

function cSkipStatement _
	( _
		byval x as integer, _
		byval is_enum as integer _
	) as integer

	var begin = x

	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		'' ';' (statement separator, though not in enums)
		case TK_SEMI
			if( is_enum = FALSE ) then
				x = cSkip( x )
				exit do
			end if

		'' ',' (enum constant separator)
		case TK_COMMA
			if( is_enum ) then
				x = cSkip( x )
				exit do
			end if

		'' '}': usually indicates end of statement, unless we're trying
		'' to skip a '}' itself
		case TK_RBRACE
			if( is_enum ) then
				exit do
			elseif( x > begin ) then
				exit do
			end if

		case TK_AST, TK_DIVIDER
			'' Reached high-level token after having seen normals?
			if( x > begin ) then
				exit do
			end if

		case TK_LPAREN, TK_LBRACKET, TK_LBRACE
			x = cFindClosingParen( x )
		end select

		x = cSkip( x )
	loop

	assert( x > begin )
	function = x
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type COPINFO
	level		as integer
	is_leftassoc	as integer
end type

'' C operator precedence (higher value = higher precedence)
dim shared as COPINFO copinfo(ASTCLASS_IIF to ASTCLASS_UNARYPLUS) = _
{ _
	( 2, FALSE), _ '' ASTCLASS_IIF
	( 3, TRUE ), _ '' ASTCLASS_LOGOR
	( 4, TRUE ), _ '' ASTCLASS_LOGAND
	( 5, TRUE ), _ '' ASTCLASS_BITOR
	( 6, TRUE ), _ '' ASTCLASS_BITXOR
	( 7, TRUE ), _ '' ASTCLASS_BITAND
	( 8, TRUE ), _ '' ASTCLASS_EQ
	( 8, TRUE ), _ '' ASTCLASS_NE
	( 9, TRUE ), _ '' ASTCLASS_LT
	( 9, TRUE ), _ '' ASTCLASS_LE
	( 9, TRUE ), _ '' ASTCLASS_GT
	( 9, TRUE ), _ '' ASTCLASS_GE
	(10, TRUE ), _ '' ASTCLASS_SHL
	(10, TRUE ), _ '' ASTCLASS_SHR
	(11, TRUE ), _ '' ASTCLASS_ADD
	(11, TRUE ), _ '' ASTCLASS_SUB
	(12, TRUE ), _ '' ASTCLASS_MUL
	(12, TRUE ), _ '' ASTCLASS_DIV
	(12, TRUE ), _ '' ASTCLASS_MOD
	(13, TRUE ), _ '' ASTCLASS_LOGNOT
	(13, TRUE ), _ '' ASTCLASS_BITNOT
	(13, TRUE ), _ '' ASTCLASS_NEGATE
	(13, TRUE )  _ '' ASTCLASS_UNARYPLUS
}

'' C expression parser based on precedence climbing
private function cExpression _
	( _
		byref x as integer, _
		byval level as integer = 0 _
	) as ASTNODE ptr

	function = NULL

	'' Unary prefix operators
	var astclass = -1
	select case( tkGet( x ) )
	case TK_EXCL   : astclass = ASTCLASS_LOGNOT    '' !
	case TK_TILDE  : astclass = ASTCLASS_BITNOT    '' ~
	case TK_MINUS  : astclass = ASTCLASS_NEGATE    '' -
	case TK_PLUS   : astclass = ASTCLASS_UNARYPLUS '' +
	end select

	dim as ASTNODE ptr a
	if( astclass >= 0 ) then
		x = cSkip( x )
		a = astNew( astclass, cExpression( x, copinfo(astclass).level ), NULL, NULL )
	else
		'' Atoms
		select case( tkGet( x ) )
		'' '(' Expression ')'
		case TK_LPAREN
			'' '('
			x = cSkip( x )

			'' Expression
			a = cExpression( x )
			if( a = NULL ) then
				exit function
			end if

			'' ')'
			if( tkGet( x ) <> TK_RPAREN ) then
				astDelete( a )
				exit function
			end if
			x = cSkip( x )

		case TK_OCTNUM, TK_DECNUM, TK_HEXNUM, TK_DECFLOAT
			a = hNumberLiteral( x )
			x = cSkip( x )

		'' DEFINED '(' Identifier ')'
		case KW_DEFINED
			x = cSkip( x )

			'' '('
			var have_parens = FALSE
			if( tkGet( x ) = TK_LPAREN ) then
				have_parens = TRUE
				x = cSkip( x )
			end if

			'' Identifier
			if( tkGet( x ) <> TK_ID ) then
				exit function
			end if
			a = astNew( ASTCLASS_ID, tkGetText( x ) )
			x = cSkip( x )

			if( have_parens ) then
				'' ')'
				if( tkGet( x ) <> TK_RPAREN ) then
					astDelete( a )
					exit function
				end if
				x = cSkip( x )
			end if

			a = astNew( ASTCLASS_DEFINED, a, NULL, NULL )

		case else
			exit function
		end select
	end if

	'' Infix operators
	do
		select case as const( tkGet( x ) )
		case TK_QUEST    : astclass = ASTCLASS_IIF    '' ? (a ? b : c)
		case TK_PIPEPIPE : astclass = ASTCLASS_LOGOR  '' ||
		case TK_AMPAMP   : astclass = ASTCLASS_LOGAND '' &&
		case TK_PIPE     : astclass = ASTCLASS_BITOR  '' |
		case TK_CIRC     : astclass = ASTCLASS_BITXOR '' ^
		case TK_AMP      : astclass = ASTCLASS_BITAND '' &
		case TK_EQEQ     : astclass = ASTCLASS_EQ     '' ==
		case TK_EXCLEQ   : astclass = ASTCLASS_NE     '' !=
		case TK_LT       : astclass = ASTCLASS_LT     '' <
		case TK_LTEQ     : astclass = ASTCLASS_LE     '' <=
		case TK_GT       : astclass = ASTCLASS_GT     '' >
		case TK_GTEQ     : astclass = ASTCLASS_GE     '' >=
		case TK_LTLT     : astclass = ASTCLASS_SHL    '' <<
		case TK_GTGT     : astclass = ASTCLASS_SHR    '' >>
		case TK_PLUS     : astclass = ASTCLASS_ADD    '' +
		case TK_MINUS    : astclass = ASTCLASS_SUB    '' -
		case TK_STAR     : astclass = ASTCLASS_MUL    '' *
		case TK_SLASH    : astclass = ASTCLASS_DIV    '' /
		case TK_PERCENT  : astclass = ASTCLASS_MOD    '' %
		case else        : exit do
		end select

		'' Higher/same level means process now (takes precedence),
		'' lower level means we're done and the parent call will
		'' continue. The first call will start with level 0.
		var oplevel = copinfo(astclass).level
		if( oplevel < level ) then
			exit do
		end if
		if( copinfo(astclass).is_leftassoc ) then
			oplevel += 1
		end if

		'' operator
		x = cSkip( x )

		'' rhs
		var b = cExpression( x, oplevel )
		if( b = NULL ) then
			astDelete( a )
			exit function
		end if

		'' Handle ?: special case
		dim as ASTNODE ptr c
		if( astclass = ASTCLASS_IIF ) then
			'' ':'?
			if( tkGet( x ) <> TK_COLON ) then
				astDelete( a )
				astDelete( b )
				exit function
			end if
			x = cSkip( x )

			c = cExpression( x, oplevel )
			if( c = NULL ) then
				astDelete( a )
				astDelete( b )
				exit function
			end if
		end if

		a = astNew( astclass, a, b, c )
	loop

	function = a
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function cSimpleToken( ) as ASTNODE ptr
	dim as ASTNODE ptr t = any
	dim as integer xbegin = any, xend = any

	select case( tkGet( parse.x ) )
	case TK_SEMI
		'' Cannot just return NULL, because we need to indicate success,
		'' so a NOP node is needed...
		t = astNew( ASTCLASS_NOP )
		parse.x = cSkip( parse.x )

	case TK_AST
		'' Any pre-existing high-level tokens (things transformed by
		'' previous parsing, such as PP directives, or anything inserted
		'' by presets) need to be recognized as "valid constructs" too.
		t = astClone( tkGetAst( parse.x ) )
		parse.x = cSkip( parse.x )

	case TK_DIVIDER
		'' (ditto)
		t = astNew( ASTCLASS_DIVIDER )
		parse.x = cSkip( parse.x )

	case else
		t = NULL
	end select

	function = t
end function

private function hMergeUnknown( ) as ASTNODE ptr
	dim as integer begin = any

	function = NULL

	if( (parse.pass = 1) or (tkIsPoisoned( parse.x ) = FALSE) ) then
		exit function
	end if

	begin = parse.x
	do
		parse.x += 1
	loop while( tkIsPoisoned( parse.x ) )

	function = astNew( ASTCLASS_UNKNOWN, _
			tkToAstText( begin, parse.x - 1 ), NULL, NULL )
end function

private sub cUnknown( byval is_enum as integer = FALSE )
	dim as integer begin = any

	'' This should only be called during the 1st pass
	assert( parse.pass = 1 )

	begin = parse.x
	parse.x = cSkipStatement( parse.x, is_enum )
	tkSetPoisoned( begin, parse.x - 1 )
end sub

'' (MultDecl{Field} | StructCompound)*
private function cStructBody( ) as ASTNODE ptr
	var group = astNew( ASTCLASS_GROUP )

	do
		select case( tkGet( parse.x ) )
		case TK_RBRACE, TK_EOF
			exit do
		end select

		var old = parse.x

		var t = hMergeUnknown( )
		if( t = NULL ) then
			parse.x = old
			t = cStructCompound( )
			if( t = NULL ) then
				parse.x = old
				t = cMultDecl( DECL_FIELD )
				if( t = NULL ) then
					parse.x = old
					t = cSimpleToken( )
					if( t = NULL ) then
						parse.x = old
						cUnknown( )
					end if
				end if
			end if
		end if

		if( t ) then
			astAddChild( group, t )
		end if
	loop

	function = group
end function

'' Identifier ['=' Expression] (',' | '}')
private function cEnumConst( ) as ASTNODE ptr
	'' Identifier
	if( tkGet( parse.x ) <> TK_ID ) then
		exit function
	end if
	var n = astNew( ASTCLASS_ENUMCONST, tkGetText( parse.x ) )
	parse.x = cSkip( parse.x )

	'' '='?
	if( tkGet( parse.x ) = TK_EQ ) then
		parse.x = cSkip( parse.x )

		var y = parse.x
		var expr = cExpression( y )
		if( expr = NULL ) then
			astDelete( n )
			exit function
		end if

		parse.x = y
		astAddChild( n, expr )
	end if

	'' (',' | '}')
	select case( tkGet( parse.x ) )
	case TK_COMMA
		parse.x = cSkip( parse.x )

	case TK_RBRACE

	case else
		astDelete( n )
		exit function
	end select

	function = n
end function

'' EnumConst (',' EnumConst)*
private function cEnumBody( ) as ASTNODE ptr
	var group = astNew( ASTCLASS_GROUP )

	do
		select case( tkGet( parse.x ) )
		case TK_RBRACE, TK_EOF
			exit do
		end select

		var old = parse.x

		var t = hMergeUnknown( )
		if( t = NULL ) then
			parse.x = old
			t = cEnumConst( )
			if( t = NULL ) then
				parse.x = old
				t = cSimpleToken( )
				if( t = NULL ) then
					parse.x = old
					cUnknown( TRUE )
				end if
			end if
		end if

		if( t ) then
			astAddChild( group, t )
		end if
	loop

	function = group
end function

'' [TYPEDEF] {STRUCT|UNION|ENUM} [Identifier] '{'
''     {StructBody|EnumBody}
'' '}' [MultDecl] ';'
private function cStructCompound( ) as ASTNODE ptr
	var head = parse.x
	var is_typedef = FALSE

	'' TYPEDEF?
	if( tkGet( parse.x ) = KW_TYPEDEF ) then
		parse.x = cSkip( parse.x )
		is_typedef = TRUE
	end if

	'' {STRUCT|UNION|ENUM}
	dim as integer astclass
	select case( tkGet( parse.x ) )
	case KW_STRUCT
		astclass = ASTCLASS_STRUCT
	case KW_UNION
		astclass = ASTCLASS_UNION
	case KW_ENUM
		astclass = ASTCLASS_ENUM
	case else
		exit function
	end select
	parse.x = cSkip( parse.x )

	'' [Identifier]
	dim as string id
	if( tkGet( parse.x ) = TK_ID ) then
		id = *tkGetText( parse.x )
		parse.x = cSkip( parse.x )
	elseif( is_typedef ) then
		'' If it's a typedef with anonymous struct block, we need to
		'' make up an id for it, for use in the base type of the
		'' typedef MultDecl. If it turns out to be just a single
		'' typedef, we can still solve it out later.
		id = hMakeTempId( )
	end if

	'' '{'
	if( tkGet( parse.x ) <> TK_LBRACE ) then
		exit function
	end if
	parse.x = cSkip( parse.x )

	var struct = astNew( astclass, id )
	astAddComment( struct, tkCollectComments( head, parse.x - 1 ) )

	if( astclass = ASTCLASS_ENUM ) then
		astAddChild( struct, cEnumBody( ) )
	else
		astAddChild( struct, cStructBody( ) )
	end if

	'' '}'
	if( tkGet( parse.x ) <> TK_RBRACE ) then
		astDelete( struct )
		exit function
	end if
	parse.x = cSkip( parse.x )

	if( is_typedef ) then
		var subtype = astNew( ASTCLASS_ID, id )
		var t = cIdList( DECL_TYPEDEF, TYPE_UDT, subtype )
		astDelete( subtype )

		if( t = NULL ) then
			astDelete( struct )
			exit function
		end if

		function = astNew( ASTCLASS_GROUP, struct, t, NULL )
	else
		'' ';'
		if( tkGet( parse.x ) <> TK_SEMI ) then
			astDelete( struct )
			exit function
		end if
		parse.x = cSkip( parse.x )

		function = struct
	end if
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

''
'' Declaration base type parsing
''
'' The base type is the data type part of a variable/procedure/typedef/parameter
'' declaration that is at the front, in front of the identifier list.
'' '*' chars indicating pointers belong to the identifier, not the type.
''
''    int a, b, c;
''    ^^^
''
''    struct UDT const *p, **pp;
''    ^^^^^^^^^^^^^^^^
''
'' Besides the base type there can be modifiers such as "signed", "unsigned",
'' "const", "short", "long". They can be used together with some base types,
'' for example "short int a;", or alone: "short a;". Modifiers can appear in
'' front of the base type or behind it, in any order. Some modifiers are
'' incompatible to each-other, such as "signed" and "unsigned", or "short" and
'' "long". There may only be 1 "short", and only 1 or 2 "long"s.
''
''    short int a;
''    unsigned a;
''    const int unsigned a;
''    long const a;
''    long long int a;
''    const const unsigned long const long const int const unsigned a;
''
'' Returns TRUE/FALSE to indicate success/failure. The type is returned through
'' the byref parameters.
''
private function cBaseType _
	( _
		byref dtype as integer, _
		byref subtype as ASTNODE ptr, _
		byval decl as integer _
	) as integer

	dim as integer sign = any, signedmods = any, unsignedmods = any
	dim as integer constmods = any, shortmods = any, longmods = any
	dim as integer basetypex = any, basetypetk = any

	dtype = TYPE_NONE
	subtype = NULL
	function = FALSE

	signedmods = 0
	unsignedmods = 0
	constmods = 0
	shortmods = 0
	longmods = 0
	basetypex = -1
	basetypetk = -1

	''
	'' 1. Parse base type and all modifiers, and count them
	''

	do
		select case as const( tkGet( parse.x ) )
		case KW_SIGNED
			signedmods += 1

		case KW_UNSIGNED
			unsignedmods += 1

		case KW_CONST
			constmods += 1

		case KW_SHORT
			shortmods += 1

		case KW_LONG
			longmods += 1

		case else
			'' Only one base type is allowed
			if( basetypex >= 0 ) then
				exit do
			end if

			select case as const( tkGet( parse.x ) )
			case KW_ENUM, KW_STRUCT, KW_UNION
				'' {ENUM|STRUCT|UNION}
				parse.x = cSkip( parse.x )

				'' Identifier
				if( tkGet( parse.x ) <> TK_ID ) then
					exit function
				end if
				basetypex = parse.x

			case TK_ID
				''
				'' Disambiguation needed:
				''    signed foo;       // foo = var id
				''    signed foo, bar;  // foo = var id, bar = var id
				''    signed foo(void); // foo = function id
				''    signed foo[1];    // foo = array id
				'' vs.
				''    signed foo bar;   // foo = typedef, bar = var id
				''    signed foo *bar;  // ditto
				''    signed foo (*bar)(void);  // ditto
				''    signed foo const bar;  // ditto
				''
				'' Checking for this is only needed if there
				'' already were tokens that belong to the type
				'' in front of the TK_ID, e.g. "signed" or
				'' "long", but not "const" which cannot be given
				'' alone in place of a type.
				''
				'' If a type is expected, and a TK_ID appears
				'' as first token, then it just must be part of
				'' the type, afterall something like
				''    foo;
				'' isn't allowed; it has to be at least
				''    mytype foo;
				''
				'' For parameters the identifier can be omitted
				'' optionally, disambiguation is impossible
				'' based on syntax only:
				''    void f(unsigned myint);
				'' vs.
				''    typedef int myint;
				''    void f(unsigned myint);
				'' To be safe, we should always assume it's the
				'' identifier
				''

				'' Already saw modifiers that themselves would
				'' be enough to form the type?
				if( signedmods or unsignedmods or _
				    longmods or shortmods ) then
					select case( tkGet( cSkip( parse.x ) ) )
					case TK_ID
						'' Another id must follow for params,
						'' otherwise it's ambigious
						if( decl = DECL_PARAM ) then
							exit function
						end if

					case TK_SEMI, TK_COMMA, TK_LBRACKET
						exit do

					case TK_LPAREN
						if( tkGet( cSkip( cSkip( parse.x ) ) ) <> TK_STAR ) then
							exit do
						end if
					end select
				end if

				'' Treat the TK_ID as the type (a typedef)
				basetypex = parse.x

			case KW_VOID, KW_CHAR, KW_FLOAT, KW_DOUBLE, KW_INT
				basetypex = parse.x

			case else
				exit do
			end select
		end select

		parse.x = cSkip( parse.x )
	loop

	''
	'' 2. Refuse invalid modifier combinations etc.
	''

	if( basetypex >= 0 ) then
		basetypetk = tkGet( basetypex )
	end if

	'' Can't have both SIGNED and UNSIGNED
	if( (signedmods > 0) and (unsignedmods > 0) ) then
		exit function
	end if

	'' Neither both SHORT and LONG
	if( (shortmods > 0) and (longmods > 0) ) then
		exit function
	end if

	'' Max. 1 SHORT allowed, and 1 or 2 LONGs
	if( (shortmods > 1) or (longmods > 2) ) then
		exit function
	end if

	select case( basetypetk )
	case TK_ID, KW_VOID, KW_FLOAT, KW_DOUBLE
		'' No SIGNED|UNSIGNED|SHORT|LONG for UDTs/floats/void
		'' (cannot be translated to FB)
		if( signedmods or unsignedmods or shortmods or longmods ) then
			exit function
		end if

		select case( basetypetk )
		case TK_ID
			select case( *tkGetText( basetypex ) )
			case "size_t"
				dtype = TYPE_UINTEGER
			case "ssize_t"
				dtype = TYPE_INTEGER
			case "int8_t", "__int8"
				dtype = TYPE_BYTE
			case "uint8_t"
				dtype = TYPE_UBYTE
			case "int16_t", "__int16"
				dtype = TYPE_SHORT
			case "uint16_t"
				dtype = TYPE_USHORT
			case "int32_t", "__int32"
				dtype = TYPE_LONG
			case "uint32_t"
				dtype = TYPE_ULONG
			case "int64_t", "__int64"
				dtype = TYPE_LONGINT
			case "uint64_t"
				dtype = TYPE_ULONGINT
			case else
				dtype = TYPE_UDT
				subtype = astNew( ASTCLASS_ID, tkGetText( basetypex ) )
			end select
		case KW_VOID
			dtype = TYPE_ANY
		case KW_FLOAT
			dtype = TYPE_SINGLE
		case KW_DOUBLE
			dtype = TYPE_DOUBLE
		case else
			assert( FALSE )
		end select

	case KW_CHAR
		'' No SHORT|LONG CHAR allowed
		if( shortmods or longmods ) then
			exit function
		end if

		'' SIGNED|UNSIGNED CHAR becomes BYTE|UBYTE,
		'' but plain CHAR probably means ZSTRING
		if( signedmods > 0 ) then
			dtype = TYPE_BYTE
		elseif( unsignedmods > 0 ) then
			dtype = TYPE_UBYTE
		else
			dtype = TYPE_ZSTRING
		end if

	case else
		'' Base type is "int" (either explicitly given, or implied
		'' because no other base type was given). Any modifiers are
		'' just added on top of that.
		if( shortmods = 1 ) then
			dtype = iif( unsignedmods > 0, TYPE_USHORT, TYPE_SHORT )
		elseif( longmods = 1 ) then
			'' TODO: How to handle translation of longs (32bit vs. 64bit)?
			exit function
		elseif( longmods = 2 ) then
			dtype = iif( unsignedmods > 0, TYPE_ULONGINT, TYPE_LONGINT )
		elseif( basetypetk = KW_INT ) then
			'' Explicit "int" base type and no modifiers
			dtype = iif( unsignedmods > 0, TYPE_ULONG, TYPE_LONG )
		elseif( unsignedmods > 0 ) then
			'' UNSIGNED only
			dtype = TYPE_ULONG
		elseif( signedmods > 0 ) then
			'' SIGNED only
			dtype = TYPE_LONG
		else
			'' No modifiers and no explicit "int" either
			exit function
		end if

	end select

	'' Any CONSTs on the base type are merged into one
	''    const int a;
	''    const int const a;
	''          int const a;
	''    const const int const const a;
	'' It's all the same...
	if( constmods > 0 ) then
		dtype = typeSetIsConst( dtype )
	end if

	function = TRUE
end function

'' ParamDecl = '...' | MultDecl{Param}
private function cParamDecl( ) as ASTNODE ptr
	dim as ASTNODE ptr t = any

	'' '...'?
	if( tkGet( parse.x ) = TK_ELLIPSIS ) then
		t = astNew( ASTCLASS_PARAM )
		astAddComment( t, tkCollectComments( parse.x, cSkip( parse.x ) - 1 ) )
		parse.x = cSkip( parse.x )
	else
		t = cMultDecl( DECL_PARAM )
	end if

	function = t
end function

'' ParamDeclList = ParamDecl (',' ParamDecl)*
private function cParamDeclList( ) as ASTNODE ptr
	var group = astNew( ASTCLASS_GROUP )

	do
		var t = cParamDecl( )
		if( t = NULL ) then
			astDelete( group )
			exit function
		end if

		astAddChild( group, t )

		'' ','?
		if( tkGet( parse.x ) <> TK_COMMA ) then
			exit do
		end if
		parse.x = cSkip( parse.x )
	loop

	function = group
end function

''
'' Declarator =
''    '*'*
''    { [Identifier] | '(' Declarator ')' }
''    { '(' ParamList ')' | ('[' ArrayElements ']')* }
''    [ '=' Initializer ]
''
'' This needs to parse things like:
''    i            for example, as part of: int i;
''    i[10]        array: int i[10];
''    <nothing>    anonymous parameter: int f(int);
''    ()           extra parentheses on anonymous parameter: int f(int ());
''    ***p         int ***p;
''    (*p)(void)   function pointer: void (*p)(void);
''    (((i)))      extra parentheses around identifier: int (((i)));
''    *(*(pp))     ditto
''    (*f(void))(void)    function returning a function pointer:
''                            void (*f(void))(void);
''    (*p[10])(void)      array of function pointers: void (*p[10])(void);
''
'' Example 1:
''
''         int (*f)(int a);
'' depth 1:     ^^
''    innerprocptrdtype = TYPE_PROC (unused)
''    procptrdtype      = typeAddrOf( TYPE_PROC )
''    innernode = NULL
''    node = NULL
''    t = VAR( f as int ptr )    (f as int ptr for now, in case it's just "int (*f);")
''
''         int (*f)(int a);
'' depth 0:    ^  ^^^^^^^^
''    innerprocptrdtype = typeAddrOf( TYPE_PROC )      (passed up from depth 1)
''    procptrdtype      = TYPE_PROC                    (unused)
''    innernode = VAR( f as int ptr )                  (passed up from depth 1)
''    node      = PROC( function( a as int ) as int )  (new function pointer subtype)
''    t = VAR( f as function( a as int ) as int )      (adjusted AST: f turned into function pointer)
''
'' Example 2:
''
''         int (*(*f)(int a))(int b);
'' depth 2:       ^^
''    innerprocptrdtype = TYPE_PROC                    (unused)
''    procptrdtype      = typeAddrOf( TYPE_PROC )
''    innernode = NULL
''    node      = NULL
''    t = VAR( f as int ptr ptr )    (f as int ptr ptr for now, in case it's just "int (*(*f));")
''
''         int (*(*f)(int a))(int b);
'' depth 1:     ^^  ^^^^^^^^
''    innerprocptrdtype = typeAddrOf( TYPE_PROC )      (passed up from depth 2)
''    procptrdtype      = typeAddrOf( TYPE_PROC )
''    innernode = VAR( f as int ptr ptr )              (passed up from depth 2)
''    node      = PROC( function( a as int ) as int ptr )  (new function pointer subtype,
''                                                     result = int ptr for now, in case it's
''                                                     just "int (*(*f)(int a));")
''    t = VAR( f as function( a as int ) as int ptr )  (adjusted AST: f turned into function pointer)
''
''         int (*(*f)(int a))(int b);
'' depth 0:    ^            ^^^^^^^^
''    innerprocptrdtype = typeAddrOf( TYPE_PROC )          (passed up from depth 1)
''    procptrdtype      = TYPE_PROC (unused)
''    innernode = PROC( function( a as int ) as int ptr )  (passed up from depth 1)
''    node      = PROC( function( b as int ) as int )      (new function pointer subtype)
''    t = VAR( f as function( a as int ) as function( b as int ) as int )
''                               (adjusted AST: f's subtype (innernode) turned into function pointer,
''                               i.e. the f function pointer now has a function pointer as its function
''                               result type, instead of "int ptr")
''
private function cDeclarator _
	( _
		byval decl as integer, _
		byval basedtype as integer, _
		byval basesubtype as ASTNODE ptr, _
		byref node as ASTNODE ptr, _
		byref procptrdtype as integer _
	) as ASTNODE ptr

	var begin = parse.x
	var dtype = basedtype
	var innerprocptrdtype = TYPE_PROC
	procptrdtype = TYPE_PROC

	'' Pointers: ('*')*
	while( tkGet( parse.x ) = TK_STAR )
		procptrdtype = typeAddrOf( procptrdtype )
		dtype = typeAddrOf( dtype )
		parse.x = cSkip( parse.x )

		'' (CONST)*
		while( tkGet( parse.x ) = KW_CONST )
			procptrdtype = typeSetIsConst( procptrdtype )
			dtype = typeSetIsConst( dtype )
			parse.x = cSkip( parse.x )
		wend
	wend

	''    '(' Declarator ')'    |    [Identifier]
	dim as ASTNODE ptr t, innernode
	if( tkGet( parse.x ) = TK_LPAREN ) then
		'' '('
		parse.x = cSkip( parse.x )

		t = cDeclarator( decl, dtype, basesubtype, innernode, innerprocptrdtype )
		if( t = NULL ) then
			exit function
		end if

		'' ')'
		if( tkGet( parse.x ) <> TK_RPAREN ) then
			astDelete( t )
			exit function
		end if
		parse.x = cSkip( parse.x )
	else
		'' [Identifier]
		dim as string id
		if( tkGet( parse.x ) = TK_ID ) then
			id = *tkGetText( parse.x )
			parse.x = cSkip( parse.x )
		else
			'' An identifier must exist, except for parameters
			if( decl <> DECL_PARAM ) then
				exit function
			end if
		end if

		dim as integer astclass
		select case( decl )
		case DECL_VAR, DECL_EXTERNVAR, DECL_STATICVAR
			astclass = ASTCLASS_VAR
		case DECL_FIELD
			astclass = ASTCLASS_FIELD
		case DECL_PARAM
			astclass = ASTCLASS_PARAM
		case DECL_TYPEDEF
			astclass = ASTCLASS_TYPEDEF
		case else
			assert( FALSE )
		end select

		t = astNew( astclass, id )

		select case( decl )
		case DECL_EXTERNVAR
			t->attrib or= ASTATTRIB_EXTERN
		case DECL_STATICVAR
			t->attrib or= ASTATTRIB_STATIC
		end select

		astSetType( t, dtype, basesubtype )
		astAddComment( t, tkCollectComments( begin, parse.x - 1 ) )
	end if

	node = t

	select case( tkGet( parse.x ) )
	'' ('[' ArrayElements ']')*
	case TK_LBRACKET
		'' Can't allow arrays on everything - currently, it's only
		'' handled for vars/fields/params
		select case( decl )
		case DECL_VAR, DECL_EXTERNVAR, DECL_STATICVAR, _
		     DECL_FIELD, DECL_PARAM

		case else
			astDelete( t )
			node = NULL
			exit function
		end select

		assert( node->array = NULL )
		node->array = astNew( ASTCLASS_ARRAY )

		do
			parse.x = cSkip( parse.x )

			var dimension = astNew( ASTCLASS_DIMENSION )
			astAddChild( node->array, dimension )

			var y = parse.x
			var elements = cExpression( y )
			if( elements = NULL ) then
				astDelete( t )
				node = NULL
				exit function
			end if
			parse.x = y

			'' lbound = 0, ubound = elements - 1
			astAddChild( dimension, astNewCONST( 0, 0, TYPE_LONG ) )
			astAddChild( dimension, _
				astNew( ASTCLASS_SUB, _
					elements, _
					astNewCONST( 1, 0, TYPE_LONG ), _
					NULL ) )

			'' ']'
			if( tkGet( parse.x ) <> TK_RBRACKET ) then
				astDelete( t )
				node = NULL
				exit function
			end if
			parse.x = cSkip( parse.x )
		loop while( tkGet( parse.x ) = TK_LBRACKET )

	'' '(' ParamList ')'
	case TK_LPAREN
		parse.x = cSkip( parse.x )

		'' Parameters turn a vardecl/fielddecl into a procdecl,
		'' unless they're for a procptr type.
		if( innerprocptrdtype <> TYPE_PROC ) then
			'' There were '()'s above and the recursive
			'' cDeclarator() call found pointers/CONSTs,
			'' these parameters are for a function pointer.
			''
			'' Whichever object should become the function pointer,
			'' its dtype/subtype must be adjusted accordingly.
			'' For the subtype, a new PROC node is created, which
			'' will hold the parameters etc. found at this level.

			'' New PROC node for the function pointer's subtype
			node = astNew( ASTCLASS_PROC )
			astSetType( node, dtype, basesubtype )

			'' Turn the object into a function pointer
			astDelete( innernode->subtype )
			innernode->dtype = innerprocptrdtype
			innernode->subtype = node
		else
			'' A plain symbol, not a pointer, becomes a function
			select case( t->class )
			case ASTCLASS_VAR, ASTCLASS_FIELD
				t->class = ASTCLASS_PROC
			end select
		end if

		'' Just '(void)'?
		if( (tkGet( parse.x ) = KW_VOID) and (tkGet( cSkip( parse.x ) ) = TK_RPAREN) ) then
			'' VOID
			parse.x = cSkip( parse.x )
		'' Not just '()'?
		elseif( tkGet( parse.x ) <> TK_RPAREN ) then
			var params = cParamDeclList( )
			if( params = NULL ) then
				astDelete( t )
				node = NULL
				exit function
			end if
			astAddChild( node, params )
		end if

		'' ')'
		if( tkGet( parse.x ) <> TK_RPAREN ) then
			astDelete( t )
			node = NULL
			exit function
		end if
		parse.x = cSkip( parse.x )
	end select

	'' ['=' Initializer]
	select case( decl )
	case DECL_PARAM
		if( tkGet( parse.x ) = TK_EQ ) then
			parse.x = cSkip( parse.x )

			var y = parse.x
			var expr = cExpression( y )
			if( expr = NULL ) then
				astDelete( t )
				node = NULL
				exit function
			end if
			parse.x = y

			assert( node->initializer = NULL )
			node->initializer = expr
		end if
	end select

	function = t
end function

'' IdList = Declarator (',' Declarator)* [';']
private function cIdList _
	( _
		byval decl as integer, _
		byval basedtype as integer, _
		byval basesubtype as ASTNODE ptr _
	) as ASTNODE ptr

	var group = astNew( ASTCLASS_GROUP )

	'' ... (',' ...)*
	do
		var t = cDeclarator( decl, basedtype, basesubtype, NULL, 0 )
		if( t = NULL ) then
			astDelete( group )
			exit function
		end if

		astAddChild( group, t )

		'' Everything can have a comma and more identifiers,
		'' except for parameters.
		if( decl = DECL_PARAM ) then
			exit do
		end if

		'' ','?
		if( tkGet( parse.x ) <> TK_COMMA ) then
			exit do
		end if
		parse.x = cSkip( parse.x )
	loop

	'' Everything except parameters must end with a ';'
	if( decl <> DECL_PARAM ) then
		'' ';'
		if( tkGet( parse.x ) <> TK_SEMI ) then
			astDelete( group )
			exit function
		end if
		parse.x = cSkip( parse.x )
	end if

	function = group
end function

''
'' Generic 'type *a, **b;' parsing, used for vars/fields/protos/params/typedefs
'' ("multiple declaration" syntax)
''    int i;
''    int a, b, c;
''    int *a, ***b, c;
''    int f(void);
''    int (*procptr)(void);
''
'' MultDecl = BaseType IdList
''
private function cMultDecl( byval decl as integer ) as ASTNODE ptr
	dim as integer dtype = any, typebegin = any, typeend = any
	dim as ASTNODE ptr subtype = any

	function = NULL

	'' BaseType
	typebegin = parse.x
	if( cBaseType( dtype, subtype, decl ) = FALSE ) then
		exit function
	end if
	typeend = parse.x

	function = cIdList( decl, dtype, subtype )
	astDelete( subtype )
end function

'' Global variable/procedure declarations
''    [EXTERN|STATIC] MultDecl
private function cGlobalDecl( ) as ASTNODE ptr
	dim as integer decl = any

	select case( tkGet( parse.x ) )
	case KW_EXTERN, KW_STATIC
		if( tkGet( parse.x ) = KW_EXTERN ) then
			decl = DECL_EXTERNVAR
		else
			decl = DECL_STATICVAR
		end if
		parse.x = cSkip( parse.x )
	case else
		decl = DECL_VAR
	end select

	function = cMultDecl( decl )
end function

'' Typedefs
''    TYPEDEF MultDecl
private function cTypedef( ) as ASTNODE ptr
	'' TYPEDEF?
	if( tkGet( parse.x ) <> KW_TYPEDEF ) then
		exit function
	end if
	parse.x = cSkip( parse.x )

	function = cMultDecl( DECL_TYPEDEF )
end function

'' Struct forward declarations
''    STRUCT Identifier ';'
private function cStructForward( ) as ASTNODE ptr
	'' STRUCT?
	if( tkGet( parse.x ) <> KW_STRUCT ) then
		exit function
	end if
	parse.x = cSkip( parse.x )

	'' Identifier?
	if( tkGet( parse.x ) <> TK_ID ) then
		exit function
	end if
	var id = *tkGetText( parse.x )
	parse.x = cSkip( parse.x )

	'' ';'?
	if( tkGet( parse.x ) <> TK_SEMI ) then
		exit function
	end if
	parse.x = cSkip( parse.x )

	function = astNew( ASTCLASS_STRUCTFWD, id )
end function

private function hToplevel( ) as ASTNODE ptr
	var group = astNew( ASTCLASS_GROUP )

	parse.x = cSkip( -1 )

	while( tkGet( parse.x ) <> TK_EOF )
		var old = parse.x

		var t = hMergeUnknown( )
		if( t = NULL ) then
			parse.x = old
			t = cStructCompound( )
			if( t = NULL ) then
				parse.x = old
				t = cGlobalDecl( )
				if( t = NULL ) then
					parse.x = old
					t = cTypedef( )
					if( t = NULL ) then
						parse.x = old
						t = cStructForward( )
						if( t = NULL ) then
							parse.x = old
							t = cSimpleToken( )
							if( t = NULL ) then
								parse.x = old
								cUnknown( )
							end if
						end if
					end if
				end if
			end if
		end if

		if( t ) then
			astAddChild( group, t )
		end if
	wend

	function = group
end function

function cToplevel( ) as ASTNODE ptr
	'' 1st pass to identify constructs & set marks correspondingly
	parse.pass = 1
	parse.tempidcount = 0
	astDelete( hToplevel( ) )

	'' 2nd pass to build up AST
	parse.pass = 2
	parse.tempidcount = 0
	function = hToplevel( )
end function

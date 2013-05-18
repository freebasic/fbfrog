'' C parsing passes

#include once "fbfrog.bi"

type PARSERSTUFF
	tempidcount	as integer
end type

dim shared as PARSERSTUFF parser

enum
	DECL_VAR = 0
	DECL_EXTERNVAR
	DECL_STATICVAR
	DECL_FIELD
	DECL_PARAM
	DECL_TYPEDEF
end enum

declare function cStructCompound _
	( _
		byval x as integer, _
		byval parent as ASTNODE ptr _
	) as integer
declare function cIdList _
	( _
		byval x as integer, _
		byval decl as integer, _
		byval basedtype as integer, _
		byval basesubtype as ASTNODE ptr, _
		byval parent as ASTNODE ptr _
	) as integer
declare function cMultDecl _
	( _
		byval x as integer, _
		byval decl as integer, _
		byval parent as ASTNODE ptr _
	) as integer

private function hMakeTempId( ) as string
	parser.tempidcount += 1
	function = "__fbfrog_AnonStruct" & parser.tempidcount
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

private function ppSkip( byval x as integer ) as integer
	dim as integer y = any

	do
		x += 1

		select case( tkGet( x ) )
		case TK_SPACE, TK_COMMENT

		case TK_BEGIN
			x = hSkipFromTo( x, TK_BEGIN, TK_END, 1 )

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

private function ppSkipToEOL( byval x as integer ) as integer
	do
		select case( tkGet( x ) )
		case TK_EOL, TK_EOF
			exit do
		end select

		x = ppSkip( x )
	loop

	function = x
end function

private function cSkip( byval x as integer ) as integer
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

private function cSkipRev( byval x as integer ) as integer
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
			exit do

		case TK_AST, TK_DIVIDER
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

		case TK_AST, TK_DIVIDER
			exit do

		case else
			x = cSkip( x )
		end select
	loop

	function = x
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function hCollectComments _
	( _
		byval first as integer, _
		byval last as integer _
	) as string

	dim as string s
	dim as zstring ptr text = any

	'' Collect all comment text from a range of tokens and merge it into
	'' one string, which can be used
	for i as integer = first to last
		if( tkGet( i ) = TK_COMMENT ) then
			text = tkGetText( i )
		else
			text = tkGetComment( i )
		end if

		if( text ) then
			if( len( s ) > 0 ) then
				s += !"\n"
			end if
			s += *text
		end if
	next

	function = s
end function

private function hIsBeforeEol _
	( _
		byval x as integer, _
		byval delta as integer _
	) as integer

	function = TRUE

	'' Can we reach EOL before hitting any non-space token?
	do
		x += delta

		select case( tkGet( x ) )
		case TK_SPACE, TK_COMMENT

		case TK_EOL, TK_EOF
			exit do

		case TK_AST, TK_DIVIDER
			'' High-level tokens count as separate lines
			exit do

		case else
			function = FALSE
			exit do
		end select
	loop

end function

private sub hAccumComment( byval x as integer, byref comment as string )
	dim as zstring ptr s = any
	dim as string text

	if( len( comment ) = 0 ) then
		exit sub
	end if

	s = tkGetComment( x )
	if( s ) then
		text = *s + !"\n"
	end if

	text += comment

	tkSetComment( x, text )
end sub

private sub hAccumTkComment( byval x as integer, byval comment as integer )
	assert( tkGet( comment ) = TK_COMMENT )
	hAccumComment( x, *tkGetText( comment ) )
end sub

sub cAssignComments( )
	dim as integer x = any, y = any, at_bol = any, at_eol = any

	x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_COMMENT
			''
			'' int A; //FOO    -> assign FOO to ';', so it can be
			''                    picked up by the A vardecl
			''
			''  /*FOO*/ int A; -> assign FOO to 'int', ditto
			''
			'' //FOO           -> assign FOO to EOL, so it can be
			'' <empty line>       picked up by a TK_DIVIDER
			''
			'' //FOO           -> assign FOO to EOL, ditto
			'' int A;
			'' <empty line>
			''
			'' //FOO           -> comment belongs to both A and B,
			'' int A;             assign to EOL for a TK_DIVIDER
			'' int B;
			''
			'' int /*FOO*/ A;  -> assign FOO to 'int'
			''
			'' int             -> assign FOO to EOL
			'' //FOO
			'' A;

			at_bol = hIsBeforeEol( x, -1 )
			at_eol = hIsBeforeEol( x,  1 )

			if( at_bol and at_eol ) then
				'' Comment above empty line?
				if( tkCount( TK_EOL, x + 1, cSkip( x ) ) >= 2 ) then
					hAccumTkComment( tkSkipSpaceAndComments( x ), x )
				else
					'' Comment above multiple statements,
					'' that aren't separated by empty lines?
					y = cSkipStatement( x )
					if( (y < cSkipStatement( y )) and _
					    (tkCount( TK_EOL, cSkipRev( y ) + 1, y - 1 ) < 2) ) then
						hAccumTkComment( tkSkipSpaceAndComments( x ), x )
					else
						'' Comment above single statement
						hAccumTkComment( cSkip( x ), x )
					end if
				end if
			elseif( at_bol ) then
				hAccumTkComment( tkSkipSpaceAndComments( x ), x )
			elseif( at_eol ) then
				hAccumTkComment( tkSkipSpaceAndComments( x, -1 ), x )
			else
				hAccumTkComment( tkSkipSpaceAndComments( x ), x )
			end if

			tkRemove( x, x )
			x -= 1

		end select

		x += 1
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function ppDirective( byval x as integer ) as integer
	dim as integer begin = any, tk = any, y = any
	dim as ASTNODE ptr expr = any

	'' not at BOL?
	y = tkSkipSpaceAndComments( x, -1 )
	select case( tkGet( y ) )
	case TK_EOL, TK_EOF
		return -1
	end select

	'' '#'
	if( tkGet( x ) <> TK_HASH ) then
		return -1
	end if
	x = ppSkip( x )

	begin = x
	expr = NULL

	tk = tkGet( x )
	select case( tk )
	'' DEFINE Identifier ['(' ParameterList ')'] Body Eol .
	case KW_DEFINE
		'' DEFINE
		x = ppSkip( x )

		'' Identifier?
		if( tkGet( x ) <> TK_ID ) then
			return -1
		end if
		expr = astNewPPDEFINE( tkGetText( x ) )
		x = ppSkip( x )

		tkRemove( begin, x - 1 )
		tkInsert( begin, TK_AST, , expr )
		begin += 1
		x = begin
		expr = NULL

		'' Parse body tokens, if any, and wrap them inside a BEGIN/END
		select case( tkGet( x ) )
		case TK_EOL, TK_EOF

		case else
			tkInsert( x, TK_BEGIN )
			x += 1

			x = ppSkipToEOL( x )

			tkInsert( x, TK_END )
			x += 1
		end select


''		'' If it's just a number literal, use a proper CONST expression
''		select case( tkGet( x ) )
''		case TK_DECNUM, TK_HEXNUM, TK_OCTNUM
''			select case( tkGet( ppSkip( x ) ) )
''			case TK_EOL, TK_EOF
''				select case( tkGet( x ) )
''				case TK_DECNUM
''					expr = astNewCONSTi( vallng( *tkGetText( x ) ) )
''				case TK_HEXNUM
''					expr = astNewCONSTi( vallng( "&h" + *tkGetText( x ) ) )
''				case TK_OCTNUM
''					expr = astNewCONSTi( vallng( "&o" + *tkGetText( x ) ) )
''				end select
''			end select
''		end select
''
''		'' Otherwise, if there are any tokens, fall back to a TEXT
''		'' expression, that may have to be translated manually
''		if( expr = NULL ) then
''			select case( tkGet( x ) )
''			case TK_EOL, TK_EOF
''
''			case else
''				y = ppSkipToEOL( x )
''				expr = astNewTEXT( tkToText( x, y ) )
''				x = y
''			end select
''		end if
''
''		expr = astNewPPDEFINE( text, expr )

	case KW_INCLUDE
		'' INCLUDE
		x = ppSkip( x )

		'' "..."
		if( tkGet( x ) <> TK_STRING ) then
			return -1
		end if
		expr = astNewPPINCLUDE( tkGetText( x ) )
		x = ppSkip( x )

	case KW_IFDEF, KW_IFNDEF
		x = ppSkip( x )

		'' Identifier?
		if( tkGet( x ) <> TK_ID ) then
			return -1
		end if
		expr = astNewID( tkGetText( x ) )
		x = ppSkip( x )

		expr = astNewDEFINED( expr )
		if( tk = KW_IFNDEF ) then
			expr = astNewLOGICNOT( expr )
		end if
		expr = astNewPPIF( expr )

	case KW_ELSE, KW_ENDIF
		x = ppSkip( x )

		if( tk = KW_ELSE ) then
			expr = astNewPPELSE( )
		else
			expr = astNewPPENDIF( )
		end if

	case else
		return -1
	end select

	'' EOL?
	select case( tkGet( x ) )
	case TK_EOL
		x = ppSkip( x )

	case TK_EOF

	case else
		return -1
	end select

	if( expr ) then
		tkRemove( begin, x - 1 )
		tkInsert( begin, TK_AST, , expr )
		begin += 1
		x = begin
	end if

	function = x
end function

private function ppUnknownDirective( byval x as integer ) as integer
	dim as integer begin = any, y = any
	dim as ASTNODE ptr expr = any

	begin = x

	'' not at BOL?
	y = tkSkipSpaceAndComments( x, -1 )
	select case( tkGet( y ) )
	case TK_EOL, TK_EOF
		return -1
	end select

	'' '#'
	if( tkGet( x ) <> TK_HASH ) then
		return -1
	end if
	x = ppSkip( x )

	y = ppSkipToEOL( x )
	expr = astNewPPUNKNOWN( tkToText( x, y ) )
	x = y

	'' EOL? (could also be EOF)
	if( tkGet( x ) = TK_EOL ) then
		x = ppSkip( x )
	end if

	tkRemove( begin, x - 1 )
	tkInsert( begin, TK_AST, , expr )
	begin += 1
	x = begin

	function = x
end function

'' Merge empty lines into TK_DIVIDER. We can assume to start at BOL,
'' as cPPDirectives() effectively parses one line after another.
private function ppDivider( byval x as integer ) as integer
	dim as integer lines = any, begin = any, eol1 = any, eol2 = any
	dim as string comment, blockcomment

	begin = x

	'' Count empty lines in a row
	lines = 0
	do
		select case( tkGet( x ) )
		case TK_EOL
			lines += 1

		case TK_SPACE

		case else
			exit do
		end select

		x += 1
	loop

	if( lines < 1 ) then
		return -1
	end if

	''  ...code...
	''
	''  //foo
	''
	''  //bar
	''  ...code...
	''
	'' "foo" is the comment associated with TK_DIVIDER, "bar" the one
	'' associated with the following block of code, stored as TK_DIVIDER's
	'' text.

	eol2 = tkSkipSpaceAndComments( x, -1 )
	eol1 = tkSkipSpaceAndComments( eol2 - 1, -1 )
	blockcomment = hCollectComments( eol1 + 1, eol2 )

	comment = hCollectComments( begin, eol1 )
	tkRemove( begin, x - 1 )
	tkInsert( begin, TK_DIVIDER, blockcomment )
	tkSetComment( begin, comment )
	x = cSkip( begin )

	function = x
end function

sub cPPDirectives( )
	dim as integer x = any, old = any

	x = ppSkip( -1 )
	while( tkGet( x ) <> TK_EOF )
		old = x

		x = ppDirective( old )
		if( x >= 0 ) then
			continue while
		end if

		x = ppUnknownDirective( old )
		if( x >= 0 ) then
			continue while
		end if

		x = ppDivider( old )
		if( x >= 0 ) then
			continue while
		end if

		'' Skip to next line
		x = ppSkip( ppSkipToEOL( old ) )
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function cSimpleToken _
	( _
		byval x as integer, _
		byval parent as ASTNODE ptr _
	) as integer

	select case( tkGet( x ) )
	case TK_SEMI
		x = cSkip( x )

	case TK_AST
		'' Any pre-existing high-level tokens (things transformed by
		'' previous parsing, such as PP directives, or anything inserted
		'' by presets) need to be recognized as "valid constructs" too.
		if( parent ) then
			astAddChild( parent, astClone( tkGetAst( x ) ) )
		end if
		x = cSkip( x )

	case TK_DIVIDER
		'' (ditto)
		x = cSkip( x )

	case else
		x = -1
	end select

	function = x
end function

private function hMergeUnknown _
	( _
		byval x as integer, _
		byval parent as ASTNODE ptr _
	) as integer

	dim as integer begin = any
	dim as ASTNODE ptr expr = any

	if( (parent = NULL) or (tkIsPoisoned( x ) = FALSE) ) then
		return -1
	end if

	begin = x
	do
		x += 1
	loop while( tkIsPoisoned( x ) )

	astAddChild( parent, astNewUNKNOWN( tkToText( begin, x - 1 ) ) )

	function = x
end function

private function cUnknown _
	( _
		byval x as integer, _
		byval parent as ASTNODE ptr _
	) as integer

	dim as integer begin = any

	'' This should only be called during the 1st pass
	assert( parent = NULL )

	begin = x
	x = cSkipStatement( x )
	tkSetPoisoned( begin, x - 1 )

	function = x
end function

'' (MultDecl{Field} | StructCompound)*
private function cStructBody _
	( _
		byval x as integer, _
		byval parent as ASTNODE ptr _
	) as integer

	dim as integer old = any

	do
		select case( tkGet( x ) )
		case TK_RBRACE, TK_EOF
			exit do
		end select

		old = x

		x = hMergeUnknown( old, parent )
		if( x >= 0 ) then
			continue do
		end if

		x = cStructCompound( old, parent )
		if( x >= 0 ) then
			continue do
		end if

		x = cMultDecl( old, DECL_FIELD, parent )
		if( x >= 0 ) then
			continue do
		end if

		x = cSimpleToken( old, parent )
		if( x >= 0 ) then
			continue do
		end if

		x = cUnknown( old, parent )
	loop

	function = x
end function

'' [TYPEDEF] {STRUCT|UNION} [Identifier] '{' StructBody '}' [MultDecl] ';'
private function cStructCompound _
	( _
		byval x as integer, _
		byval parent as ASTNODE ptr _
	) as integer

	dim as integer head = any, is_typedef = any
	dim as string id
	dim as ASTNODE ptr struct = any

	head = x
	is_typedef = FALSE
	struct = NULL

	'' TYPEDEF?
	if( tkGet( x ) = KW_TYPEDEF ) then
		x = cSkip( x )
		is_typedef = TRUE
	end if

	'' {STRUCT|UNION}
	select case( tkGet( x ) )
	case KW_STRUCT, KW_UNION

	case else
		return -1
	end select
	x = cSkip( x )

	'' [Identifier]
	if( tkGet( x ) = TK_ID ) then
		id = *tkGetText( x )
		x = cSkip( x )
	elseif( is_typedef ) then
		'' If it's a typedef with anonymous struct block, we need to
		'' make up an id for it, for use in the base type of the
		'' typedef MultDecl. If it turns out to be just a single
		'' typedef, we can still solve it out later.
		id = hMakeTempId( )
	end if

	'' '{'
	if( tkGet( x ) <> TK_LBRACE ) then
		return -1
	end if
	x = cSkip( x )

	if( parent ) then
		struct = astNew( ASTCLASS_STRUCT )
		astSetId( struct, id )
		astAddComment( struct, hCollectComments( head, x - 1 ) )
		astAddChild( parent, struct )
	end if

	x = cStructBody( x, struct )

	'' '}'
	if( tkGet( x ) <> TK_RBRACE ) then
		return -1
	end if
	x = cSkip( x )

	if( is_typedef ) then
		struct = astNewID( id )
		x = cIdList( x, DECL_TYPEDEF, TYPE_UDT, struct, parent )
		astDelete( struct )
		if( x < 0 ) then
			return -1
		end if
	else
		'' ';'
		if( tkGet( x ) <> TK_SEMI ) then
			return -1
		end if
		x = cSkip( x )
	end if

	function = x
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function cSignMod _
	( _
		byval x as integer, _
		byref sign as integer _
	) as integer

	sign = 0

	'' [SIGNED|UNSIGNED]
	select case( tkGet( x ) )
	case KW_SIGNED
		sign = -1
		x = cSkip( x )
	case KW_UNSIGNED
		sign = 1
		x = cSkip( x )
	end select

	function = x
end function

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
private function cBaseType _
	( _
		byval x as integer, _
		byref dtype as integer, _
		byref subtype as ASTNODE ptr, _
		byval decl as integer _
	) as integer

	dim as integer sign = any, signedmods = any, unsignedmods = any
	dim as integer constmods = any, shortmods = any, longmods = any
	dim as integer basetypex = any, basetypetk = any

	dtype = TYPE_NONE
	subtype = NULL

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
		select case as const( tkGet( x ) )
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

			select case as const( tkGet( x ) )
			case KW_ENUM, KW_STRUCT, KW_UNION
				'' {ENUM|STRUCT|UNION}
				x = cSkip( x )

				'' Identifier
				if( tkGet( x ) <> TK_ID ) then
					return -1
				end if
				basetypex = x

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
					select case( tkGet( cSkip( x ) ) )
					case TK_ID
						'' Another id must follow for params,
						'' otherwise it's ambigious
						if( decl = DECL_PARAM ) then
							return -1
						end if

					case TK_SEMI, TK_COMMA, TK_LBRACKET
						exit do

					case TK_LPAREN
						if( tkGet( cSkip( cSkip( x ) ) ) <> TK_STAR ) then
							exit do
						end if
					end select
				end if

				'' Treat the TK_ID as the type (a typedef)
				basetypex = x

			case KW_VOID, KW_CHAR, KW_FLOAT, KW_DOUBLE, KW_INT
				basetypex = x

			case else
				exit do
			end select
		end select

		x = cSkip( x )
	loop

	''
	'' 2. Refuse invalid modifier combinations etc.
	''

	if( basetypex >= 0 ) then
		basetypetk = tkGet( basetypex )
	end if

	'' Can't have both SIGNED and UNSIGNED
	if( (signedmods > 0) and (unsignedmods > 0) ) then
		return -1
	end if

	'' Neither both SHORT and LONG
	if( (shortmods > 0) and (longmods > 0) ) then
		return -1
	end if

	'' Max. 1 SHORT allowed, and 1 or 2 LONGs
	if( (shortmods > 1) or (longmods > 2) ) then
		return -1
	end if

	select case( basetypetk )
	case TK_ID, KW_VOID, KW_FLOAT, KW_DOUBLE
		'' No SIGNED|UNSIGNED|SHORT|LONG for UDTs/floats/void
		'' (cannot be translated to FB)
		if( signedmods or unsignedmods or shortmods or longmods ) then
			return -1
		end if

		select case( basetypetk )
		case TK_ID
			dtype = TYPE_UDT
			subtype = astNewID( tkGetText( basetypex ) )
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
			return -1
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
			return -1
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
			return -1
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

	function = x
end function

''    '...' | MultDecl{Param}
private function cParamDecl _
	( _
		byval x as integer, _
		byval parent as ASTNODE ptr _
	) as integer

	dim as ASTNODE ptr param = any

	'' '...'?
	if( tkGet( x ) = TK_ELLIPSIS ) then
		if( parent ) then
			param = astNew( ASTCLASS_PARAM )
			astAddComment( param, hCollectComments( x, cSkip( x ) - 1 ) )
			astAddChild( parent, param )
		end if
		x = cSkip( x )
	else
		x = cMultDecl( x, DECL_PARAM, parent )
	end if

	function = x
end function

'' [ParamDecl (',' ParamDecl)*]
private function cParamDeclList _
	( _
		byval x as integer, _
		byval parent as ASTNODE ptr _
	) as integer

	do
		x = cParamDecl( x, parent )

		'' ','?
		if( tkGet( x ) <> TK_COMMA ) then
			exit do
		end if
		x = cSkip( x )
	loop

	function = x
end function

''
'' Declarator =
''    '*'*
''    { [Identifier] | '(' Declarator ')' }
''    { '(' ParamList ')' | '[' ArrayElements ']' }
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
private function cDeclarator _
	( _
		byval x as integer, _
		byval decl as integer, _
		byval basedtype as integer, _
		byval basesubtype as ASTNODE ptr, _
		byval parent as ASTNODE ptr, _
		byref ast as ASTNODE ptr, _
		byref procptrdtype as integer _
	) as integer

	dim as ASTNODE ptr proc = any
	dim as integer begin = any, astclass = any, elements = any
	dim as integer dtype = any, innerprocptrdtype = any
	dim as string id

	begin = x
	dtype = basedtype
	innerprocptrdtype = TYPE_PROC
	procptrdtype = TYPE_PROC
	elements = 0
	ast = NULL
	proc = NULL

	'' Pointers: ('*')*
	while( tkGet( x ) = TK_STAR )
		procptrdtype = typeAddrOf( procptrdtype )
		dtype = typeAddrOf( dtype )
		x = cSkip( x )

		'' (CONST)*
		while( tkGet( x ) = KW_CONST )
			procptrdtype = typeSetIsConst( procptrdtype )
			dtype = typeSetIsConst( dtype )
			x = cSkip( x )
		wend
	wend

	if( tkGet( x ) = TK_LPAREN ) then
		'' '('
		x = cSkip( x )

		x = cDeclarator( x, decl, dtype, basesubtype, parent, ast, innerprocptrdtype )

		'' ')'
		if( tkGet( x ) <> TK_RPAREN ) then
			return -1
		end if
		x = cSkip( x )
	else
		if( tkGet( x ) = TK_ID ) then
			id = *tkGetText( x )
			x = cSkip( x )
		else
			'' An identifier must exist, except for parameters
			if( decl <> DECL_PARAM ) then
				return -1
			end if
		end if

		if( parent ) then
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

			ast = astNew( astclass )

			select case( decl )
			case DECL_EXTERNVAR
				ast->attrib or= ASTATTRIB_EXTERN
			case DECL_STATICVAR
				ast->attrib or= ASTATTRIB_STATIC
			end select

			astSetId( ast, id )
			astSetType( ast, dtype, basesubtype )
			astAddComment( ast, hCollectComments( begin, x - 1 ) )
			astAddChild( parent, ast )
		end if
	end if

	select case( tkGet( x ) )
	'' '[' ArrayElements ']'
	case TK_LBRACKET
		x = cSkip( x )

		'' Simple number?
		if( tkGet( x ) <> TK_DECNUM ) then
			return -1
		end if
		elements = valint( *tkGetText( x ) )
		x = cSkip( x )

		'' ']'
		if( tkGet( x ) <> TK_RBRACKET ) then
			return -1
		end if
		x = cSkip( x )

	'' '(' ParamList ')'
	case TK_LPAREN
		x = cSkip( x )

		if( ast ) then
			'' Parameters turn a vardecl/fielddecl into a procdecl,
			'' unless they're for a procptr type.
			if( innerprocptrdtype <> TYPE_PROC ) then
				'' There were '()'s above and the recursive
				'' cDeclarator() call found pointers/CONSTs,
				'' these parameters are for a function pointer.
				proc = astNew( ASTCLASS_PROC )

				'' The function pointer's result type is the
				'' base type plus any pointers up to this level.
				astSetType( proc, dtype, basesubtype )

				'' The declared symbol's type is the function
				'' pointer plus additional pointers if any
				astDelete( ast->subtype )
				ast->dtype = innerprocptrdtype
				ast->subtype = proc
			else
				'' A plain symbol, not a pointer, becomes a function
				select case( ast->class )
				case ASTCLASS_VAR, ASTCLASS_FIELD
					ast->class = ASTCLASS_PROC
				end select
				proc = ast
			end if
		end if

		'' Just '(void)'?
		if( (tkGet( x ) = KW_VOID) and (tkGet( cSkip( x ) ) = TK_RPAREN) ) then
			'' VOID
			x = cSkip( x )
		'' Not just '()'?
		elseif( tkGet( x ) <> TK_RPAREN ) then
			x = cParamDeclList( x, proc )
		end if

		'' ')'
		if( tkGet( x ) <> TK_RPAREN ) then
			return -1
		end if
		x = cSkip( x )
	end select

	function = x
end function

'' IdList = Declarator (',' Declarator)* [';']
private function cIdList _
	( _
		byval x as integer, _
		byval decl as integer, _
		byval basedtype as integer, _
		byval basesubtype as ASTNODE ptr, _
		byval parent as ASTNODE ptr _
	) as integer

	'' ... (',' ...)*
	do
		x = cDeclarator( x, decl, basedtype, basesubtype, parent, NULL, 0 )
		if( x < 0 ) then
			return -1
		end if

		'' Everything can have a comma and more identifiers,
		'' except for parameters.
		if( decl = DECL_PARAM ) then
			exit do
		end if

		'' ','?
		if( tkGet( x ) <> TK_COMMA ) then
			exit do
		end if
		x = cSkip( x )
	loop

	'' Everything except parameters must end with a ';'
	if( decl <> DECL_PARAM ) then
		'' ';'
		if( tkGet( x ) <> TK_SEMI ) then
			return -1
		end if
		x = cSkip( x )
	end if

	function = x
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
private function cMultDecl _
	( _
		byval x as integer, _
		byval decl as integer, _
		byval parent as ASTNODE ptr _
	) as integer

	dim as integer dtype = any, typebegin = any, typeend = any
	dim as ASTNODE ptr subtype = any

	'' BaseType
	typebegin = x
	x = cBaseType( x, dtype, subtype, decl )
	if( x < 0 ) then
		return -1
	end if
	typeend = x

	x = cIdList( x, decl, dtype, subtype, parent )
	astDelete( subtype )
	if( x < 0 ) then
		return -1
	end if

	function = x
end function

'' Global variable/procedure declarations
''    [EXTERN|STATIC] MultDecl
private function cGlobalDecl _
	( _
		byval x as integer, _
		byval parent as ASTNODE ptr _
	) as integer

	dim as integer decl = any

	select case( tkGet( x ) )
	case KW_EXTERN, KW_STATIC
		if( tkGet( x ) = KW_EXTERN ) then
			decl = DECL_EXTERNVAR
		else
			decl = DECL_STATICVAR
		end if
		x = cSkip( x )
	case else
		decl = DECL_VAR
	end select

	function = cMultDecl( x, decl, parent )
end function

'' Typedefs
''    TYPEDEF MultDecl
private function cTypedef _
	( _
		byval x as integer, _
		byval parent as ASTNODE ptr _
	) as integer

	'' TYPEDEF?
	if( tkGet( x ) <> KW_TYPEDEF ) then
		return -1
	end if
	x = cSkip( x )

	function = cMultDecl( x, DECL_TYPEDEF, parent )
end function

private sub hToplevel( byval parent as ASTNODE ptr )
	dim as integer x = any, old = any

	x = cSkip( -1 )

	while( tkGet( x ) <> TK_EOF )
		old = x

		x = hMergeUnknown( old, parent )
		if( x >= 0 ) then
			continue while
		end if

		x = cStructCompound( old, parent )
		if( x >= 0 ) then
			continue while
		end if

		x = cGlobalDecl( old, parent )
		if( x >= 0 ) then
			continue while
		end if

		x = cTypedef( old, parent )
		if( x >= 0 ) then
			continue while
		end if

		x = cSimpleToken( old, parent )
		if( x >= 0 ) then
			continue while
		end if

		x = cUnknown( old, parent )
	wend
end sub

function cToplevel( byval parent as ASTNODE ptr ) as ASTNODE ptr
	'' 1st pass to identify constructs & set marks correspondingly
	parser.tempidcount = 0
	hToplevel( NULL )

	'' 2nd pass to build up AST
	parser.tempidcount = 0
	hToplevel( parent )

	function = parent
end function

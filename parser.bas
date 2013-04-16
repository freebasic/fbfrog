'' C parsing passes

#include once "fbfrog.bi"

type PARSERSTUFF
	dryrun		as integer
end type

dim shared as PARSERSTUFF parser

declare function cStructCompound( byval x as integer ) as integer
declare function cMultDecl _
	( _
		byval x as integer, _
		byval decl as integer _
	) as integer

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

private function cSkipSpaceAndComments _
	( _
		byval x as integer, _
		byval delta as integer = 1 _
	) as integer

	do
		x += delta

		select case( tkGet( x ) )
		case TK_SPACE, TK_COMMENT

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

		case else
			if( tkIsStmtSep( x ) ) then
				exit do
			end if

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

private function hCount _
	( _
		byval tk as integer, _
		byval first as integer, _
		byval last as integer _
	) as integer

	dim as integer count = any

	count = 0
	for i as integer = first to last
		if( tkGet( i ) = tk ) then
			count += 1
		end if
	next

	function = count
end function

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

	do
		x += delta

		select case( tkGet( x ) )
		case TK_SPACE, TK_COMMENT

		case TK_EOL, TK_EOF
			exit do

		case else
			'' High-level tokens count as separate lines
			function = tkIsStmtSep( x )
			exit do
		end select
	loop

end function

private sub hAccumComment( byval x as integer, byref comment as string )
	dim as zstring ptr s = any
	dim as string text

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
	dim as integer x = any, at_bol = any, at_eol = any

	x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_COMMENT
			'' a) comment at EOL, behind code
			'' b) comment at BOL, in front of code, or above code
			''    and not separated with an empty line
			'' c) comment alone in line, followed by empty line
			'' d) comment in the middle of code in a line
			at_bol = hIsBeforeEol( x, -1 )
			at_eol = hIsBeforeEol( x,  1 )

			if( at_bol and at_eol ) then
				if( hCount( TK_EOL, x + 1, cSkip( x ) ) >= 2 ) then
					hAccumTkComment( cSkipSpaceAndComments( x ), x )
				else
					hAccumTkComment( cSkip( x ), x )
				end if
			elseif( at_bol ) then
				hAccumTkComment( cSkipSpaceAndComments( x ), x )
			elseif( at_eol ) then
				hAccumTkComment( cSkipSpaceAndComments( x, -1 ), x )
			else
				hAccumTkComment( cSkipSpaceAndComments( x ), x )
			end if

			tkRemove( x, x )
			x -= 1

		end select

		x += 1
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function ppDirective( byval x as integer ) as integer
	dim as integer begin = any
	dim as string text

	begin = x

	'' not at BOL?
	if( tkIsStmtSep( x - 1 ) = FALSE ) then
		return -1
	end if

	'' '#'
	if( tkGet( x ) <> TK_HASH ) then
		return -1
	end if
	x = ppSkip( x )

	select case( tkGet( x ) )
	'' DEFINE Identifier ['(' ParameterList ')'] Body Eol .
	case KW_DEFINE
		'' DEFINE
		x = ppSkip( x )

		'' Identifier?
		if( tkGet( x ) <> TK_ID ) then
			return -1
		end if
		text = *tkGetText( x )
		x = ppSkip( x )

		tkRemove( begin, x - 1 )
		tkInsert( begin, TK_PPDEFINE, text )
		begin += 1
		x = begin

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

	case KW_INCLUDE
		'' INCLUDE
		x = ppSkip( x )

		'' "..."
		if( tkGet( x ) <> TK_STRING ) then
			return -1
		end if
		text = *tkGetText( x )
		x = ppSkip( x )

		tkRemove( begin, x )
		tkInsert( begin, TK_PPINCLUDE, text )
		begin += 1
		x = begin

	case else
		return -1
	end select

	'' EOL?
	select case( tkGet( x ) )
	case TK_EOL
		tkRemove( x, ppSkip( x ) - 1 )

	case TK_EOF

	case else
		return -1
	end select

	function = x
end function

private function ppUnknownDirective( byval x as integer ) as integer
	dim as integer begin = any

	begin = x

	'' not at BOL?
	if( tkIsStmtSep( x - 1 ) = FALSE ) then
		return -1
	end if

	'' '#'
	if( tkGet( x ) <> TK_HASH ) then
		return -1
	end if
	x = ppSkip( x )

	x = ppSkipToEOL( x )

	'' EOL? (could also be EOF)
	if( tkGet( x ) = TK_EOL ) then
		x = ppSkip( x )
	end if

	tkInsert( begin, TK_TODO, "unknown PP directive (sorry)" )
	begin += 1
	x += 1
	tkInsert( begin, TK_BEGIN )
	begin += 1
	x += 1

	tkInsert( x, TK_END )
	x += 1

	function = x
end function

'' Merge empty lines into TK_DIVIDER, and also look for comments that are
'' surrounded by empty lines. We can assume to start at BOL, as cPPDirectives()
'' effectively parses one line after another.
function ppDivider( byval x as integer ) as integer
	dim as integer lines = any, begin = any
	dim as string comment

	begin = x

	'' Count empty lines in a row, while allowing only 1 line with comments
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

	if( lines < 2 ) then
		return -1
	end if

	comment = hCollectComments( begin, x - 1 )
	tkRemove( begin, x - 1 )
	tkInsert( begin, TK_DIVIDER )
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

private function cSimpleToken( byval x as integer ) as integer
	select case( tkGet( x ) )
	case TK_SEMI
		if( parser.dryrun ) then
			x = cSkip( x )
		else
			tkRemove( x, cSkip( x ) - 1 )
		end if

	case else
		'' Any pre-existing high-level tokens (things transformed by
		'' previous parsing, such as PP directives, or anything inserted
		'' by presets) need to be recognized as "valid constructs" too.
		if( tkIsStmtSep( x ) ) then
			x = cSkip( x )
		else
			x = -1
		end if
	end select

	function = x
end function

private function hMergeUnknown( byval x as integer ) as integer
	dim as integer begin = any

	if( parser.dryrun ) then
		return -1
	end if

	if( tkIsPoisoned( x ) = FALSE ) then
		return -1
	end if

	begin = x
	do
		x += 1
	loop while( tkIsPoisoned( x ) )

	tkInsert( begin, TK_TODO, "unknown construct (sorry)" )
	begin += 1
	x += 1

	tkInsert( begin, TK_BEGIN )
	begin += 1
	x += 1

	tkInsert( x, TK_END )
	x += 1

	function = x
end function

private function cUnknown( byval x as integer ) as integer
	dim as integer begin = any

	assert( parser.dryrun )
	begin = x
	x = cSkipStatement( x )
	tkSetPoisoned( begin, x - 1 )

	function = x
end function

'' (MultDecl{Field} | StructCompound)*
private function cStructBody( byval x as integer ) as integer
	dim as integer old = any

	do
		select case( tkGet( x ) )
		case TK_RBRACE, TK_EOF
			exit do
		end select

		old = x

		x = hMergeUnknown( old )
		if( x >= 0 ) then
			continue do
		end if

		x = cStructCompound( old )
		if( x >= 0 ) then
			continue do
		end if

		x = cMultDecl( old, TK_FIELD )
		if( x >= 0 ) then
			continue do
		end if

		x = cSimpleToken( old )
		if( x >= 0 ) then
			continue do
		end if

		x = cUnknown( old )
	loop

	function = x
end function

'' {STRUCT|UNION} [Identifier] '{' StructBody '}'
private function cStructCompound( byval x as integer ) as integer
	dim as integer head = any, tail = any
	dim as string id, comment

	head = x

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
	end if

	'' '{'
	if( tkGet( x ) <> TK_LBRACE ) then
		return -1
	end if
	x = cSkip( x )

	if( parser.dryrun = FALSE ) then
		comment = hCollectComments( head, x - 1 )
		tkRemove( head, x - 1 )
		tkInsert( head, TK_STRUCT, id )
		tkSetComment( head, comment )
		head += 1
		x = head
	end if

	x = cStructBody( x )

	tail = x

	'' '}'
	if( tkGet( x ) <> TK_RBRACE ) then
		return -1
	end if
	x = cSkip( x )

	'' ';'
	if( tkGet( x ) <> TK_SEMI ) then
		return -1
	end if
	x = cSkip( x )

	if( parser.dryrun = FALSE ) then
		'' TK_BEGIN/END should be inserted together in one go, because
		'' the field parsing may need to look ahead/back; that would
		'' break if the token buffer is in an inconsistent state.
		tkInsert( head, TK_BEGIN )
		head += 1
		tail += 1
		x += 1

		comment = hCollectComments( tail, x - 1 )
		tkRemove( tail, x - 1 )
		tkInsert( tail, TK_END )
		tkSetComment( tail, comment )
		tail += 1
		x = tail
	end if

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

	dim as integer sign = any

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
			return -1
		end if

		'' {ENUM|STRUCT|UNION}
		x = cSkip( x )

		'' Identifier
		if( tkGet( x ) <> TK_ID ) then
			return -1
		end if
		dtype = typeSetDt( dtype, TYPE_UDT )
		subtype = *tkGetText( x )
		x = cSkip( x )

	case TK_ID
		if( sign <> 0 ) then
			return -1
		end if

		'' Identifier
		dtype = typeSetDt( dtype, TYPE_UDT )
		subtype = *tkGetText( x )
		x = cSkip( x )

	case KW_VOID
		if( sign <> 0 ) then
			return -1
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
		return -1
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
	dim as string comment

	'' '...'?
	if( tkGet( x ) = TK_ELLIPSIS ) then
		if( parser.dryrun = FALSE ) then
			comment = hCollectComments( x, cSkip( x ) - 1 )
			tkRemove( x, cSkip( x ) - 1 )
			tkInsert( x, TK_PARAMVARARG )
			tkSetComment( x, comment )
		end if
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
		if( parser.dryrun ) then
			x = cSkip( x )
		else
			tkRemove( x, cSkip( x ) - 1 )
		end if
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
	dim as string id, comment

	begin = x
	dtype = basedtype

	'' PtrCount
	x = cPtrCount( x, dtype )

	'' '('?
	if( tkGet( x ) = TK_LPAREN ) then
		x = cSkip( x )

		'' '*'
		if( tkGet( x ) <> TK_STAR ) then
			return -1
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
		case TK_TYPEDEF
			decl = TK_TYPEDEFPROCPTR
		case else
			return -1
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
			return -1
		end select
	end if

	if( is_procptr ) then
		'' ')'
		if( tkGet( x ) <> TK_RPAREN ) then
			return -1
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
		     TK_PARAMPROCPTR, TK_FIELDPROCPTR, TK_TYPEDEFPROCPTR

		case else
			return -1
		end select

		has_params = TRUE
	else
		'' If it's a function pointer there must also be a parameter list
		if( is_procptr ) then
			return -1
		end if
		has_params = FALSE
	end if

	if( parser.dryrun = FALSE ) then
		comment = hCollectComments( begin, x - 1 )
		tkRemove( begin, x - 1 )
		tkInsert( begin, decl, id )
		tkSetComment( begin, comment )
		tkSetType( begin, dtype, basesubtype )
		begin += 1
		x = begin
	end if

	if( has_params ) then
		'' Just '(void)'?
		if( (tkGet( x ) = KW_VOID) and (tkGet( cSkip( x ) ) = TK_RPAREN) ) then
			'' VOID
			if( parser.dryrun ) then
				x = cSkip( x )
			else
				tkRemove( x, cSkip( x ) - 1 )
			end if
		'' Not just '()'?
		elseif( tkGet( x ) <> TK_RPAREN ) then
			if( parser.dryrun = FALSE ) then
				tkInsert( x, TK_BEGIN )
				x += 1
			end if

			x = cParamDeclList( x )

			if( parser.dryrun = FALSE ) then
				tkInsert( x, TK_END )
				x += 1
			end if
		end if

		'' ')'
		if( tkGet( x ) <> TK_RPAREN ) then
			return -1
		end if
		if( parser.dryrun ) then
			x = cSkip( x )
		else
			tkRemove( x, cSkip( x ) - 1 )
		end if
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
''    BaseType DeclElement (',' DeclElement)* [';']
''
private function cMultDecl _
	( _
		byval x as integer, _
		byval decl as integer _
	) as integer

	dim as integer dtype = any, typebegin = any, typeend = any
	dim as string subtype

	'' BaseType
	typebegin = x
	x = cBaseType( x, dtype, subtype )
	if( x < 0 ) then
		return -1
	end if
	typeend = x

	'' ... (',' ...)*
	do
		x = cDeclElement( x, decl, dtype, subtype )
		if( x < 0 ) then
			return -1
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
		if( parser.dryrun ) then
			x = cSkip( x )
		else
			tkRemove( x, cSkip( x ) - 1 )
		end if
	loop

	'' Everything except params must end with a ';'
	select case( decl )
	case TK_PARAM, TK_PARAMPROCPTR, TK_PARAMVARARG

	case else
		'' ';'
		if( tkGet( x ) <> TK_SEMI ) then
			return -1
		end if
		if( parser.dryrun ) then
			x = cSkip( x )
		else
			hAccumComment( cSkipRev( x ), _
				hCollectComments( x, _
					cSkipSpaceAndComments( x ) - 1 ) )

			tkRemove( x, cSkip( x ) - 1 )
		end if
	end select

	if( parser.dryrun = FALSE ) then
		hAccumComment( typeend, _
			hCollectComments( typebegin, typeend - 1 ) )
		tkRemove( typebegin, typeend - 1 )
		x -= typeend - typebegin
	end if

	function = x
end function

'' Global variable/procedure declarations
''    [EXTERN|STATIC] MultDecl
private function cGlobalDecl( byval x as integer ) as integer
	dim as integer decl = any

	select case( tkGet( x ) )
	case KW_EXTERN, KW_STATIC
		if( tkGet( x ) = KW_EXTERN ) then
			decl = TK_EXTERNGLOBAL
		else
			decl = TK_STATICGLOBAL
		end if

		if( parser.dryrun ) then
			x = cSkip( x )
		else
			tkRemove( x, cSkip( x ) - 1 )
		end if

	case else
		decl = TK_GLOBAL
	end select

	function = cMultDecl( x, decl )
end function

'' Typedefs
''    TYPEDEF MultDecl
private function cTypedef( byval x as integer ) as integer

	'' TYPEDEF?
	if( tkGet( x ) <> KW_TYPEDEF ) then
		return -1
	end if
	if( parser.dryrun ) then
		x = cSkip( x )
	else
		tkRemove( x, cSkip( x ) - 1 )
	end if

	function = cMultDecl( x, TK_TYPEDEF )
end function

private sub hToplevel( )
	dim as integer x = any, old = any

	x = cSkip( -1 )

	'' Prune space at BOF
	if( parser.dryrun = FALSE ) then
		tkRemove( 0, x - 1 )
		x = 0
	end if

	while( tkGet( x ) <> TK_EOF )
		old = x

		x = hMergeUnknown( old )
		if( x >= 0 ) then
			continue while
		end if

		x = cStructCompound( old )
		if( x >= 0 ) then
			continue while
		end if

		x = cGlobalDecl( old )
		if( x >= 0 ) then
			continue while
		end if

		x = cTypedef( old )
		if( x >= 0 ) then
			continue while
		end if

		x = cSimpleToken( old )
		if( x >= 0 ) then
			continue while
		end if

		x = cUnknown( old )
	wend
end sub

sub cToplevel( )
	'' 1st pass to identify constructs & set marks correspondingly
	parser.dryrun = TRUE
	hToplevel( )

	'' 2nd pass to merge them into high-level tokens
	parser.dryrun = FALSE
	hToplevel( )
end sub

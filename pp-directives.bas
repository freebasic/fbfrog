''
'' CPP directive parsing
''
'' ppDirectives1() merges PP directives into TK_AST tokens, except that #define
'' bodies and #if expressions are not yet parsed, but only enclosed in TK_BEGIN
'' and TK_END tokens.
''
'' ppDirectives2() goes through all PP directives and finishes the parsing job,
'' by parsing the #define bodies and #if expressions into ASTs properly,
'' assigning them to the TK_AST tokens of the corresponding directives, and
'' removing the TK_BEGIN/TK_END and the tokens they enclosed.
''
'' With this separation it's possible to identify PP directives and still do
'' macro expansion etc. in #define bodies or #if expressions, before the 2nd
'' step finalizes the parsing.
''

#include once "fbfrog.bi"

private function ppSkip( byval x as integer ) as integer
	dim as integer y = any

	do
		x += 1

		select case( tkGet( x ) )
		case TK_SPACE, TK_COMMENT

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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function hNumberLiteral( byval x as integer ) as ASTNODE ptr
	dim as longint value

	select case( tkGet( x ) )
	case TK_DECNUM
		value = vallng( *tkGetText( x ) )
	case TK_HEXNUM
		value = vallng( "&h" + *tkGetText( x ) )
	case TK_OCTNUM
		value = vallng( "&o" + *tkGetText( x ) )
	case else
		assert( FALSE )
	end select

	function = astNewCONSTi( value, TYPE_LONGINT )
end function

type PPOPINFO
	level		as integer
	is_leftassoc	as integer
end type

'' PP operator precedence (higher value = higher precedence)
dim shared as PPOPINFO ppopinfo(ASTCLASS_IIF to ASTCLASS_UNARYPLUS) = _
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

'' PP expression parser based on precedence climbing
private function ppExpression _
	( _
		byref x as integer, _
		byval level as integer = 0 _
	) as ASTNODE ptr

	dim as ASTNODE ptr a = any, b = any, c = any
	dim as integer astclass = any, oplevel = any, have_parens = any

	function = NULL

	'' Unary prefix operators
	select case( tkGet( x ) )
	case TK_EXCL   : astclass = ASTCLASS_LOGNOT    '' !
	case TK_TILDE  : astclass = ASTCLASS_BITNOT    '' ~
	case TK_MINUS  : astclass = ASTCLASS_NEGATE    '' -
	case TK_PLUS   : astclass = ASTCLASS_UNARYPLUS '' +
	case else      : astclass = -1
	end select

	if( astclass >= 0 ) then
		x = ppSkip( x )
		a = astNew( astclass, ppExpression( x, ppopinfo(astclass).level ), NULL, NULL )
	else
		'' Atoms
		select case( tkGet( x ) )
		'' '(' Expression ')'
		case TK_LPAREN
			'' '('
			x = ppSkip( x )

			'' Expression
			a = ppExpression( x )
			if( a = NULL ) then
				exit function
			end if

			'' ')'
			if( tkGet( x ) <> TK_RPAREN ) then
				astDelete( a )
				exit function
			end if
			x = ppSkip( x )

		case TK_OCTNUM, TK_DECNUM, TK_HEXNUM
			a = hNumberLiteral( x )
			x = ppSkip( x )

		'' DEFINED '(' Identifier ')'
		case KW_DEFINED
			x = ppSkip( x )

			'' '('
			if( tkGet( x ) = TK_LPAREN ) then
				have_parens = TRUE
				x = ppSkip( x )
			else
				have_parens = FALSE
			end if

			'' Identifier
			if( tkGet( x ) <> TK_ID ) then
				exit function
			end if
			a = astNew( ASTCLASS_ID, tkGetText( x ) )
			x = ppSkip( x )

			if( have_parens ) then
				'' ')'
				if( tkGet( x ) <> TK_RPAREN ) then
					astDelete( a )
					exit function
				end if
				x = ppSkip( x )
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
		oplevel = ppopinfo(astclass).level
		if( oplevel < level ) then
			exit do
		end if
		if( ppopinfo(astclass).is_leftassoc ) then
			oplevel += 1
		end if

		'' operator
		x = ppSkip( x )

		'' rhs
		b = ppExpression( x, oplevel )
		if( b = NULL ) then
			astDelete( a )
			exit function
		end if

		'' Handle ?: special case
		if( astclass = ASTCLASS_IIF ) then
			'' ':'?
			if( tkGet( x ) <> TK_COLON ) then
				astDelete( a )
				astDelete( b )
				exit function
			end if
			x = ppSkip( x )

			c = ppExpression( x, oplevel )
			if( c = NULL ) then
				astDelete( a )
				astDelete( b )
				exit function
			end if
		else
			c = NULL
		end if

		a = astNew( astclass, a, b, c )
	loop

	function = a
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

namespace eval
	dim shared as THASH symbols
end namespace

sub ppAddSymbol( byval id as zstring ptr, byval is_defined as integer )
	if( eval.symbols.items = NULL ) then
		hashInit( @eval.symbols, 4 )
	end if
	hashAddOverwrite( @eval.symbols, id, cptr( any ptr, is_defined ) )
end sub

sub ppResetSymbols( )
	hashEnd( @eval.symbols )
	eval.symbols.items = NULL
end sub

function ppExprFold( byval n as ASTNODE ptr ) as ASTNODE ptr
	dim as THASHITEM ptr item = any
	dim as long v1 = any, v2 = any

	function = n

	if( n = NULL ) then
		exit function
	end if

	select case as const( n->class )
	case ASTCLASS_DEFINED
		'' defined() on known symbol?
		assert( n->l->class = ASTCLASS_ID )
		item = hashLookup( @eval.symbols, n->l->text, hashHash( n->l->text ) )
		if( item->s ) then
			function = astNewCONSTi( iif( item->data, 1, 0 ), TYPE_LONG )
			astDelete( n )
		end if

	case ASTCLASS_IIF
		n->l = ppExprFold( n->l )
		n->r = ppExprFold( n->r )
		n->next = ppExprFold( n->next )

		if( n->next->class = ASTCLASS_CONST ) then
			if( n->next->intval ) then
				function = astClone( n->l )
			else
				function = astClone( n->r )
			end if
			astDelete( n )
		end if

	case ASTCLASS_LOGOR, ASTCLASS_LOGAND, _
	     ASTCLASS_BITOR, ASTCLASS_BITXOR, ASTCLASS_BITAND, _
	     ASTCLASS_EQ, ASTCLASS_NE, _
	     ASTCLASS_LT, ASTCLASS_LE, _
	     ASTCLASS_GT, ASTCLASS_GE, _
	     ASTCLASS_SHL, ASTCLASS_SHR, _
	     ASTCLASS_ADD, ASTCLASS_SUB, _
	     ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD

		n->l = ppExprFold( n->l )
		n->r = ppExprFold( n->r )

		if( (n->l->class = ASTCLASS_CONST) and _
		    (n->r->class = ASTCLASS_CONST) ) then
			v1 = n->l->intval
			v2 = n->r->intval

			select case as const( n->class )
			case ASTCLASS_LOGOR  : v1    = iif( v1 orelse  v2, 1, 0 )
			case ASTCLASS_LOGAND : v1    = iif( v1 andalso v2, 1, 0 )
			case ASTCLASS_BITOR  : v1  or= v2
			case ASTCLASS_BITXOR : v1 xor= v2
			case ASTCLASS_BITAND : v1 and= v2
			case ASTCLASS_EQ     : v1    = iif( v1 =  v2, 1, 0 )
			case ASTCLASS_NE     : v1    = iif( v1 <> v2, 1, 0 )
			case ASTCLASS_LT     : v1    = iif( v1 <  v2, 1, 0 )
			case ASTCLASS_LE     : v1    = iif( v1 <= v2, 1, 0 )
			case ASTCLASS_GT     : v1    = iif( v1 >  v2, 1, 0 )
			case ASTCLASS_GE     : v1    = iif( v1 >= v2, 1, 0 )
			case ASTCLASS_SHL    : v1 shl= v2
			case ASTCLASS_SHR    : v1 shr= v2
			case ASTCLASS_ADD    : v1   += v2
			case ASTCLASS_SUB    : v1   -= v2
			case ASTCLASS_MUL    : v1   *= v2
			case ASTCLASS_DIV    : v1   /= v2
			case ASTCLASS_MOD    : v1 mod= v2
			case else
				assert( FALSE )
			end select

			function = astNewCONSTi( v1, TYPE_LONG )
			astDelete( n )
		end if

	case ASTCLASS_LOGNOT, ASTCLASS_BITNOT, _
	     ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS

		n->l = ppExprFold( n->l )

		if( n->l->class = ASTCLASS_CONST ) then
			v1 = n->l->intval

			select case as const( n->class )
			case ASTCLASS_LOGNOT    : v1 = iif( v1, 0, 1 )
			case ASTCLASS_BITNOT    : v1 = not v1
			case ASTCLASS_NEGATE    : v1 = -v1
			case ASTCLASS_UNARYPLUS : '' nothing to do
			case else
				assert( FALSE )
			end select

			function = astNewCONSTi( v1, TYPE_LONG )
			astDelete( n )
		end if

	end select
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function ppDirective( byval x as integer ) as integer
	dim as integer begin = any, keepbegin = any, tk = any, y = any, astclass = any
	dim as ASTNODE ptr t = any

	begin = x
	keepbegin = -1
	t = NULL

	'' not at BOL?
	select case( tkGet( tkSkipSpaceAndComments( x, -1 ) ) )
	case TK_EOL, TK_EOF, TK_AST, TK_DIVIDER, TK_END

	case else
		return -1
	end select

	'' '#'
	if( tkGet( x ) <> TK_HASH ) then
		return -1
	end if
	x = ppSkip( x )

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
		t = astNew( ASTCLASS_PPDEFINE, tkGetText( x ) )
		x = ppSkip( x )

		keepbegin = x
		x = ppSkipToEOL( x )

	case KW_INCLUDE
		'' INCLUDE
		x = ppSkip( x )

		'' "filename"
		if( tkGet( x ) <> TK_STRING ) then
			return -1
		end if
		t = astNew( ASTCLASS_PPINCLUDE, tkGetText( x ) )
		x = ppSkip( x )

	case KW_IF, KW_ELIF
		x = ppSkip( x )
		keepbegin = x

		x = ppSkipToEOL( x )
		if( x = keepbegin ) then
			return -1
		end if

		t = astNew( iif( tk = KW_IF, ASTCLASS_PPIF, ASTCLASS_PPELSEIF ) )

	case KW_IFDEF, KW_IFNDEF
		x = ppSkip( x )

		'' Identifier?
		if( tkGet( x ) <> TK_ID ) then
			return -1
		end if
		t = astNew( ASTCLASS_ID, tkGetText( x ) )
		x = ppSkip( x )

		t = astNew( ASTCLASS_DEFINED, t, NULL, NULL )
		if( tk = KW_IFNDEF ) then
			t = astNew( ASTCLASS_LOGNOT, t, NULL, NULL )
		end if
		t = astNew( ASTCLASS_PPIF, t, NULL, NULL )

	case KW_ELSE, KW_ENDIF
		x = ppSkip( x )
		t = astNew( iif( tk = KW_ELSE, ASTCLASS_PPELSE, ASTCLASS_PPENDIF ) )

	case else
		return -1
	end select

	'' EOL?
	select case( tkGet( x ) )
	case TK_EOL, TK_EOF

	case else
		astDelete( t )
		return -1
	end select

	if( keepbegin >= 0 ) then
		tkRemove( begin, keepbegin - 1 )
		x -= keepbegin - begin
		keepbegin -= keepbegin - begin

		tkInsert( begin, TK_AST, , t )
		begin += 1
		keepbegin += 1
		x += 1

		tkInsert( keepbegin, TK_BEGIN )
		x += 1
		tkInsert( x, TK_END )
		x += 1
	else
		tkRemove( begin, x - 1 )
		tkInsert( begin, TK_AST, , t )
		begin += 1
		x = begin
	end if

	if( tkGet( x ) = TK_EOL ) then
		tkRemove( x, ppSkip( x ) - 1 )
	end if

	function = x
end function

private function ppSimpleToken( byval x as integer ) as integer
	'' Handle pre-existing high-level tokens
	select case( tkGet( x ) )
	case TK_AST
		x += 1
	case TK_DIVIDER
		x += 1
	case else
		x = -1
	end select
	function = x
end function

private function ppUnknownDirective( byval x as integer ) as integer
	dim as integer begin = any, y = any
	dim as ASTNODE ptr expr = any

	begin = x

	'' not at BOL?
	select case( tkGet( tkSkipSpaceAndComments( x, -1 ) ) )
	case TK_EOL, TK_EOF, TK_AST, TK_DIVIDER, TK_END

	case else
		return -1
	end select

	'' '#'
	if( tkGet( x ) <> TK_HASH ) then
		return -1
	end if
	x = ppSkip( x )

	y = ppSkipToEOL( x )
	expr = astNew( ASTCLASS_TEXT, tkToText( begin, y ) )
	expr = astNew( ASTCLASS_PPUNKNOWN, expr, NULL, NULL )
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

sub ppDirectives1( )
	dim as integer x = any, old = any

	x = ppSkip( -1 )
	while( tkGet( x ) <> TK_EOF )
		old = x

		x = ppDirective( old )
		if( x >= 0 ) then
			continue while
		end if

		x = ppSimpleToken( old )
		if( x >= 0 ) then
			continue while
		end if

		x = ppUnknownDirective( old )
		if( x >= 0 ) then
			continue while
		end if

		'' Skip to next line
		x = ppSkip( ppSkipToEOL( old ) )
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub ppDirectives2( )
	dim as integer x = any, begin = any
	dim as ASTNODE ptr t = any, expr = any

	x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_AST
			t = tkGetAst( x )
			x += 1

			select case( t->class )
			case ASTCLASS_PPDEFINE
				'' BEGIN
				assert( tkGet( x ) = TK_BEGIN )
				begin = x
				x += 1

				'' Body tokens?
				if( tkGet( x ) <> TK_END ) then
					do
						x += 1
					loop while( tkGet( x ) <> TK_END )
					astAddChild( t, astNew( ASTCLASS_TEXT, tkToText( begin + 1, x - 1 ) ) )
				end if

				'' END
				assert( tkGet( x ) = TK_END )
				tkRemove( begin, x )
				x = begin

			case ASTCLASS_PPIF, ASTCLASS_PPELSEIF
				'' No #if expression yet?
				if( t->l = NULL ) then
					'' BEGIN
					assert( tkGet( x ) = TK_BEGIN )
					begin = x
					x += 1

					'' Expression tokens
					expr = ppExpression( x )
					'' TK_END not reached after ppExpression()?
					if( tkGet( x ) <> TK_END ) then
						'' Then either no expression could be parsed at all,
						'' or it was followed by "junk" tokens...
						astDelete( expr )

						do
							x += 1
						loop while( tkGet( x ) <> TK_END )

						'' Turn it into a PPUNKNOWN
						astAddChild( t, astNew( ASTCLASS_TEXT, tkToText( begin + 1, x - 1 ) ) )
						t = astNew( ASTCLASS_PPUNKNOWN, astClone( t ), NULL, NULL )
						tkSetAst( begin - 1, t )
					else
						astAddChild( t, expr )
					end if

					'' END
					assert( tkGet( x ) = TK_END )
					tkRemove( begin, x )
					x = begin
				end if

			end select

		case else
			x += 1
		end select
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

const IFSTACKSIZE = 64

enum
	COND_UNKNOWN = 0
	COND_TRUE
	COND_FALSE
end enum

type IFSTACKNODE
	cond		as integer '' COND_*
	found_else	as integer
end type

dim shared as IFSTACKNODE ifstack(0 to IFSTACKSIZE-1)

sub ppEvalIfs( )
	dim as integer x = any, level = any, skip = any
	dim as ASTNODE ptr t = any

	x = 0
	level = -1
	skip = -1
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_AST
			t = tkGetAst( x )

			select case( t->class )
			case ASTCLASS_PPIF, ASTCLASS_PPELSEIF
				t = ppExprFold( astClone( t ) )
				tkSetAst( x, t )

				if( t->class = ASTCLASS_PPIF ) then
					level += 1
				else
					if( level < 0 ) then
						oops( "#elif without #if" )
					elseif( ifstack(level).found_else ) then
						oops( "#elif after #else" )
					end if
				end if

				if( t->l->class = ASTCLASS_CONST ) then
					ifstack(level).cond = iif( t->l->intval <> 0, COND_TRUE, COND_FALSE )

					'' not yet skipping?
					if( skip = -1 ) then
						'' #if FALSE? start skipping
						if( ifstack(level).cond = COND_FALSE ) then
							skip = level + 1
						end if
					end if
				else
					ifstack(level).cond = COND_UNKNOWN
				end if
				ifstack(level).found_else = FALSE

				print level,;
				if( t->class = ASTCLASS_PPIF ) then
					print "if";
				else
					print "elseif";
				end if
				print , "cond=" & ifstack(level).cond, "skip=" & skip

				if( ifstack(level).cond <> COND_UNKNOWN ) then
					tkRemove( x, x )
					t = NULL
					tkInsert( x, TK_EOL )
				end if

			case ASTCLASS_PPELSE
				if( ifstack(level).found_else ) then
					oops( "repeated #else" )
				end if
				ifstack(level).found_else = TRUE

				'' Skipping?
				if( skip >= 0 ) then
					'' Was the #if corresponding to this #else the cause of the skipping?
					if( skip = level + 1 ) then
						'' It must have been an #if FALSE
						assert( ifstack(level).cond = COND_FALSE )

						'' Now stop skipping and parse this #else block
						skip = -1
					end if
				else
					'' Not skipping, so the #if corresponding to this #else must have been
					'' an #if TRUE (and no other skipping from outer levels either).
					assert( ifstack(level).cond = COND_TRUE )

					'' Start skipping now to skip the #else block
					skip = level + 1
				end if

				if( ifstack(level).cond <> COND_UNKNOWN ) then
					tkRemove( x, x )
					t = NULL
					tkInsert( x, TK_EOL )
				end if

				print level, "else", "skip=" & skip

			case ASTCLASS_PPENDIF
				if( level < 0 ) then
					oops( "#endif without #if" )
				end if

				'' Skipping?
				if( skip >= 0 ) then
					'' Was the #if/#else corresponding to this #endif the cause of the skipping?
					if( skip = level + 1 ) then
						'' Now stop skipping
						skip = -1
					end if
				end if

				if( ifstack(level).cond <> COND_UNKNOWN ) then
					tkRemove( x, x )
					t = NULL
					tkInsert( x, TK_EOL )
				end if

				print level, "endif", "skip=" & skip
				level -= 1

			end select

			x += 1

		case else
			if( skip >= 0 ) then
				tkRemove( x, x )
				x -= 1
			end if
			x += 1
		end select
	loop
end sub

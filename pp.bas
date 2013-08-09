''
'' Token buffer preprocessing
'' --------------------------
''
'' ppComments() removes TK_COMMENTs, for easier parsing later. It can assign
'' their text to other tokens (tkSetComment()) if comments should be preserved
'' for later.
''
'' ppDividers() merges empty lines (i.e. multiple TK_EOLs separated only by
'' TK_SPACEs) into TK_DIVIDERs, for nicer output formatting later. It can be
'' nice to preserve the block/section/paragraph layout of the input, and still
'' trim down unnecessary newlines.
''
''
'' CPP directive parsing
'' ---------------------
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
''
'' #if evaluation
'' --------------
''
'' ppEvalExpressions() goes through all #ifs and #elseifs and tries to simplify
'' the expressions. ppAddSymbol() can be used to register symbols as "defined"
'' or "undefined" for #if defined() or #ifdef checks. defined() checks on
'' unknown symbols are not solved out.
''
'' #if evaluation is done with these 3 steps:
''
'' 1. ppSplitElseIfs(): Splitting up #elseifs blocks into normal
''    #else/#if/#endif blocks
'' 2. ppEvalIfs(): Checks each #if whether the condition is known to be true
''    or false, i.e. expressions that could be simplified down to a constant,
''    and if so, deletes tokens accordingly before continuing to the next #if.
''    Not having to worry about #elseifs greatly simplifies this step.
'' 3. ppMergeElseIfs(): Merging #else/#if/#endif blocks back into #elseifs
''

#include once "fbfrog.bi"

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

private sub hStripAllComments( )
	dim as integer x = any
	x = 0
	while( tkGet( x ) <> TK_EOF )
		if( tkGet( x ) = TK_COMMENT ) then
			tkRemove( x, x )
			x -= 1
		end if
		x += 1
	wend
end sub

private function ppComment( byval x as integer ) as integer
	dim as integer y = any, at_bol = any, at_eol = any

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

	function = x
end function

sub ppComments( )
	dim as integer x = any

	'' TODO
	hStripAllComments( )

	x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_COMMENT
			x = ppComment( x )

		end select

		x += 1
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Merge empty lines into TK_DIVIDER, assuming we're starting at BOL.
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
	blockcomment = tkCollectComments( eol1 + 1, eol2 )

	comment = tkCollectComments( begin, eol1 )
	tkRemove( begin, x - 1 )
	tkInsert( begin, TK_DIVIDER, blockcomment )
	tkSetComment( begin, comment )
	x = begin

	function = x
end function

sub ppDividers( )
	dim as integer x = any, old = any

	x = 0
	while( tkGet( x ) <> TK_EOF )
		old = x

		x = ppDivider( old )
		if( x >= 0 ) then
			continue while
		end if

		'' Skip to next BOL
		x = old
		do
			select case( tkGet( x ) )
			case TK_EOF
				exit do
			case TK_EOL
				x += 1
				exit do
			end select

			x += 1
		loop
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

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

function hNumberLiteral( byval x as integer ) as ASTNODE ptr
	dim as ASTNODE ptr n

	select case( tkGet( x ) )
	case TK_DECNUM
		n = astNewCONST( vallng( *tkGetText( x ) ), 0, TYPE_LONGINT )
	case TK_HEXNUM
		n = astNewCONST( vallng( "&h" + *tkGetText( x ) ), 0, TYPE_LONGINT )
		n->attrib or= ASTATTRIB_HEX
	case TK_OCTNUM
		n = astNewCONST( vallng( "&o" + *tkGetText( x ) ), 0, TYPE_LONGINT )
		n->attrib or= ASTATTRIB_OCT
	case TK_DECFLOAT
		n = astNewCONST( 0, val( *tkGetText( x ) ), TYPE_DOUBLE )
	case else
		assert( FALSE )
	end select

	function = n
end function

type PPOPINFO
	level		as integer
	is_leftassoc	as integer
end type

'' C PP operator precedence (higher value = higher precedence)
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

'' C PP expression parser based on precedence climbing
private function ppExpression _
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
		x = ppSkip( x )
		a = astNew( astclass, ppExpression( x, ppopinfo(astclass).level ) )
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

		case TK_OCTNUM, TK_DECNUM, TK_HEXNUM, TK_DECFLOAT
			a = hNumberLiteral( x )
			x = ppSkip( x )

		'' DEFINED '(' Identifier ')'
		case KW_DEFINED
			x = ppSkip( x )

			'' '('
			var have_parens = FALSE
			if( tkGet( x ) = TK_LPAREN ) then
				have_parens = TRUE
				x = ppSkip( x )
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

			a = astNew( ASTCLASS_DEFINED, a )

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
		var oplevel = ppopinfo(astclass).level
		if( oplevel < level ) then
			exit do
		end if
		if( ppopinfo(astclass).is_leftassoc ) then
			oplevel += 1
		end if

		'' operator
		x = ppSkip( x )

		'' rhs
		var b = ppExpression( x, oplevel )
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
			x = ppSkip( x )

			c = ppExpression( x, oplevel )
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

private function ppDirective( byval x as integer ) as integer
	var begin = x
	var keepbegin = -1
	dim as ASTNODE ptr t

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

	var tk = tkGet( x )
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

		t = astNew( ASTCLASS_DEFINED, t )
		if( tk = KW_IFNDEF ) then
			t = astNew( ASTCLASS_LOGNOT, t )
		end if
		t = astNew( ASTCLASS_PPIF, t )

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
	expr = tkToAstText( begin, y )
	expr = astNew( ASTCLASS_PPUNKNOWN, expr )
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
	var x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_AST
			var t = tkGetAst( x )
			x += 1

			select case( t->class )
			case ASTCLASS_PPDEFINE
				'' BEGIN
				assert( tkGet( x ) = TK_BEGIN )
				var begin = x
				x += 1

				'' Body tokens?
				if( tkGet( x ) <> TK_END ) then
					'' Try to parse the body as expression
					var expr = ppExpression( x )
					'' Expression found and TK_END reached?
					if( (expr <> NULL) and (tkGet( x ) = TK_END) ) then
						astAddChild( t, expr )
					else
						'' Then either no expression could be parsed at all,
						'' or it was followed by "junk" tokens...
						astDelete( expr )
						while( tkGet( x ) <> TK_END )
							x += 1
						wend
						astAddChild( t, tkToAstText( begin + 1, x - 1 ) )
					end if
				end if

				'' END
				assert( tkGet( x ) = TK_END )
				tkRemove( begin, x )
				x = begin

			case ASTCLASS_PPIF, ASTCLASS_PPELSEIF
				'' No #if expression yet?
				if( t->head = NULL ) then
					'' BEGIN
					assert( tkGet( x ) = TK_BEGIN )
					var begin = x
					x += 1

					'' Expression tokens
					var expr = ppExpression( x )
					'' TK_END not reached after ppExpression()?
					if( tkGet( x ) <> TK_END ) then
						'' Then either no expression could be parsed at all,
						'' or it was followed by "junk" tokens...
						astDelete( expr )

						do
							x += 1
						loop while( tkGet( x ) <> TK_END )

						'' Turn it into a PPUNKNOWN
						astAddChild( t, tkToAstText( begin + 1, x - 1 ) )
						t = astNew( ASTCLASS_PPUNKNOWN, astClone( t ) )
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

namespace eval
	dim shared as THASH symbols
	dim shared as ASTNODE ptr macro
end namespace

enum
	COND_UNKNOWN = 0
	COND_TRUE
	COND_FALSE
end enum

sub ppAddSymbol( byval id as zstring ptr, byval is_defined as integer )
	hashAddOverwrite( @eval.symbols, id, cptr( any ptr, is_defined ) )
end sub

sub ppEvalInit( )
	hashInit( @eval.symbols, 4 )
end sub

sub ppEvalEnd( )
	hashEnd( @eval.symbols )
end sub

sub ppMacroBegin( byval id as zstring ptr, byval params as integer )
	assert( params > 0 )
	eval.macro = astNew( ASTCLASS_PPMACRO, id )
	eval.macro->macroparams = params
end sub

sub ppMacroToken( byval tk as integer, byval text as zstring ptr )
	var n = astNew( ASTCLASS_TK, text )
	n->tk = tk
	astAddChild( eval.macro, n )
end sub

sub ppMacroParam( byval index as integer )
	assert( (index >= 0) and (index < eval.macro->macroparams) )
	var n = astNew( ASTCLASS_MACROPARAM )
	n->macroparam = index
	astAddChild( eval.macro, n )
end sub

private function hMacroCall( byval x as integer ) as integer
	var begin = x

	'' ID
	x += 1

	'' '('?
	if( tkGet( x ) <> TK_LPAREN ) then
		return begin
	end if
	x += 1

	const MAXARGS = 32
	dim as integer argbegin(0 to MAXARGS-1), argend(0 to MAXARGS-1)
	var args = 0

	'' For each arg...
	do
		if( args = MAXARGS ) then
			return begin
		end if

		argbegin(args) = x

		'' For each token that's part of this arg...
		var level = 0
		do
			select case( tkGet( x ) )
			case TK_LPAREN
				level += 1
			case TK_RPAREN
				if( level <= 0 ) then
					exit do
				end if
				level -= 1
			case TK_COMMA
				if( level <= 0 ) then
					exit do
				end if
			case TK_EOF
				return begin
			end select
			x += 1
		loop

		argend(args) = x - 1
		args += 1

		'' ','?
		if( tkGet( x ) <> TK_COMMA ) then
			exit do
		end if
		x += 1
	loop

	'' ')'?
	if( tkGet( x ) <> TK_RPAREN ) then
		return begin
	end if
	x += 1

	'' As many args as params?
	if( eval.macro->macroparams <> args ) then
		return begin
	end if

	'' Insert the macro body behind the call
	var callend = x - 1
	var child = eval.macro->head
	while( child )
		select case( child->class )
		case ASTCLASS_TK
			tkInsert( x, child->tk, child->text )
			x += 1
		case ASTCLASS_MACROPARAM
			var arg = child->macroparam
			assert( (arg >= 0) and (arg < args) )
			'' Copy the arg's tokens into the body
			for i as integer = argbegin(arg) to argend(arg)
				tkInsert( x, tkGet( i ), tkGetText( i ) )
				x += 1
			next
		case else
			assert( FALSE )
		end select
		child = child->next
	wend

	'' Then remove the call tokens
	tkRemove( begin, callend )
	x -= callend - begin + 1

	function = x
end function

private sub hExpandMacro( )
	var x = 0
	while( tkGet( x ) <> TK_EOF )
		if( tkGet( x ) = TK_ID ) then
			if( *tkGetText( x ) = *eval.macro->text ) then
				x = hMacroCall( x )
			end if
		end if
		x += 1
	wend
end sub

sub ppMacroEnd( )
	hExpandMacro( )
	astDelete( eval.macro )
	eval.macro = NULL
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function hLookupSymbol( byval id as zstring ptr ) as integer
	var item = hashLookup( @eval.symbols, id, hashHash( id ) )
	if( item->s ) then
		function = iif( item->data, COND_TRUE, COND_FALSE )
	else
		function = COND_UNKNOWN
	end if
end function

private function hFold( byval n as ASTNODE ptr ) as ASTNODE ptr
	function = n

	if( n = NULL ) then
		exit function
	end if

	var child = n->head
	while( child )
		child = astReplaceChild( n, child, hFold( astClone( child ) ) )
		child = child->next
	wend

	select case as const( n->class )
	case ASTCLASS_DEFINED
		'' defined() on known symbol?
		assert( n->head->class = ASTCLASS_ID )
		var cond = hLookupSymbol( n->head->text )
		if( cond <> COND_UNKNOWN ) then
			'' defined()    ->    1|0
			function = astNewCONST( iif( cond = COND_TRUE, 1, 0 ), 0, TYPE_LONG )
			astDelete( n )
		end if

	case ASTCLASS_IIF
		if( n->head->class = ASTCLASS_CONST ) then
			assert( typeIsFloat( n->dtype ) = FALSE )
			if( n->val.i ) then
				function = astClone( n->head->next )
			else
				function = astClone( n->tail )
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

		if( (n->head->class = ASTCLASS_CONST) and _
		    (n->tail->class = ASTCLASS_CONST) ) then
			assert( typeIsFloat( n->head->dtype ) = FALSE )
			assert( typeIsFloat( n->tail->dtype ) = FALSE )
			var v1 = n->head->val.i
			var v2 = n->tail->val.i

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

			function = astNewCONST( v1, 0, TYPE_LONG )
			astDelete( n )

		'' Check for short-curcuiting for || and &&,
		'' if only the lhs is a CONST
		elseif( (n->head->class = ASTCLASS_CONST) and _
		        (n->tail->class <> ASTCLASS_CONST) ) then
			select case( n->class )
			case ASTCLASS_LOGOR, ASTCLASS_LOGAND
				assert( typeIsFloat( n->head->dtype ) = FALSE )
				var v1 = n->head->val.i

				'' 1 || unknown -> 1
				'' 0 && unknown -> 0
				if( v1 = iif( n->class = ASTCLASS_LOGOR, 1, 0 ) ) then
					function = astNewCONST( v1, 0, TYPE_LONG )
					astDelete( n )
				end if
			end select
		end if

	case ASTCLASS_LOGNOT, ASTCLASS_BITNOT, _
	     ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS

		if( n->head->class = ASTCLASS_CONST ) then
			assert( typeIsFloat( n->head->dtype ) = FALSE )
			var v1 = n->head->val.i

			select case as const( n->class )
			case ASTCLASS_LOGNOT    : v1 = iif( v1, 0, 1 )
			case ASTCLASS_BITNOT    : v1 = not v1
			case ASTCLASS_NEGATE    : v1 = -v1
			case ASTCLASS_UNARYPLUS : '' nothing to do
			case else
				assert( FALSE )
			end select

			function = astNewCONST( v1, 0, TYPE_LONG )
			astDelete( n )
		end if

	end select
end function

sub ppEvalExpressions( )
	var x = 0

	while( tkGet( x ) <> TK_EOF )
		if( tkGet( x ) = TK_AST ) then
			var t = tkGetAst( x )

			select case( t->class )
			case ASTCLASS_PPIF, ASTCLASS_PPELSEIF
				t = hFold( astClone( t ) )
				tkSetAst( x, t )
			end select
		end if

		x += 1
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Find corresponding #else and closing #endif, while stepping over nested
'' #if blocks.
private sub hFindElseEndIf _
	( _
		byval x as integer, _
		byref xelse as integer, _
		byref xendif as integer _
	)

	xelse = -1
	xendif = -1

	var level = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_AST
			var t = tkGetAst( x )

			select case( t->class )
			case ASTCLASS_PPIF
				level += 1

			case ASTCLASS_PPELSE
				if( level = 0 ) then
					xelse = x
				end if

			case ASTCLASS_PPENDIF
				if( level = 0 ) then
					xendif = x
					exit do
				end if
				level -= 1

			end select

		end select

		x += 1
	loop

	'' If no #else was found, use same position as for #endif
	if( xelse < 0 ) then
		xelse = xendif
	end if
end sub

'' Expand #elseifs into normal #if blocks:
''
'' 1.   #if 1       2. #if 1          3. #if 1
''      #elseif 2      #else             #else
''      #elseif 3          #if 2             #if 2
''      #else              #elseif 3         #else
''      #endif             #else                 #if 3
''                         #endif                #else
''                     #endif                    #endif
''                                           #endif
''                                       #endif
sub ppSplitElseIfs( )
	var x = 0

	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_AST
			var t = tkGetAst( x )

			'' Found an #elseif? Replace it by #else #if
			if( t->class = ASTCLASS_PPELSEIF ) then
				t = astClone( t->head )
				tkRemove( x, x )
				tkInsert( x, TK_AST, , astNew( ASTCLASS_PPELSE ) )
				x += 1
				tkInsert( x, TK_AST, , astNew( ASTCLASS_PPIF, t ) )

				'' Find the corresponding #endif,
				'' and insert another #endif in front of it
				dim as integer xelse, xendif
				hFindElseEndIf( x + 1, xelse, xendif )
				tkInsert( xendif, TK_AST, , astNew( ASTCLASS_PPENDIF ) )
			end if

			x += 1

		case else
			x += 1
		end select
	loop
end sub

'' Solve out #if/#else/#endif blocks if the condition is known to be TRUE/FALSE
sub ppEvalIfs( )
	var x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_AST
			var t = tkGetAst( x )
			if( t->class = ASTCLASS_PPIF ) then
				var cond = COND_UNKNOWN
				if( t->head->class = ASTCLASS_CONST ) then
					assert( typeIsFloat( t->head->dtype ) = FALSE )
					cond = iif( t->head->val.i <> 0, COND_TRUE, COND_FALSE )
				end if

				if( cond <> COND_UNKNOWN ) then
					dim as integer xelse, xendif
					hFindElseEndIf( x + 1, xelse, xendif )

					if( cond = COND_TRUE ) then
						'' Remove whole #else..#endif block and the #if
						tkRemove( xelse, xendif )
						tkRemove( x, x )
					else
						if( xelse = xendif ) then
							'' No #else found; remove whole #if..#endif block
							tkRemove( x, xendif )
						else
							'' Remove whole #if..#else block and the #endif
							tkRemove( xendif, xendif )
							tkRemove( x, xelse )
						end if
					end if

					x -= 1
				end if
			end if

		end select

		x += 1
	loop
end sub

'' Merge #else #if back into #elseifs
sub ppMergeElseIfs( )
	var x = 0

	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_AST
			'' Found an #else followed by an #if?
			var t = tkGetAst( x )
			if( t->class = ASTCLASS_PPELSE ) then
				if( tkGet( x + 1 ) = TK_AST ) then
					t = tkGetAst( x + 1 )
					if( t->class = ASTCLASS_PPIF ) then
						t = astClone( t->head )
						tkRemove( x, x + 1 )
						tkInsert( x, TK_AST, , astNew( ASTCLASS_PPELSEIF, t ) )

						'' Find the corresponding #endif and remove it
						dim as integer xelse, xendif
						hFindElseEndIf( x + 1, xelse, xendif )
						tkRemove( xendif, xendif )
					end if
				end if
			end if

			x += 1

		case else
			x += 1
		end select
	loop
end sub

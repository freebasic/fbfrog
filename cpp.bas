''
'' Token buffer preprocessing
'' --------------------------
''
'' cppComments() assigns comments from TK_COMMENTs (if any exist even, depending
'' on whether lexLoadC() was asked to preserve them or not) to other,
'' non-whitespace, tokens. It tries to be smart and effectively assign comments
'' to corresponding high-level constructs. For example, if a comment is found
'' at the end of a non-empty line, it will be given to the last non-whitespace
'' token in that line. This way, the C parser later won't be disturbed by any
'' TK_COMMENTs and can easily collect assigned comments from the tokens of
'' high-level constructs.
''
'' cppDividers() merges empty lines (i.e. multiple TK_EOLs) into TK_DIVIDERs,
'' for nicer output formatting later. It can be nice to preserve the
'' block/section/paragraph layout of the input, and still trim down unnecessary
'' newlines.
''
''
'' CPP directive parsing, #if evaluation, macro expansion
'' ------------------------------------------------------
''
'' cppIdentifyDirectives() parses the input tokens, looking for CPP directives.
'' For any directives found, it merges the tokens into single TK_PP* tokens,
'' to make further parsing easier.
''
'' #define bodies are loaded into ASTs which is attached to the TK_PPDEFINE
'' token, and temporarily re-inserted into the tk buffer when it needs to be
'' parsed by macro expansion code or later the C parser.
''
'' #if expressions are not yet parsed though, but only enclosed in TK_BEGIN and
'' TK_END tokens, so they can later be macro-expanded and then parsed by
'' cppExpression().
''
'' cppMain() goes through the token buffer much like a C preprocessor would do,
'' keeping track of #defines and #undefs, doing macro expansion, evaluating #if
'' blocks, and expanding #includes.
''
'' cppNoExpandSym() can be used to disable macro expansion for certain symbols.
'' This should be pretty rare though; usually in headers where function
'' declarations etc. are obfuscated by macros, they're going to need to be
'' expanded.
''
'' cppRemoveSym() registers symbols (#defines/#undefs) which should be removed
'' instead of being preserved in the binding. Doing this on the PP level instead
'' of later in the AST is useful for #defines whose bodies can't be parsed as
'' C expressions.
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
		case TK_COMMENT

		case TK_EOL, TK_EOF
			exit do

		case TK_PPINCLUDE, TK_PPDEFINE, TK_PPIF, TK_PPELSEIF, TK_PPELSE, _
		     TK_PPENDIF, TK_PPUNDEF, TK_PPERROR, TK_PPWARNING, TK_DIVIDER
			'' High-level tokens count as separate lines
			exit do

		case else
			function = FALSE
			exit do
		end select
	loop

end function

private sub hAccumComment( byval x as integer, byval comment as zstring ptr )
	if( len( *comment ) = 0 ) then
		exit sub
	end if

	dim as string text
	var s = tkGetComment( x )
	if( s ) then
		text = *s + !"\n"
	end if

	text += *comment

	tkSetComment( x, text )
end sub

private sub hAccumTkComment( byval x as integer, byval comment as integer )
	assert( tkGet( comment ) = TK_COMMENT )
	assert( tkGet( x ) <> TK_EOF )
	hAccumComment( x, tkGetText( comment ) )
end sub

function hFindClosingParen( byval x as integer ) as integer
	var opening = tkGet( x )
	var level = 0

	dim as integer closing
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
		x = tkSkipCommentEol( x )

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

		case TK_END, _
		     TK_PPINCLUDE, TK_PPDEFINE, TK_PPIF, TK_PPELSEIF, TK_PPELSE, _
		     TK_PPENDIF, TK_PPUNDEF, TK_PPERROR, TK_PPWARNING, TK_DIVIDER
			x = tkSkipCommentEol( x, -1 )
			exit do

		end select
	loop

	function = x
end function

private function hSkipStatement( byval x as integer ) as integer
	var begin = x

	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		'' ';' (statement separator)
		case TK_SEMI
			x = tkSkipCommentEol( x )
			exit do

		'' '#' usually starts PP directives, so that'd be the beginning
		'' of a new statement, unless we're skipping that '#' itself.
		case TK_HASH
			if( x <= begin ) then
				'' Otherwise, skip the whole PP directive -- until EOL
				do
					x = tkSkipComment( x )

					select case( tkGet( x ) )
					case TK_EOF
						exit do
					case TK_EOL
						'' Skip to next non-space token behind the EOL
						x = tkSkipCommentEol( x )
						exit do
					end select
				loop
			end if

			exit do

		case TK_LPAREN, TK_LBRACKET, TK_LBRACE
			x = hFindClosingParen( x )
		end select

		x = tkSkipCommentEol( x )
	loop

	assert( iif( tkGet( begin ) <> TK_EOF, x > begin, TRUE ) )
	function = x
end function

private function cppComment( byval x as integer ) as integer
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

	var at_bol = hIsBeforeEol( x, -1 )
	var at_eol = hIsBeforeEol( x,  1 )

	var xnext = tkSkipComment( x )
	var xprev = tkSkipComment( x, -1 )

	if( at_bol and at_eol ) then
		var xnextnonspace = tkSkipCommentEol( x )
		var xnextstmt = hSkipStatement( x )

		'' Comment above empty line (ie. above two EOLs or EOL & EOF)?
		if( (tkCount( TK_EOL, x + 1, xnextnonspace ) >= 2) or _
		    ((tkGet(                xnext   ) = TK_EOL) and _
		     (tkGet( tkSkipComment( xnext ) ) = TK_EOF)) ) then
			hAccumTkComment( xnext, x )

		'' Comment above multiple consecutive statements? (not separated by empty lines)
		elseif( (xnextstmt < hSkipStatement( xnextstmt )) and _
		        (tkCount( TK_EOL, tkSkipCommentEol( xnextstmt, -1 ) + 1, xnextstmt - 1 ) < 2) ) then
			hAccumTkComment( xnext, x )

		'' Comment(s) is/are the only token(s) in the file?
		elseif( (tkGet( xprev ) = TK_EOF) and (tkGet( xnext ) = TK_EOF) ) then
			'' Insert a TK_EOL to hold the comment
			tkInsert( x, TK_EOL )
			x += 1
			hAccumTkComment( x - 1, x )

		'' Comment at EOF?
		elseif( tkGet( xnext ) = TK_EOF ) then
			hAccumTkComment( xprev, x )

		'' Comment above single statement
		else
			hAccumTkComment( xnextnonspace, x )
		end if

	elseif( at_bol ) then
		hAccumTkComment( xnext, x )

	elseif( at_eol ) then
		'' If behind a ',' (parameter/declarator list) or ';',
		'' assign to token before that, making it easier to handle in
		'' the parser, because it only has to collect tokens from the
		'' core of a declaration, not the separator tokens like ',' or
		'' ';' which are typically handled by separate functions.
		select case( tkGet( xprev ) )
		case TK_COMMA, TK_SEMI
			'' Unless that ',' or ';' is the only token in this line
			'' (which should be rare though in practice)
			var xprevprev = tkSkipComment( xprev, -1 )
			select case( xprevprev )
			case TK_EOL, TK_EOF

			case else
				xprev = xprevprev
			end select
		end select

		hAccumTkComment( xprev, x )

	else
		hAccumTkComment( xnext, x )
	end if

	tkRemove( x, x )
	x -= 1

	function = x
end function

private sub cppComments( )
	var x = 0
	while( tkGet( x ) <> TK_EOF )
		if( tkGet( x ) = TK_COMMENT ) then
			x = cppComment( x )
		end if
		x += 1
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private sub cppDividers( )
	var x = 0

	while( tkGet( x ) <> TK_EOF )
		var begin = x

		'' Count empty lines in a row
		var lines = 0
		while( tkGet( x ) = TK_EOL )
			lines += 1
			x += 1
		wend

		if( lines >= 1 ) then
			'' Merge empty lines into TK_DIVIDER, assuming we're starting at BOL.
			''
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

			var blockcomment = tkCollectComments( x - 1, x - 1 )

			var comment = tkCollectComments( begin, x - 2 )
			tkFold( begin, x - 1, TK_DIVIDER, blockcomment )
			tkSetComment( begin, comment )
			x = begin
		else
			'' Skip to next BOL
			x = begin
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
		end if
	wend

	'' And remove DIVIDERs again inside statements, otherwise the C parser
	'' will choke on them...
	x = 0
	while( tkGet( x ) <> TK_EOF )
		var y = hSkipStatement( x ) - 1

		'' But don't touch DIVIDERs at begin/end of statement, only
		'' remove those in the middle, if any
		while( (x < y) and (tkGet( x ) = TK_DIVIDER) )
			x += 1
		wend

		while( (x < y) and (tkGet( y ) = TK_DIVIDER) )
			y -= 1
		wend

		while( x < y )
			if( tkGet( x ) = TK_DIVIDER ) then
				tkRemove( x, x )
				x -= 1
				y -= 1
			end if
			x += 1
		wend

		x = tkSkipCommentEol( x )
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' #if expression parser used by cppMain()

function hNumberLiteral( byval x as integer ) as ASTNODE ptr
	dim as ASTNODE ptr n

	select case( tkGet( x ) )
	case TK_DECNUM
		n = astNewCONSTI( vallng( *tkGetText( x ) ), TYPE_LONGINT )
	case TK_HEXNUM
		n = astNewCONSTI( vallng( "&h" + *tkGetText( x ) ), TYPE_LONGINT )
		n->attrib or= ASTATTRIB_HEX
	case TK_OCTNUM
		n = astNewCONSTI( vallng( "&o" + *tkGetText( x ) ), TYPE_LONGINT )
		n->attrib or= ASTATTRIB_OCT
	case TK_DECFLOAT
		n = astNewCONSTF( val( *tkGetText( x ) ), TYPE_DOUBLE )
	case else
		assert( FALSE )
	end select

	function = n
end function

'' C operator precedence, starting at 1, higher value = higher precedence
dim shared as integer cprecedence(ASTCLASS_CLOGOR to ASTCLASS_IIF) = _
{ _
	 2, _ '' ASTCLASS_CLOGOR
	 3, _ '' ASTCLASS_CLOGAND
	 0, _ '' ASTCLASS_ORELSE (unused)
	 0, _ '' ASTCLASS_ANDALSO (unused)
	 4, _ '' ASTCLASS_OR
	 5, _ '' ASTCLASS_XOR
	 6, _ '' ASTCLASS_AND
	 7, _ '' ASTCLASS_CEQ
	 7, _ '' ASTCLASS_CNE
	 8, _ '' ASTCLASS_CLT
	 8, _ '' ASTCLASS_CLE
	 8, _ '' ASTCLASS_CGT
	 8, _ '' ASTCLASS_CGE
	 0, _ '' ASTCLASS_EQ (unused)
	 0, _ '' ASTCLASS_NE (unused)
	 0, _ '' ASTCLASS_LT (unused)
	 0, _ '' ASTCLASS_LE (unused)
	 0, _ '' ASTCLASS_GT (unused)
	 0, _ '' ASTCLASS_GE (unused)
	 9, _ '' ASTCLASS_SHL
	 9, _ '' ASTCLASS_SHR
	10, _ '' ASTCLASS_ADD
	10, _ '' ASTCLASS_SUB
	11, _ '' ASTCLASS_MUL
	11, _ '' ASTCLASS_DIV
	11, _ '' ASTCLASS_MOD
	13, _ '' ASTCLASS_INDEX
	13, _ '' ASTCLASS_MEMBER
	13, _ '' ASTCLASS_MEMBERDEREF
	 0, _ '' ASTCLASS_STRCAT
	12, _ '' ASTCLASS_CLOGNOT
	12, _ '' ASTCLASS_NOT
	12, _ '' ASTCLASS_NEGATE
	12, _ '' ASTCLASS_UNARYPLUS
	 0, _ '' ASTCLASS_CDEFINED (unused)
	 0, _ '' ASTCLASS_DEFINED (unused)
	12, _ '' ASTCLASS_ADDROF
	12, _ '' ASTCLASS_DEREF
	 0, _ '' ASTCLASS_STRINGIFY (unused)
	12, _ '' ASTCLASS_SIZEOF
	 0, _ '' ASTCLASS_CAST (unused)
	 1  _ '' ASTCLASS_IIF
}

'' C PP expression parser based on precedence climbing
private function cppExpression _
	( _
		byref x as integer, _
		byval level as integer = 0 _
	) as ASTNODE ptr

	'' Unary prefix operators
	var op = -1
	select case( tkGet( x ) )
	case TK_EXCL  : op = ASTCLASS_CLOGNOT   '' !
	case TK_TILDE : op = ASTCLASS_NOT       '' ~
	case TK_MINUS : op = ASTCLASS_NEGATE    '' -
	case TK_PLUS  : op = ASTCLASS_UNARYPLUS '' +
	end select

	dim as ASTNODE ptr a
	if( op >= 0 ) then
		var uopx = x
		x += 1
		a = astTakeLoc( astNewUOP( op, cppExpression( x, cprecedence(op) ) ), uopx )
	else
		'' Atoms
		select case( tkGet( x ) )
		'' '(' Expression ')'
		case TK_LPAREN
			'' '('
			x += 1

			'' Expression
			a = cppExpression( x )

			'' ')'
			tkExpect( x, TK_RPAREN, "for '(...)' parenthesized expression" )
			x += 1

		'' Number literals
		case TK_OCTNUM, TK_DECNUM, TK_HEXNUM, TK_DECFLOAT
			a = astTakeLoc( hNumberLiteral( x ), x )
			x += 1

		'' Identifier
		case TK_ID
			'' Accepting identifiers as atoms, so that they can
			'' later be replaced with literal zeros (unexpanded
			'' identifiers in #if conditions are treated as literal
			'' zero in C) if they need to be evaluated, perhaps with
			'' a warning, and otherwise be ignored (depends on NOP
			'' folding). For example:
			''    #if defined A && B == 123
			'' If A isn't defined then B doesn't need to be
			'' evaluated and no warning should be shown.
			a = astTakeLoc( astNewID( tkGetText( x ) ), x )
			x += 1

		'' DEFINED '(' Identifier ')'
		case KW_DEFINED
			var definedx = x
			x += 1

			'' '('
			var have_parens = FALSE
			if( tkGet( x ) = TK_LPAREN ) then
				have_parens = TRUE
				x += 1
			end if

			'' Identifier
			tkExpect( x, TK_ID, "as operand of DEFINED" )
			a = astTakeLoc( astNewID( tkGetText( x ) ), x )
			x += 1

			if( have_parens ) then
				'' ')'
				tkExpect( x, TK_RPAREN, "for DEFINED(...)" )
				x += 1
			end if

			a = astTakeLoc( astNewUOP( ASTCLASS_CDEFINED, a ), definedx )

		case else
			tkOopsExpected( x, "number literal or '(...)' (atom expression)" )
		end select
	end if

	'' Infix operators
	do
		select case as const( tkGet( x ) )
		case TK_QUEST    : op = ASTCLASS_IIF     '' ? (a ? b : c)
		case TK_PIPEPIPE : op = ASTCLASS_CLOGOR  '' ||
		case TK_AMPAMP   : op = ASTCLASS_CLOGAND '' &&
		case TK_PIPE     : op = ASTCLASS_OR      '' |
		case TK_CIRC     : op = ASTCLASS_XOR     '' ^
		case TK_AMP      : op = ASTCLASS_AND     '' &
		case TK_EQEQ     : op = ASTCLASS_CEQ     '' ==
		case TK_EXCLEQ   : op = ASTCLASS_CNE     '' !=
		case TK_LT       : op = ASTCLASS_CLT     '' <
		case TK_LTEQ     : op = ASTCLASS_CLE     '' <=
		case TK_GT       : op = ASTCLASS_CGT     '' >
		case TK_GTEQ     : op = ASTCLASS_CGE     '' >=
		case TK_LTLT     : op = ASTCLASS_SHL     '' <<
		case TK_GTGT     : op = ASTCLASS_SHR     '' >>
		case TK_PLUS     : op = ASTCLASS_ADD     '' +
		case TK_MINUS    : op = ASTCLASS_SUB     '' -
		case TK_STAR     : op = ASTCLASS_MUL     '' *
		case TK_SLASH    : op = ASTCLASS_DIV     '' /
		case TK_PERCENT  : op = ASTCLASS_MOD     '' %
		case else        : exit do
		end select

		'' Higher/same level means process now (takes precedence),
		'' lower level means we're done and the parent call will
		'' continue. The first call will start with level 0.
		var oplevel = cprecedence(op)
		if( oplevel < level ) then
			exit do
		end if
		'' Left associative?
		if( op <> ASTCLASS_IIF ) then
			oplevel += 1
		end if

		'' operator
		var bopx = x
		x += 1

		'' rhs
		var b = cppExpression( x, oplevel )

		'' Handle ?: special case
		if( op = ASTCLASS_IIF ) then
			'' ':'?
			tkExpect( x, TK_COLON, "for a?b:c iif operator" )
			x += 1

			var c = cppExpression( x, oplevel )

			a = astNewIIF( a, b, c )
		else
			a = astNewBOP( op, a, b )
		end if
		astTakeLoc( a, bopx )
	loop

	function = a
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub hMacroParamList( byref x as integer, byval t as ASTNODE ptr )
	assert( tkGet( x ) >= TK_ID )
	t->paramcount = -1
	x += 1

	'' '(' following directly behind the macro id, no spaces in between?
	if( (tkGet( x ) = TK_LPAREN) and (not tkGetBehindSpace( x )) ) then
		'' '('
		x += 1
		t->paramcount = 0

		'' List of macro parameters:
		'' Identifier (',' Identifier)*
		do
			'' macro parameter's Identifier
			if( tkGet( x ) <> TK_ID ) then
				exit do
			end if
			astAppend( t, astNew( ASTCLASS_MACROPARAM, tkGetText( x ) ) )
			t->paramcount += 1
			x += 1

			'' ','?
			if( tkGet( x ) <> TK_COMMA ) then
				exit do
			end if
			x += 1
		loop

		'' ')'?
		tkExpect( x, TK_RPAREN, "to close the parameter list in this macro declaration" )
		x += 1
	end if
end sub

private function cppDirective( byval x as integer ) as integer
	var begin = x

	'' '#'
	assert( tkGet( x ) = TK_HASH )
	x += 1

	var tk = tkGet( x )

	select case( tk )
	'' DEFINE Identifier ['(' ParameterList ')'] Body Eol
	case KW_DEFINE
		x += 1

		'' Identifier? (keywords should be allowed to, so anything >= TK_ID)
		if( tkGet( x ) < TK_ID ) then
			tkExpect( x, TK_ID, "behind #define" )
		end if
		var macro = astNew( ASTCLASS_PPDEFINE, tkGetIdOrKw( x ) )

		hMacroParamList( x, macro )

		'' Load body tokens into AST
		assert( macro->expr = NULL )
		macro->expr = astNew( ASTCLASS_MACROBODY )
		do
			select case( tkGet( x ) )
			case TK_EOL, TK_EOF
				exit do
			end select

			astAppend( macro->expr, astNewTK( x ) )

			x += 1
		loop

		tkFold( begin, x - 1, TK_PPDEFINE )
		tkSetAst( begin, macro )
		x = begin + 1

	case KW_INCLUDE
		x += 1

		'' "filename"
		tkExpect( x, TK_STRING, "containing the #include file name" )

		tkFold( begin, x, TK_PPINCLUDE, tkGetText( x ) )
		x = begin + 1

	case KW_IF, KW_ELIF
		tkFold( begin, x, iif( tk = KW_IF, TK_PPIF, TK_PPELSEIF ) )
		x = begin + 1

		'' Enclose #if condition expression tokens in TK_BEGIN/END
		tkInsert( x, TK_BEGIN )
		x += 1
		var exprbegin = x
		do
			select case( tkGet( x ) )
			case TK_EOL, TK_EOF
				exit do
			end select
			x += 1
		loop
		if( x = exprbegin ) then
			tkOopsExpected( x, "#if condition" )
		end if
		tkInsert( x, TK_END )
		x += 1

	case KW_IFDEF, KW_IFNDEF
		x += 1

		'' Identifier?
		tkExpect( x, TK_ID, iif( tk = KW_IFNDEF, @"behind #ifndef", @"behind #ifdef" ) )

		'' Build up "[!]defined id" expression
		var expr = astTakeLoc( astNewID( tkGetText( x ) ), x )
		expr = astTakeLoc( astNewUOP( ASTCLASS_CDEFINED, expr ), x - 1 )
		expr->location.column += 2  '' ifdef -> def, ifndef -> ndef
		expr->location.length = 3
		if( tk = KW_IFNDEF ) then
			expr->location.column += 1  '' ndef -> def
			expr = astTakeLoc( astNewUOP( ASTCLASS_CLOGNOT, expr ), x - 1 )
			expr->location.column += 2  '' ifndef -> n
			expr->location.length = 1
		end if

		tkFold( begin, x, TK_PPIF )
		tkSetAst( begin, expr )
		x = begin + 1

	case KW_ELSE, KW_ENDIF
		tkFold( begin, x, iif( tk = KW_ELSE, TK_PPELSE, TK_PPENDIF ) )
		x = begin + 1

	case KW_UNDEF
		x += 1

		'' Identifier?
		tkExpect( x, TK_ID, "behind #undef" )

		tkFold( begin, x, TK_PPUNDEF, tkGetText( x ) )
		x = begin + 1

	case KW_PRAGMA
		x += 1

		select case( tkGet( x ) )
		case TK_ID
			'' #pragma message("...")
			select case( *tkGetText( x ) )
			case "message"
				x += 1

				tkExpect( x, TK_LPAREN, "for #pragma message" )
				x += 1

				tkExpect( x, TK_STRING, "for #pragma message" )
				x += 1

				tkExpect( x, TK_RPAREN, "for #pragma message" )

				tkRemove( begin, x )
				x = begin

			case else
				tkOops( x, "unknown #pragma" )
			end select
		case else
			tkOops( x, "unknown #pragma" )
		end select

	'' #error|#warning ...Tokens until EOL...
	case KW_ERROR, KW_WARNING
		x += 1

		while( tkGet( x ) <> TK_EOL )
			x += 1
		wend
		x -= 1

		tkFold( begin, x, iif( tk = KW_ERROR, TK_PPERROR, TK_PPWARNING ), tkGetText( x ) )
		x = begin + 1

	case TK_EOL
		'' '#' followed by EOL (accepted by gcc/clang too)
		tkRemove( begin, x - 1 )
		x = begin

	case else
		tkOops( x, "unknown PP directive" )
	end select

	'' EOL?
	select case( tkGet( x ) )
	case TK_EOL
		tkRemove( x, x )

	case TK_EOF

	case else
		tkOopsExpected( x, "EOL behind PP directive" )
	end select

	function = x
end function

private sub cppIdentifyDirectives( )
	var x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		'' '#'
		case TK_HASH
			'' At BOL?
			select case( tkGet( tkSkipComment( x, -1 ) ) )
			case TK_EOL, TK_EOF, TK_END, TK_DIVIDER, _
			     TK_PPINCLUDE, TK_PPDEFINE, TK_PPIF, TK_PPELSEIF, _
			     TK_PPELSE, TK_PPENDIF, TK_PPUNDEF, TK_PPERROR, TK_PPWARNING
				x = cppDirective( x )
			case else
				x += 1
			end select

		case else
			x += 1
		end select
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' Macro call parsing & macro expansion functions used by cppMain()

const MAXARGS = 128
dim shared as integer argbegin(0 to MAXARGS-1), argend(0 to MAXARGS-1), argcount

private sub hMacroCallArgs _
	( _
		byref x as integer, _
		byval macro as ASTNODE ptr _
	)

	'' For each arg...
	do
		if( argcount = MAXARGS ) then
			tkOops( x, "macro call arg buffer too small, MAXARGS=" & MAXARGS )
		end if

		argbegin(argcount) = x

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
				tkOopsExpected( x, "')' to close macro call argument list" )
			end select
			x += 1
		loop

		argend(argcount) = x - 1
		argcount += 1

		'' ','?
		if( tkGet( x ) <> TK_COMMA ) then
			exit do
		end if
		x += 1
	loop

	'' As many args as params?
	if( argcount <> macro->paramcount ) then
		dim s as string
		if( argcount > macro->paramcount ) then
			s = "too many"
		else
			s = "not enough"
		end if
		s += " arguments for '" + *macro->text + "' macro call: "
		s &= argcount & " given, " & macro->paramcount & " needed"
		tkOops( x, s )
	end if
end sub

private function hStringify( byval arg as integer ) as string
	'' Turn this macro argument's tokens into a string and
	'' insert it as string literal
	dim as string s

	assert( (arg >= 0) and (arg < argcount) )
	for i as integer = argbegin(arg) to argend(arg)
		if( tkGetBehindSpace( i ) ) then
			s += " "
		end if

		select case as const( tkGet( i ) )
		case TK_ID       : s += *tkGetText( i )
		case TK_DECNUM   : s += *tkGetText( i )
		case TK_HEXNUM   : s += "0x" + *tkGetText( i )
		case TK_OCTNUM   : s += "0" + *tkGetText( i )
		case TK_DECFLOAT : s += *tkGetText( i )
		case TK_STRING   : s += """" + *tkGetText( i ) + """"
		case TK_CHAR     : s += "'" + *tkGetText( i ) + "'"
		case TK_WSTRING  : s += "L""" + *tkGetText( i ) + """"
		case TK_WCHAR    : s += "L'" + *tkGetText( i ) + "'"
		case TK_EXCL to TK_TILDE, KW__C_FIRST to KW__C_LAST
			s += *tkInfoText( tkGet( i ) )
		case else
			tkOops( i, "can't #stringify this token" )
		end select
	next

	function = s
end function

private sub hTryMergeTokens _
	( _
		byval l as integer, _
		byval r as integer, _
		byref mergetk as integer, _
		byref mergetext as string _
	)

	'' Try to merge the two tokens
	select case( tkGet( l ) )
	case is >= TK_ID
		select case( tkGet( r ) )
		'' id ## id -> id
		case is >= TK_ID
			mergetk = TK_ID
			mergetext = *tkGetIdOrKw( l ) + *tkGetIdOrKw( r )

		'' id ## decnum -> id
		case TK_DECNUM
			mergetk = TK_ID
			mergetext = *tkGetIdOrKw( l ) + *tkGetText( r )

		'' id ## hexnum -> id
		case TK_HEXNUM
			mergetk = TK_ID
			mergetext = *tkGetIdOrKw( l ) + "0x" + *tkGetText( r )

		'' id ## octnum -> id
		case TK_OCTNUM
			mergetk = TK_ID
			mergetext = *tkGetIdOrKw( l ) + "0" + *tkGetText( r )

		end select

	case TK_DECNUM
		select case( tkGet( r ) )
		'' decnum ## id -> hexnum (0##xFF)
		case is >= TK_ID
			var ltext = *tkGetText( l )
			var rtext = *tkGetIdOrKw( r )
			'' lhs must be '0', rhs must start with 'x'
			if( (ltext = "0") and (left( rtext, 1 ) = "x") ) then
				'' Rest of rhs must be only hex digits, or empty
				var hexdigits = right( rtext, len( rtext ) - 1 )
				if( strContainsNonHexDigits( hexdigits ) = FALSE ) then
					mergetk = TK_HEXNUM
					mergetext = hexdigits
				end if
			end if

		'' decnum ## decnum -> octnum (0##1), decnum (1##2)
		case TK_DECNUM
			var ltext = *tkGetText( l )
			var rtext = *tkGetText( r )
			if( ltext = "0" ) then
				if( strContainsNonOctDigits( rtext ) = FALSE ) then
					mergetk = TK_OCTNUM
					mergetext = rtext
				end if
			else
				mergetk = TK_DECNUM
				mergetext = ltext + rtext
			end if

		'' decnum ## octnum -> octnum (0##01), decnum (1##01)
		case TK_OCTNUM
			var ltext = *tkGetText( l )
			var rtext = *tkGetText( r )
			if( ltext = "0" ) then
				mergetk = TK_OCTNUM
				mergetext = rtext
			else
				mergetk = TK_DECNUM
				mergetext = ltext + "0" + rtext
			end if

		end select

	case TK_HEXNUM
		select case( tkGet( r ) )
		'' hexnum ## id -> hexnum (0xAA##BB)
		case is >= TK_ID
			var rtext = tkGetIdOrKw( r )
			if( strContainsNonHexDigits( rtext ) = FALSE ) then
				mergetk = TK_HEXNUM
				mergetext = *tkGetText( l ) + *rtext
			end if

		'' hexnum ## decnum -> hexnum (0xFF##123)
		case TK_DECNUM
			mergetk = TK_HEXNUM
			mergetext = *tkGetText( l ) + *tkGetText( r )

		'' hexnum ## octnum -> hexnum (0xFF##01)
		case TK_OCTNUM
			mergetk = TK_HEXNUM
			mergetext = *tkGetText( l ) + "0" + *tkGetText( r )

		end select

	case TK_OCTNUM
		select case( tkGet( r ) )
		'' octnum ## decnum -> octnum (01##2)
		case TK_DECNUM
			var rtext = tkGetText( r )
			if( strContainsNonOctDigits( rtext ) = FALSE ) then
				mergetk = TK_OCTNUM
				mergetext = *tkGetText( l ) + *rtext
			end if

		'' octnum ## octnum -> octnum (01##01)
		case TK_OCTNUM
			mergetk = TK_OCTNUM
			mergetext = *tkGetText( l ) + "0" + *tkGetText( r )

		end select

	end select

end sub

function hInsertMacroBody _
	( _
		byval x as integer, _
		byval macro as ASTNODE ptr _
	) as integer

	tkInsert( x, TK_BEGIN )
	x += 1

	var macrobody = macro->expr
	if( macrobody ) then
		assert( macrobody->class = ASTCLASS_MACROBODY )
		var tk = macrobody->head
		while( tk )
			assert( tk->class = ASTCLASS_TK )

			tkInsert( x, tk->tk, tk->text )
			tkSetLocation( x, @tk->location )
			x += 1

			tk = tk->next
		wend
	end if

	tkInsert( x, TK_END )
	x += 1

	function = x
end function

private sub hInsertMacroExpansion _
	( _
		byref x as integer, _
		byval macro as ASTNODE ptr _
	)

	''
	'' Temporarily insert the macro body tokens from AST back into the
	'' tk buffer, allowing them to be accessed easily be index, instead of
	'' the linked list of ASTNODEs. Once done with the macro expansion, the
	'' inserted body needs to be removed again.
	''
	'' It's easiest to insert the body at the front of the tk buffer, since
	'' then only the x position has to be shifted once at the beginning and
	'' once at the end. The code below assumes that the expansion is
	'' inserted behind the macro body, so that the token positions
	'' referencing the body don't need to be adjusted every time an
	'' expansion token is inserted.
	''
	'' Also, surround the macro body with TK_BEGIN/TK_END, to allow the code
	'' below to read "out-of-bounds" by -1 or +1, which simplifies handling
	'' of '#' stringify and '##' merge operators.
	''
	const bodybegin = 0
	var bodyend = hInsertMacroBody( bodybegin, macro ) - 1
	assert( tkGet( bodybegin ) = TK_BEGIN )
	assert( tkGet( bodyend ) = TK_END )
	'' x and the positions stored into argbegin()/argend() must be updated
	x += bodyend - bodybegin + 1
	for i as integer = 0 to argcount-1
		argbegin(i) += bodyend - bodybegin + 1
		argend(i)   += bodyend - bodybegin + 1
	next

	''
	'' Notes on '##' merging:
	''
	'' The operands of '##' must be expanded in case they're macro
	'' parameters. If the corresponding arguments are empty and the macro
	'' parameters expand to nothing, then no merging is done on that side,
	'' perhaps none at all.
	''
	'' Specifically, no merging is done with preceding or following tokens
	'' coming from somewhere else besides those macro parameters.
	''
	'' If a macro parameter expands to multiple tokens, the merging operator
	'' affects the last/first token from the lhs/rhs operands respectively,
	'' not necessarily all the tokens inserted in place of the parameter(s).
	''
	'' It seems like a good idea to do parameter expansion and '##' merging
	'' in two separate passes. For this, the 1st pass
	''  - must expand empty macro parameters to a place-holder token
	''    (TK_EMPTYMACROPARAM)
	''  - and also somehow mark '##' from the macro body for merging
	''    (TK_HASHHASH turned into TK_PPMERGE)
	'' allowing the 2nd pass to
	''  - tell when a '##' operand was empty, to avoid accidentally using
	''    unrelated preceding/following tokens as '##' operand(s)
	''  - tell when a '##' came from the macro body (then it should be used
	''    for merging) or a macro argument (then no merging)
	''

	var xbegin = x

	'' Pass 1: For each token in the macro body...
	var b = bodybegin + 1
	do
		select case( tkGet( b ) )
		case TK_END
			exit do

		'' '#' stringify operator
		case TK_HASH
			b += 1

			var is_stringify = FALSE

			'' Followed by identifier?
			if( tkGet( b ) = TK_ID ) then
				'' Is it a macro parameter?
				var arg = astLookupMacroParam( macro, tkGetText( b ) )
				if( arg >= 0 ) then
					tkInsert( x, TK_STRING, hStringify( arg ) )
					x += 1
					is_stringify = TRUE
				end if
			end if

			if( is_stringify = FALSE ) then
				'' Plain '#' (probably never used in practice?)
				b -= 1
				tkCopy( x, b, b )
				x += 1
			end if

		'' '##'
		case TK_HASHHASH
			'' Insert TK_PPMERGE instead, for pass 2
			tkInsert( x, TK_PPMERGE )
			x += 1

		'' Identifier
		case TK_ID
			'' Expand if it's a macro parameter
			var arg = astLookupMacroParam( macro, tkGetText( b ) )
			if( arg >= 0 ) then
				'' Argument was empty?
				if( argbegin(arg) > argend(arg) ) then
					'' Insert place-holder, for pass 2
					tkInsert( x, TK_EMPTYMACROPARAM )
					x += 1
				else
					'' Insert the arg's tokens into the expansion
					tkCopy( x, argbegin(arg), argend(arg) )
					x += argend(arg) - argbegin(arg) + 1
				end if
			else
				'' Not a macro parameter, insert as-is
				tkCopy( x, b, b )
				x += 1
			end if

		case else
			'' Uninteresting token, insert as-is
			tkCopy( x, b, b )
			x += 1
		end select

		b += 1
	loop

	'' Pass 2: Do '##' merging in the expansion
	scope
		var y = xbegin
		while( y < x )
			'' '##' from original macro body (and not '##' from a macro argument)?
			if( tkGet( y ) = TK_PPMERGE ) then
				'' Find lhs/rhs tokens, unless they're TK_EMPTYMACROPARAMs,
				'' or don't exist at all (out-of-bounds), in case of '##'
				'' at begin and/or end of the macro body.
				var l = y - 1
				if( l < xbegin ) then
					l = -1
				elseif( tkGet( l ) = TK_EMPTYMACROPARAM ) then
					l = -1
				end if

				var r = y + 1
				if( r >= x ) then
					r = -1
				elseif( tkGet( r ) = TK_EMPTYMACROPARAM ) then
					r = -1
				end if

				if( (l >= 0) and (r >= 0) ) then
					dim as integer mergetk = -1
					dim as string mergetext

					hTryMergeTokens( l, r, mergetk, mergetext )

					if( mergetk < 0 ) then
						print tkDumpOne( l )
						print tkDumpOne( r )
						oops( "cannot merge these two tokens when expanding macro '" & *macro->text & "'" )
					end if

					'' Insert merged token in front of the '##'
					tkInsert( y, mergetk, mergetext )
					y += 1
					r += 1
					x += 1

					'' Remove l/r
					tkRemove( l, l )
					y -= 1
					r -= 1
					x -= 1
					tkRemove( r, r )
					x -= 1
				'elseif( l >= 0 ) then
					'' No rhs, simply preserve the lhs
				'elseif( r >= 0 ) then
					'' No lhs, simply preserve the rhs
				'else
					'' Nothing to merge at all
				end if

				'' '##'
				assert( tkGet( y ) = TK_PPMERGE )
				tkRemove( y, y )
				y -= 1
				x -= 1
			end if

			y += 1
		wend
	end scope

	'' Pass 3: Remove TK_EMPTYMACROPARAM place holders from the expansion
	scope
		var y = xbegin
		while( y < x )
			if( tkGet( y ) = TK_EMPTYMACROPARAM ) then
				tkRemove( y, y )
				y -= 1
				x -= 1
			end if
			y += 1
		wend
	end scope

	'' Remove the macro body that was temporarily inserted above
	tkRemove( bodybegin, bodyend )
	x -= bodyend - bodybegin + 1
end sub

private function hMacroCall _
	( _
		byval x as integer, _
		byval macro as ASTNODE ptr _
	) as integer

	var begin = x

	'' ID
	assert( tkGet( x ) = TK_ID )
	x += 1

	argcount = -1

	'' Not just "#define m"?
	if( macro->paramcount >= 0 ) then
		'' '('?
		if( tkGet( x ) <> TK_LPAREN ) then
			return FALSE
		end if
		x += 1

		argcount = 0

		'' Not just "#define m()"?
		if( macro->paramcount > 0 ) then
			'' Parse the argument list and fill the argbegin() and
			'' argend() arrays accordingly
			hMacroCallArgs( x, macro )
		end if

		'' ')'?
		tkExpect( x, TK_RPAREN, "to close macro call argument list" )
		x += 1
	end if

	var expansionbegin = x

	'' Insert the macro body behind the call (this way the positions
	'' stored in argbegin()/argend() stay valid)
	hInsertMacroExpansion( x, macro )

	'' Set expansion level on the expansion tokens,
	'' = minlevel from macro call tokens + 1
	tkSetExpansionLevel( expansionbegin, x - 1, _
		tkGetExpansionLevel( _
			tkFindTokenWithMinExpansionLevel( begin, expansionbegin - 1 ) _
		) + 1 )

	'' Then remove the call tokens
	tkRemove( begin, expansionbegin - 1 )
	x -= (expansionbegin - 1) - begin + 1

	function = TRUE
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

namespace eval
	'' Lists of known macros etc. Initial symbols can be added before
	'' cppMain(), then it will use and adjust the lists while parsing.

	'' Lookup table of #define token positions
	dim shared macros		as THASH

	dim shared noexpands		as THASH
	dim shared removes		as THASH
end namespace

sub cppInit( )
	hashInit( @eval.macros, 4, TRUE )
	hashInit( @eval.noexpands, 4, TRUE )
	hashInit( @eval.removes, 4, TRUE )
end sub

sub cppEnd( )
	scope
		'' Free all macro ASTs
		for i as integer = 0 to eval.macros.room-1
			astDelete( eval.macros.items[i].data )
		next
	end scope
	hashEnd( @eval.macros )
	hashEnd( @eval.noexpands )
	hashEnd( @eval.removes )
end sub

sub cppNoExpandSym( byval id as zstring ptr )
	hashAddOverwrite( @eval.noexpands, id, NULL )
end sub

sub cppRemoveSym( byval id as zstring ptr )
	hashAddOverwrite( @eval.removes, id, NULL )
end sub

private function hLookupMacro( byval id as zstring ptr ) as ASTNODE ptr
	var item = hashLookup( @eval.macros, id, hashHash( id ) )
	if( item->s ) then
		function = item->data
	end if
end function

private sub hRegisterMacro _
	( _
		byval id as zstring ptr, _
		byval macro as ASTNODE ptr _
	)

	var hash = hashHash( id )
	var item = hashLookup( @eval.macros, id, hash )

	'' Free existing macro AST for this id, if any
	if( item->data ) then
		astDelete( item->data )
	end if

	hashAdd( @eval.macros, item, hash, id, macro )
end sub

private sub hAddMacro( byval macro as ASTNODE ptr )
	assert( macro->class = ASTCLASS_PPDEFINE )

	'' Report conflicting #defines
	if( frog.verbose ) then
		var macro2 = hLookupMacro( macro->text )
		if( macro2 ) then
			if( astIsEqual( macro, macro2 ) = FALSE ) then
				'' Existing #define wasn't reported yet?
				if( (macro2->attrib and ASTATTRIB_REPORTED) = 0 ) then
					astReport( macro2, "conflicting #define for '" + *macro->text + "', first one:", FALSE )
					macro2->attrib or= ASTATTRIB_REPORTED
				end if

				assert( (macro->attrib and ASTATTRIB_REPORTED) = 0 )
				astReport( macro, "conflicting #define for '" + *macro->text + "', new one:", FALSE )
				macro->attrib or= ASTATTRIB_REPORTED
			end if
		end if
	end if

	hRegisterMacro( macro->text, macro )
end sub

private sub hUndefMacro( byval id as zstring ptr )
	hRegisterMacro( id, NULL )
end sub

private function hShouldExpandSym( byval id as zstring ptr ) as integer
	function = (hashLookup( @eval.noexpands, id, hashHash( id ) )->s = NULL)
end function

private sub hMaybeExpandId( byref x as integer )
	assert( tkGet( x ) = TK_ID )
	var id = tkGetText( x )
	var macro = hLookupMacro( id )
	if( macro ) then
		'' Only expand if not registered as NOEXPAND
		if( hShouldExpandSym( id ) ) then
			if( hMacroCall( x, macro ) ) then
				'' The macro call will be replaced with the body,
				'' the token at the TK_ID's position must be re-parsed.
				x -= 1
			end if
		end if
	end if
end sub

private sub hExpandInIfCondition( byval x as integer )
	assert( tkGet( x ) = TK_BEGIN )

	do
		x += 1

		select case( tkGet( x ) )
		case TK_END
			exit do

		'' DEFINED ['('] Identifier [')']
		case KW_DEFINED
			x += 1

			'' '('?
			var have_lparen = FALSE
			if( tkGet( x ) = TK_LPAREN ) then
				have_lparen = TRUE
				x += 1
			end if

			'' Identifier? (not doing any expansion here)
			if( tkGet( x ) = TK_ID ) then
				x += 1
			end if

			'' ')'?
			if( have_lparen ) then
				if( tkGet( x ) = TK_RPAREN ) then
					x += 1
				end if
			end if

			x -= 1

		'' Identifier (anything unrelated to DEFINED)
		case TK_ID
			hMaybeExpandId( x )

		end select
	loop
end sub

private function hParseIfCondition( byval x as integer ) as ASTNODE ptr
	'' BEGIN
	assert( tkGet( x ) = TK_BEGIN )
	var begin = x
	x += 1

	'' Try parsing an expression
	function = cppExpression( x )

	'' TK_END not reached after cppExpression()?
	if( tkGet( x ) <> TK_END ) then
		'' Then either no expression could be parsed at all,
		'' or it was followed by "junk" tokens...
		tkOops( begin - 1, "couldn't parse #if condition expression" )
	end if

	'' END
	assert( tkGet( x ) = TK_END )
end function

private function cppFoldKnownDefineds( byval n as ASTNODE ptr ) as ASTNODE ptr
	if( n = NULL ) then exit function
	function = n

	n->expr = cppFoldKnownDefineds( n->expr )
	n->l = cppFoldKnownDefineds( n->l )
	n->r = cppFoldKnownDefineds( n->r )

	'' defined(id)?
	if( n->class = ASTCLASS_DEFINED ) then
		assert( n->l->class = ASTCLASS_ID )
		var id = n->l->text

		var item = hashLookup( @eval.macros, id, hashHash( id ) )

		'' Known symbol?
		if( item->s ) then
			'' Currently defined?
			var is_defined = (item->data <> NULL)

			'' FB defined()    ->   -1|0
			'' item->data = is_defined
			function = astNewCONSTI( is_defined, TYPE_LONG )
			astDelete( n )
		end if
	end if
end function

private function cppFoldUnknownIds( byval n as ASTNODE ptr ) as ASTNODE ptr
	if( n = NULL ) then exit function
	function = n

	select case( n->class )
	case ASTCLASS_ID
		'' Unexpanded identifier, assume it's undefined, like a CPP
		if( frog.verbose ) then
			astReport( n, "treating unexpanded identifier '" + *n->text + "' as literal zero" )
		end if

		'' id   ->   0
		function = astNewCONSTI( 0, TYPE_LONGINT )
		astDelete( n )

	case ASTCLASS_DEFINED
		'' Unsolved defined(), must be an unknown symbol, so it
		'' should expand to FALSE. (see also astFoldKnownDefineds())
		assert( n->l->class = ASTCLASS_ID )
		if( frog.verbose ) then
			astReport( n->l, "assuming symbol '" + *n->l->text + "' is undefined" )
		end if

		'' defined()   ->   0
		function = astNewCONSTI( 0, TYPE_LONGINT )
		astDelete( n )

	case else
		n->expr = cppFoldUnknownIds( n->expr )
		n->l = cppFoldUnknownIds( n->l )
		n->r = cppFoldUnknownIds( n->r )
	end select
end function

'' Folding for CPP #if condition expressions
private function cppFold( byval n as ASTNODE ptr ) as ASTNODE ptr
	'' Solve out known defined()'s, and fold as much as possible. This
	'' may also solve out unknown defined()'s/atomic identifiers, if they're
	'' no-ops. (so that below, we have to make less assumptions)
	n = astFold( cppFoldKnownDefineds( n ), TRUE )

	'' Solve out unsolved defined()'s and atomic identifiers like a CPP,
	'' by assuming they're undefined, while showing warnings about it,
	'' and fold again.
	n = astFold( cppFoldUnknownIds( n ), TRUE )

	function = n
end function

private function hEvalIfCondition( byval x as integer ) as integer
	assert( (tkGet( x ) = TK_PPIF) or (tkGet( x ) = TK_PPELSEIF) )
	var xif = x
	var t = tkGetAst( x )

	'' No #if expression yet?
	if( t = NULL ) then
		x += 1

		'' Expand macros in the #if condition expression, before parsing
		'' it, but don't expand the "id" in "defined id".
		hExpandInIfCondition( x )

		t = hParseIfCondition( x )

		x = xif
	end if

	'' 1. Try to evaluate the condition
	t = cppFold( astOpsC2FB( astClone( t ) ) )
	tkSetAst( x, t )

	'' 2. Check the condition
	if( astIsCONSTI( t ) = FALSE ) then
		const MESSAGE = "couldn't evaluate #if condition"
		if( t->location.file ) then
			hReport( @t->location, MESSAGE, TRUE )
			end 1
		end if
		tkOops( x, MESSAGE )
	end if

	function = (t->vali <> 0)
end function

private function hLookupRemoveSym( byval id as zstring ptr ) as integer
	function = (hashLookup( @eval.removes, id, hashHash( id ) )->s <> NULL)
end function

private function hSkipFromBeginToEnd( byval x as integer ) as integer
	assert( tkGet( x ) = TK_BEGIN )
	'' Find TK_END
	do
		x += 1
	loop while( tkGet( x ) <> TK_END )
	function = x
end function

private sub hRemoveTokenAndTkBeginEnd( byref x as integer )
	var last = x

	select case( tkGet( x ) )
	case TK_PPIF, TK_PPELSEIF
		if( tkGet( x + 1 ) = TK_BEGIN ) then
			last = hSkipFromBeginToEnd( x + 1 )
		end if
	end select

	tkRemove( x, last )
	x -= 1
end sub

''
'' #if/file context stack
''
'' The stack starts out with only the toplevel file context.
'' Both #if blocks and #include contexts are put on the same stack, so that an
'' #endif found in an #include won't be able to close an #if from the parent
'' file, since the #include stack node is in the way, and must be popped first.
''
enum
	'' States:
	'' 0 = file context
	PPSTACK_IF = 1  '' #if context, fresh
	PPSTACK_TRUE    '' #if context, saw #if/#elseif TRUE (and thus, further #elseif TRUE's must be skipped)
	PPSTACK_ELSE    '' #if context, saw #else (and no further #elseif/#else can be allowed)
end enum

const MAXPPSTACK = 128
dim shared ppstack(0 to MAXPPSTACK-1) as integer

private sub hCheckStackLevel( byval x as integer, byval level as integer )
	if( level >= MAXPPSTACK ) then
		tkOops( x, "#if/#include stack too small, MAXPPSTACK=" & MAXPPSTACK )
	end if
end sub

private sub hLoadFile _
	( _
		byval x as integer, _
		byval includeloc as TKLOCATION ptr, _
		byval filename as zstring ptr, _
		byval whitespace as integer _
	)

	lexLoadC( x, filebufferFromFile( filename, includeloc ), whitespace )

	if( whitespace ) then
		cppComments( )
		cppDividers( )
	end if
	cppIdentifyDirectives( )

end sub

'' Search for #included files in one of the parent directories
'' of the context file. Usually the #include will refer to a
'' file in the same directory or in a sub-directory at the same
'' level or some levels up.
private function hSearchHeaderFile _
	( _
		byval contextfile as zstring ptr, _
		byref inctext as string _
	) as string

	if( frog.verbose ) then
		print "searching: " + inctext + " (context = " + *contextfile + ")"
	end if

	'' 1. If #including by absolute path, use it as-is
	if( pathIsAbsolute( inctext ) ) then
		return inctext
	end if

	'' 2. Relative to context file
	var incfile = pathAddDiv( pathOnly( *contextfile ) ) + inctext
	if( frog.verbose ) then
		print "trying: " + incfile
	end if
	if( hFileExists( incfile ) ) then
		return incfile
	end if

	var i = frog.incdirs->head
	while( i )

		incfile = pathAddDiv( *i->text ) + inctext
		if( frog.verbose ) then
			print "trying: " + incfile
		end if
		if( hFileExists( incfile ) ) then
			return incfile
		end if

		i = i->next
	wend

	function = ""
end function

private function hFindIncludeBOF( byval x as integer ) as integer
	do
		x -= 1
	loop while( tkGet( x ) <> TK_BEGININCLUDE )
	function = x
end function

sub cppMain _
	( _
		byval topfile as ASTNODE ptr, _
		byval whitespace as integer, _
		byval nomerge as integer _
	)

	var x = 0
	var skiplevel = MAXPPSTACK
	var level = 0

	'' Add toplevel file behind current tokens (could be pre-#defines)
	hLoadFile( tkGetCount( ), @topfile->location, topfile->text, whitespace )
	ppstack(level) = 0

	do
		select case( tkGet( x ) )
		case TK_EOF
			'' If anything is left on the stack at EOF, it can only be #ifs
			'' (#includes should be popped due to TK_ENDINCLUDE's already)
			if( level > 0 ) then
				assert( ppstack(level) >= PPSTACK_IF )
				tkOops( x, "missing #endif" )
			end if
			exit do

		case TK_ENDINCLUDE
			assert( skiplevel = MAXPPSTACK )
			assert( level > 0 )
			if( ppstack(level) >= PPSTACK_IF ) then
				tkOops( x - 1, "missing #endif in #included file" )
			end if
			level -= 1

			var begin = hFindIncludeBOF( x )

			if( nomerge ) then
				'' #include expansion wasn't requested, so remove the
				'' #included tokens now, after they've been parsed.
				'' (i.e. all #defines/#undefs were seen, but the code
				'' won't be preserved)
				tkRemove( begin, x )
				x = begin - 1
			else
				'' Remove just the TK_BEGININCLUDE/TK_ENDINCLUDE,
				'' keep the #included tokens.
				tkRemove( begin, begin )
				x -= 1
				tkRemove( x, x )
				x -= 1
			end if

		case TK_PPINCLUDE
			if( skiplevel <> MAXPPSTACK ) then
				hRemoveTokenAndTkBeginEnd( x )
			else
				var location = tkGetLocation( x )
				var context = iif( location->file, location->file->name, NULL )
				if( context = NULL ) then
					context = topfile->text
				end if
				var inctext = *tkGetText( x )
				var incfile = hSearchHeaderFile( context, inctext )

				if( len( incfile ) > 0 ) then
					print space( frog.maxversionstrlen ) + "include: " + incfile

					level += 1
					hCheckStackLevel( x, level )
					ppstack(level) = 0

					'' Leave the #include token where it is, and insert the content behind it
					var xinclude = x
					x += 1

					'' Insert this so we can go back end delete all the #included tokens easily
					tkInsert( x, TK_BEGININCLUDE )
					x += 1

					'' Insert an EOL, so cppIdentifyDirectives() can identify BOL
					'' when parsing the #included tokens, to be able to detect CPP
					'' directives at the beginning of the #included tokens.
					tkInsert( x, TK_EOL )
					x += 1

					'' Load the included file's tokens and put a TK_ENDINCLUDE behind it,
					'' so we can detect the included EOF and pop the #include context from
					'' the ppstack.
					tkInsert( x, TK_ENDINCLUDE )
					hLoadFile( x, tkGetLocation( xinclude ), incfile, whitespace )

					'' Remove the #include
					tkRemove( xinclude, xinclude )
					x -= 1

					'' Start parsing the #included content
					'' (starting behind the EOL inserted above)
					x -= 1
					assert( tkGet( x ) = TK_EOL )
				else
					print space( frog.maxversionstrlen ) + "include: " + inctext + " (not found)"
				end if
			end if

		case TK_PPIF
			level += 1
			hCheckStackLevel( x, level )
			ppstack(level) = PPSTACK_IF

			'' Not skipping? Then evaluate
			if( skiplevel = MAXPPSTACK ) then
				if( hEvalIfCondition( x ) ) then
					'' #if TRUE, don't skip
					ppstack(level) = PPSTACK_TRUE
				else
					'' #if FALSE, start skipping
					skiplevel = level
				end if
			end if

			hRemoveTokenAndTkBeginEnd( x )

		case TK_PPELSEIF
			if( ppstack(level) < PPSTACK_IF ) then
				tkOops( x, "#elif without #if" )
			end if

			if( ppstack(level) = PPSTACK_ELSE ) then
				tkOops( x, "#elif after #else" )
			end if

			'' Not skipping, or skipping due to previous #if/#elseif FALSE?
			'' Then evaluate the #elseif to check whether to continue skipping or not
			if( (skiplevel = MAXPPSTACK) or (skiplevel = level) ) then
				'' If there was a previous #if/#elseif TRUE on this level,
				'' then this #elseif must be skipped no matter what its condition is.
				if( ppstack(level) = PPSTACK_TRUE ) then
					'' Start/continue skipping
					skiplevel = level
				else
					if( hEvalIfCondition( x ) ) then
						'' #elseif TRUE, don't skip
						ppstack(level) = PPSTACK_TRUE
						skiplevel = MAXPPSTACK
					else
						'' #elseif FALSE, start/continue skipping
						skiplevel = level
					end if
				end if
			end if

			hRemoveTokenAndTkBeginEnd( x )

		case TK_PPELSE
			if( ppstack(level) < PPSTACK_IF ) then
				tkOops( x, "#else without #if" )
			end if

			if( ppstack(level) = PPSTACK_ELSE ) then
				tkOops( x, "#else after #else" )
			end if

			'' Not skipping, or skipping due to previous #if/#elseif FALSE?
			'' Then check whether to skip this #else block or not.
			if( (skiplevel = MAXPPSTACK) or (skiplevel = level) ) then
				if( ppstack(level) = PPSTACK_TRUE ) then
					'' Previous #if/#elseif TRUE, skip #else
					skiplevel = level
				else
					'' Previous #if/#elseif FALSE, don't skip #else
					skiplevel = MAXPPSTACK
				end if
			end if

			ppstack(level) = PPSTACK_ELSE

			hRemoveTokenAndTkBeginEnd( x )

		case TK_PPENDIF
			if( ppstack(level) < PPSTACK_IF ) then
				tkOops( x, "#endif without #if" )
			end if

			'' If skipping due to current level, then stop skipping.
			if( skiplevel = level ) then
				skiplevel = MAXPPSTACK
			end if

			hRemoveTokenAndTkBeginEnd( x )

			level -= 1

		case TK_PPDEFINE
			if( skiplevel <> MAXPPSTACK ) then
				hRemoveTokenAndTkBeginEnd( x )
			else
				'' Register/overwrite as known defined symbol
				var t = tkGetAst( x )
				hAddMacro( astClone( t ) )

				'' Don't preserve the #define if the symbol was registed for removal
				if( hLookupRemoveSym( t->text ) ) then
					hRemoveTokenAndTkBeginEnd( x )
				end if
			end if

		case TK_PPUNDEF
			if( skiplevel <> MAXPPSTACK ) then
				hRemoveTokenAndTkBeginEnd( x )
			else
				'' Register/overwrite as known undefined symbol
				var id = tkGetText( x )
				hUndefMacro( id )

				'' Don't preserve the #undef if the symbol was registed for removal
				if( hLookupRemoveSym( id ) ) then
					hRemoveTokenAndTkBeginEnd( x )
				end if
			end if

		case TK_PPERROR
			if( skiplevel <> MAXPPSTACK ) then
				hRemoveTokenAndTkBeginEnd( x )
			else
				'' Not using the #error's text as error message,
				'' otherwise it would be mistaken for being generated by fbfrog.
				tkOops( x, "#error" )
			end if

		case TK_PPWARNING
			if( skiplevel <> MAXPPSTACK ) then
				hRemoveTokenAndTkBeginEnd( x )
			else
				'' ditto
				tkReport( x, "#warning", TRUE )
			end if

		case TK_ID
			if( skiplevel <> MAXPPSTACK ) then
				hRemoveTokenAndTkBeginEnd( x )
			else
				hMaybeExpandId( x )
			end if

		case else
			'' Remove tokens if skipping
			if( skiplevel <> MAXPPSTACK ) then
				hRemoveTokenAndTkBeginEnd( x )
			end if
		end select

		x += 1
	loop
end sub

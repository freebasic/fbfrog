''
'' Token buffer preprocessing
'' --------------------------
''
'' ppComments() assigns comments from TK_COMMENTs (if any exist even, depending
'' on whether lexLoadFile() was asked to preserve them or not) to other,
'' non-whitespace, tokens. It tries to be smart and effectively assign comments
'' to corresponding high-level constructs. For example, if a comment is found
'' at the end of a non-empty line, it will be given to the last non-whitespace
'' token in that line. This way, the C parser later won't be disturbed by any
'' TK_COMMENTs and can easily collect assigned comments from the tokens of
'' high-level constructs.
''
'' ppDividers() merges empty lines (i.e. multiple TK_EOLs) into TK_DIVIDERs,
'' for nicer output formatting later. It can be nice to preserve the
'' block/section/paragraph layout of the input, and still trim down unnecessary
'' newlines.
''
''
'' CPP directive parsing, #if evaluation, macro expansion
'' ------------------------------------------------------
''
'' ppDirectives1() merges PP directives into TK_PP* tokens, except that #define
'' bodies and #if expressions are not yet parsed, but only enclosed in TK_BEGIN
'' and TK_END tokens. This basic identification of PP directives is enough for
'' an #include detection pre-parsing step.
''
'' ppEval() goes through the token buffer almost like a C preprocessor would do.
'' It keeps track of #defines and #undefs and expands macro calls for "precious"
'' macros. It also expands macros inside #if conditions, then parses them as
'' expressions, evaluates them, and solves out #if blocks, preserving only the
'' #if/#else paths, depending on whether the expression evaluated to TRUE/FALSE.
'' If an #if block can't be solved out because of an unknown symbol, an error
'' will be shown.
''
'' ppExpandSym() registers "precious" symbols that should be macro-expanded,
'' if ppEval() finds a corresponding #define. ppEval() doesn't expand macros
'' automatically in order to allow presets to select what to expand.
''
'' ppRemoveSym() registers symbols (#defines/#undefs) which should be removed
'' instead of being preserved in the binding. Doing this on the PP level instead
'' of later in the AST is useful for #defines whose bodies can't be parsed as
'' C expressions.
''
'' ppPreDefine() adds initial macros, including simple symbols that should be
'' treated as initially defined.
''
'' ppPreUndef() registers symbols as initially undefined.
''
'' Pre-#defines/#undefs are simply inserted at the top of the token buffer,
'' so ppEval() parses them like any other #define/#undef it finds, except that
'' pre-#defines/#undefs are also automatically registered with ppRemoveSym().
''
'' #define bodies are preserved (enclosed in TK_BEGIN/END):
''  - macros can be expanded simply by copying the tokens from the original body
''  - no need to store the macro body tokens in an AST
''  - makes the expansion easier, because tk*() functions can be used for
''    everything - no need to walk AST nodes
''  - good for C parser later which can just parse the #define bodies and
''    doesn't need to worry about re-inserting them into the tk buffer to be
''    able to parse them
''  - of course pre-#defines read in from presets must still be represented in
''    the AST before ppPreDefine() can insert them at the top of the tk buffer
''  - since the #define bodies are needed for expansion, #defines registered
''    for removal instead of C parsing can't be removed immediately - they must
''    be marked with ASTATTRIB_REMOVE so the C parser will ignore them.
''
'' ppParseIfExprOnly() is a ppEval() replacement that just parses #if
'' expressions into ASTs but doesn't evaluate/expand anything, for use by PP
'' expression parser test cases.
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

		case TK_PPINCLUDE, TK_PPDEFINE, TK_PPIF, TK_PPELSEIF, _
		     TK_PPELSE, TK_PPENDIF, TK_PPUNDEF, TK_DIVIDER
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

private function hFindClosingParen( byval x as integer ) as integer
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

		case TK_PPINCLUDE, TK_PPDEFINE, TK_PPIF, TK_PPELSEIF, _
		     TK_PPELSE, TK_PPENDIF, TK_PPUNDEF, TK_DIVIDER
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

		'' '}': usually indicates end of statement, unless we're trying
		'' to skip a '}' itself
		case TK_RBRACE
			if( x > begin ) then
				exit do
			end if

		case TK_PPINCLUDE, TK_PPDEFINE, TK_PPIF, TK_PPELSEIF, _
		     TK_PPELSE, TK_PPENDIF, TK_PPUNDEF, TK_DIVIDER
			'' Reached high-level token after having seen normals?
			if( x > begin ) then
				exit do
			end if

		case TK_LPAREN, TK_LBRACKET, TK_LBRACE
			x = hFindClosingParen( x )
		end select

		x = tkSkipCommentEol( x )
	loop

	assert( iif( tkGet( begin ) <> TK_EOF, x > begin, TRUE ) )
	function = x
end function

private function ppComment( byval x as integer ) as integer
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

sub ppComments( )
	var x = 0
	while( tkGet( x ) <> TK_EOF )
		if( tkGet( x ) = TK_COMMENT ) then
			x = ppComment( x )
		end if
		x += 1
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Merge empty lines into TK_DIVIDER, assuming we're starting at BOL.
private function ppDivider( byval x as integer ) as integer
	var begin = x

	'' Count empty lines in a row
	var lines = 0
	do
		select case( tkGet( x ) )
		case TK_EOL
			lines += 1

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

	var blockcomment = tkCollectComments( x - 1, x - 1 )

	var comment = tkCollectComments( begin, x - 2 )
	tkFold( begin, x - 1, TK_DIVIDER, blockcomment )
	tkSetComment( begin, comment )
	x = begin

	function = x
end function

sub ppDividers( )
	var x = 0
	while( tkGet( x ) <> TK_EOF )
		var old = x

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

private function ppSkipToEOL( byval x as integer ) as integer
	do
		select case( tkGet( x ) )
		case TK_EOL, TK_EOF
			exit do
		end select

		x += 1
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

'' C operator precedence, starting at 1, higher value = higher precedence
dim shared as integer cprecedence(ASTOP_IIF to ASTOP_STRINGIFY) = _
{ _
	 1, _ '' ASTOP_IIF
	 2, _ '' ASTOP_CLOGOR
	 3, _ '' ASTOP_CLOGAND
	 0, _ '' ASTOP_ORELSE (unused)
	 0, _ '' ASTOP_ANDALSO (unused)
	 4, _ '' ASTOP_OR
	 5, _ '' ASTOP_XOR
	 6, _ '' ASTOP_AND
	 7, _ '' ASTOP_CEQ
	 7, _ '' ASTOP_CNE
	 8, _ '' ASTOP_CLT
	 8, _ '' ASTOP_CLE
	 8, _ '' ASTOP_CGT
	 8, _ '' ASTOP_CGE
	 0, _ '' ASTOP_EQ (unused)
	 0, _ '' ASTOP_NE (unused)
	 0, _ '' ASTOP_LT (unused)
	 0, _ '' ASTOP_LE (unused)
	 0, _ '' ASTOP_GT (unused)
	 0, _ '' ASTOP_GE (unused)
	 9, _ '' ASTOP_SHL
	 9, _ '' ASTOP_SHR
	10, _ '' ASTOP_ADD
	10, _ '' ASTOP_SUB
	11, _ '' ASTOP_MUL
	11, _ '' ASTOP_DIV
	11, _ '' ASTOP_MOD
	13, _ '' ASTOP_INDEX
	13, _ '' ASTOP_MEMBER
	13, _ '' ASTOP_MEMBERDEREF
	12, _ '' ASTOP_CLOGNOT
	12, _ '' ASTOP_NOT
	12, _ '' ASTOP_NEGATE
	12, _ '' ASTOP_UNARYPLUS
	 0, _ '' ASTOP_CDEFINED (unused)
	 0, _ '' ASTOP_DEFINED (unused)
	12, _ '' ASTOP_ADDROF
	12, _ '' ASTOP_DEREF
	 0  _ '' ASTOP_STRINGIFY (unused)
}

'' C PP expression parser based on precedence climbing
private function ppExpression _
	( _
		byref x as integer, _
		byval level as integer = 0 _
	) as ASTNODE ptr

	'' Unary prefix operators
	var op = -1
	select case( tkGet( x ) )
	case TK_EXCL  : op = ASTOP_CLOGNOT   '' !
	case TK_TILDE : op = ASTOP_NOT       '' ~
	case TK_MINUS : op = ASTOP_NEGATE    '' -
	case TK_PLUS  : op = ASTOP_UNARYPLUS '' +
	end select

	dim as ASTNODE ptr a
	if( op >= 0 ) then
		var uopx = x
		x += 1
		a = astNewUOP( op, ppExpression( x, cprecedence(op) ) )
		a->location = *tkGetLocation( uopx )
	else
		'' Atoms
		select case( tkGet( x ) )
		'' '(' Expression ')'
		case TK_LPAREN
			'' '('
			x += 1

			'' Expression
			a = ppExpression( x )

			'' ')'
			tkExpect( x, TK_RPAREN, "for '(...)' parenthesized expression" )
			x += 1

		'' Number literals
		case TK_OCTNUM, TK_DECNUM, TK_HEXNUM, TK_DECFLOAT
			a = hNumberLiteral( x )
			a->location = *tkGetLocation( x )
			x += 1

		'' Identifier
		case TK_ID
			'' Accepting identifiers as atoms to allow more PP
			'' expressions to be parsed, such as
			''    defined FOO && FOO == 123
			'' without having to expand FOO.
			a = astNewID( tkGetText( x ) )
			a->location = *tkGetLocation( x )
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
			a = astNewID( tkGetText( x ) )
			a->location = *tkGetLocation( x )
			x += 1

			if( have_parens ) then
				'' ')'
				tkExpect( x, TK_RPAREN, "for DEFINED(...)" )
				x += 1
			end if

			a = astNewUOP( ASTOP_CDEFINED, a )
			a->location = *tkGetLocation( definedx )

		case else
			tkOopsExpected( x, "number literal or '(...)' (atom expression)", NULL )
		end select
	end if

	'' Infix operators
	do
		select case as const( tkGet( x ) )
		case TK_QUEST    : op = ASTOP_IIF     '' ? (a ? b : c)
		case TK_PIPEPIPE : op = ASTOP_CLOGOR  '' ||
		case TK_AMPAMP   : op = ASTOP_CLOGAND '' &&
		case TK_PIPE     : op = ASTOP_OR      '' |
		case TK_CIRC     : op = ASTOP_XOR     '' ^
		case TK_AMP      : op = ASTOP_AND     '' &
		case TK_EQEQ     : op = ASTOP_CEQ     '' ==
		case TK_EXCLEQ   : op = ASTOP_CNE     '' !=
		case TK_LT       : op = ASTOP_CLT     '' <
		case TK_LTEQ     : op = ASTOP_CLE     '' <=
		case TK_GT       : op = ASTOP_CGT     '' >
		case TK_GTEQ     : op = ASTOP_CGE     '' >=
		case TK_LTLT     : op = ASTOP_SHL     '' <<
		case TK_GTGT     : op = ASTOP_SHR     '' >>
		case TK_PLUS     : op = ASTOP_ADD     '' +
		case TK_MINUS    : op = ASTOP_SUB     '' -
		case TK_STAR     : op = ASTOP_MUL     '' *
		case TK_SLASH    : op = ASTOP_DIV     '' /
		case TK_PERCENT  : op = ASTOP_MOD     '' %
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
		if( op <> ASTOP_IIF ) then
			oplevel += 1
		end if

		'' operator
		var bopx = x
		x += 1

		'' rhs
		var b = ppExpression( x, oplevel )

		'' Handle ?: special case
		if( op = ASTOP_IIF ) then
			'' ':'?
			tkExpect( x, TK_COLON, "for a?b:c iif operator" )
			x += 1

			var c = ppExpression( x, oplevel )

			a = astNewIIF( a, b, c )
		else
			a = astNewBOP( op, a, b )
		end if
		a->location = *tkGetLocation( bopx )
	loop

	function = a
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub hMacroParamList( byref x as integer, byval t as ASTNODE ptr )
	assert( tkGet( x ) = TK_ID )
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

private function ppDirective( byval x as integer ) as integer
	var begin = x

	'' '#'
	assert( tkGet( x ) = TK_HASH )
	x += 1

	var tk = tkGet( x )

	select case( tk )
	'' DEFINE Identifier ['(' ParameterList ')'] Body Eol
	case KW_DEFINE
		x += 1

		'' Identifier?
		tkExpect( x, TK_ID, "behind #define" )
		var macro = astNew( ASTCLASS_PPDEFINE, tkGetText( x ) )

		hMacroParamList( x, macro )

		tkFold( begin, x - 1, TK_PPDEFINE )
		tkSetAst( begin, macro )
		x = begin + 1

		'' Enclose body tokens in TK_BEGIN/END
		tkInsert( x, TK_BEGIN )
		x += 1
		x = ppSkipToEOL( x )
		tkInsert( x, TK_END )
		x += 1

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
		x = ppSkipToEOL( x )
		if( x = exprbegin ) then
			tkOopsExpected( x, "#if condition", NULL )
		end if
		tkInsert( x, TK_END )
		x += 1

	case KW_IFDEF, KW_IFNDEF
		x += 1

		'' Identifier?
		tkExpect( x, TK_ID, iif( tk = KW_IFNDEF, @"behind #ifndef", @"behind #ifdef" ) )

		'' Build up "[!]defined id" expression
		var expr = astNewID( tkGetText( x ) )
		expr->location = *tkGetLocation( x )
		expr = astNewUOP( ASTOP_CDEFINED, expr )
		expr->location = *tkGetLocation( x - 1 )
		expr->location.column += 2  '' ifdef -> def, ifndef -> ndef
		expr->location.length = 3
		if( tk = KW_IFNDEF ) then
			expr->location.column += 1  '' ndef -> def
			expr = astNewUOP( ASTOP_CLOGNOT, expr )
			expr->location = *tkGetLocation( x - 1 )
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

	case else
		tkOops( x, "unknown PP directive" )
	end select

	'' EOL?
	select case( tkGet( x ) )
	case TK_EOL
		tkRemove( x, x )

	case TK_EOF

	case else
		tkOopsExpected( x, "EOL behind PP directive", NULL )
	end select

	function = x
end function

sub ppDirectives1( )
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
			     TK_PPELSE, TK_PPENDIF, TK_PPUNDEF
				x = ppDirective( x )
			case else
				'' Skip to next line
				x = ppSkipToEOL( x ) + 1
			end select

		case TK_PPINCLUDE, TK_PPDEFINE, TK_PPIF, TK_PPELSEIF, _
		     TK_PPELSE, TK_PPENDIF, TK_PPUNDEF, TK_DIVIDER
			x += 1

		case else
			'' Skip to next line
			x = ppSkipToEOL( x ) + 1
		end select
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

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
				tkOopsExpected( x, "')' to close macro call argument list", NULL )
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

private sub hInsertMacroExpansion _
	( _
		byref x as integer, _
		byval xmacro as integer, _
		byval macro as ASTNODE ptr _
	)

	''
	'' Note: The macro body is surrounded by TK_BEGIN/END, so it's ok to
	'' read "out-of-bounds" here by -1 or +1. This is useful to handle the
	'' '#' stringify and '##' merge operators.
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
	''  - tell when a '##' operand was empty, to avoid accidentially using
	''    unrelated preceding/following tokens as '##' operand(s)
	''  - tell when a '##' came from the macro body (then it should be used
	''    for merging) or a macro argument (then no merging)
	''

	'' Pass 1: For each token in the macro body...
	assert( tkGet( xmacro     ) = TK_PPDEFINE )
	assert( tkGet( xmacro + 1 ) = TK_BEGIN    )
	var b = xmacro + 2
	var xbegin = x
	do
		select case( tkGet( b ) )
		case TK_END
			exit do

		'' '#' stringify operator
		case TK_HASH
			b += 1

			'' Followed by identifier?
			if( tkGet( b ) = TK_ID ) then
				'' Is it a macro parameter?
				var arg = astLookupMacroParam( macro, tkGetText( b ) )
				if( arg >= 0 ) then
					tkInsert( x, TK_STRING, hStringify( arg ) )
					x += 1
				end if
			else
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

end sub

private function hMacroCall _
	( _
		byval x as integer, _
		byval xmacro as integer _
	) as integer

	assert( tkGet( xmacro ) = TK_PPDEFINE )
	var macro = tkGetAst( xmacro )

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
	hInsertMacroExpansion( x, xmacro, macro )

	'' Then remove the call tokens
	tkRemove( begin, expansionbegin - 1 )
	x -= (expansionbegin - 1) - begin + 1

	function = TRUE
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

enum
	COND_UNKNOWN = 0
	COND_TRUE
	COND_FALSE
end enum

namespace eval
	'' Lists of known symbols etc. Initial symbols can be added before
	'' ppEval(), then it will use and adjust the lists while parsing.

	'' symbols known to be defined or undefined
	dim shared knowns		as THASH

	dim shared expands		as THASH
	'' #defines/#undefs to remove instead of preserving
	dim shared removes		as THASH

	'' Lookup table of #define token positions
	dim shared macros		as THASH

	'' Token position where to insert pre-#defines/#undefs
	'' - starts out at 0, the top of the tk buffer, that's the best place
	''   for pre-#defines and pretty much the only option anyways
	'' - instead of inserting each one of the pre-#defines at position 0
	''   (they would end up in reverse order), this counter keeps advancing
	''   so pre-#defines can be inserted in the original order
	dim shared xpre          as integer
end namespace

sub ppEvalInit( )
	hashInit( @eval.knowns, 4, TRUE )
	hashInit( @eval.expands, 4, TRUE )
	hashInit( @eval.removes, 4, TRUE )
	hashInit( @eval.macros, 4, TRUE )
	eval.xpre = 0
end sub

sub ppEvalEnd( )
	hashEnd( @eval.knowns )
	hashEnd( @eval.expands )
	hashEnd( @eval.removes )
	hashEnd( @eval.macros )
end sub

sub ppExpandSym( byval id as zstring ptr )
	hashAddOverwrite( @eval.expands, id, NULL )
end sub

sub ppRemoveSym( byval id as zstring ptr )
	hashAddOverwrite( @eval.removes, id, NULL )
end sub

sub ppPreUndef( byval id as zstring ptr )
	'' Add #undef at the top of the tk buffer, where ppEval() will see it
	tkInsert( eval.xpre, TK_PPUNDEF, id )
	eval.xpre += 1
end sub

sub ppPreDefine overload( byval macro as ASTNODE ptr )
	'' Insert the #define with body tokens in TK_BEGIN/END at the top of
	'' the tk buffer, where ppEval() will see it.
	var newmacro = astClone( macro )
	astDelete( newmacro->expr )
	newmacro->expr = NULL

	tkInsert( eval.xpre, TK_PPDEFINE )
	tkSetAst( eval.xpre, newmacro )
	eval.xpre += 1

	tkInsert( eval.xpre, TK_BEGIN )
	eval.xpre += 1

	var macrobody = macro->expr
	if( macrobody ) then
		assert( macrobody->class = ASTCLASS_MACROBODY )
		var tk = macrobody->head
		while( tk )
			assert( tk->class = ASTCLASS_TK )

			tkInsert( eval.xpre, tk->tk, tk->text )
			eval.xpre += 1

			tk = tk->next
		wend
	end if

	tkInsert( eval.xpre, TK_END )
	eval.xpre += 1
end sub

sub ppPreDefine overload( byval id as zstring ptr )
	var macro = astNew( ASTCLASS_PPDEFINE, id )
	macro->paramcount = -1
	ppPreDefine( macro )
	astDelete( macro )
end sub

private function hLookupMacro( byval id as zstring ptr ) as integer
	var item = hashLookup( @eval.macros, id, hashHash( id ) )
	if( item->s ) then
		function = cint( item->data )
	else
		function = -1
	end if
end function

private sub hAddMacro( byval macro as ASTNODE ptr, byval xmacro as integer )
	assert( tkGetAst( xmacro ) = macro )

	var xexisting = hLookupMacro( macro->text )
	if( xexisting >= 0 ) then
		assert( tkGet( xexisting ) = TK_PPDEFINE )
		var existing = tkGetAst( xexisting )
		if( astIsEqualDecl( macro, existing ) = FALSE ) then
			print "1st #define:"
			TRACE( xexisting )
			print "2nd #define:"
			TRACE( xmacro )
			oops( "conflicting #define for " + *macro->text )
		end if
	end if

	hashAddOverwrite( @eval.macros, macro->text, cptr( any ptr, xmacro ) )
end sub

private sub hUndefMacro( byval id as zstring ptr )
	var xexisting = hLookupMacro( id )
	if( xexisting >= 0 ) then
		hashAddOverwrite( @eval.macros, id, cptr( any ptr, -1 ) )
	end if
end sub

private function hLookupExpandSym( byval id as zstring ptr ) as integer
	function = (hashLookup( @eval.expands, id, hashHash( id ) )->s <> NULL)
end function

private sub hMaybeExpandId( byref x as integer )
	assert( tkGet( x ) = TK_ID )
	var id = tkGetText( x )
	if( hLookupExpandSym( id ) ) then
		var xmacro = hLookupMacro( id )
		if( xmacro >= 0 ) then
			if( hMacroCall( x, xmacro ) ) then
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
	function = ppExpression( x )

	'' TK_END not reached after ppExpression()?
	if( tkGet( x ) <> TK_END ) then
		'' Then either no expression could be parsed at all,
		'' or it was followed by "junk" tokens...
		tkOops( begin - 1, "couldn't parse #if condition expression" )
	end if

	'' END
	assert( tkGet( x ) = TK_END )
end function

private function hComplainAboutExpr _
	( _
		byval n as ASTNODE ptr, _
		byref message as string _
	) as integer

	if( n->location.file ) then
		oopsLocation( @n->location, message )
		function = FALSE
	else
		function = TRUE
	end if

end function

'' Returns FALSE if it reports an error, returns TRUE to indicate the caller
'' needs to show a generic/fallback error
private function hCheckUnsolvedExpr( byval n as ASTNODE ptr ) as integer
	if( n = NULL ) then
		return TRUE
	end if

	function = FALSE

	select case as const( n->class )
	case ASTCLASS_ID
		if( hComplainAboutExpr( n, "unknown symbol, need more info" ) = FALSE ) then
			exit function
		end if

	case ASTCLASS_UOP
		if( hCheckUnsolvedExpr( n->l ) = FALSE ) then
			exit function
		end if

	case ASTCLASS_BOP
		if( hCheckUnsolvedExpr( n->l ) = FALSE ) then
			exit function
		end if
		if( hCheckUnsolvedExpr( n->r ) = FALSE ) then
			exit function
		end if

		if( (n->l->class = ASTCLASS_CONST) and _
		    (n->r->class = ASTCLASS_CONST) ) then
			if( (not typeIsFloat( n->l->dtype )) and _
			    (not typeIsFloat( n->r->dtype )) ) then
				var v2 = n->r->vali

				var divbyzero = FALSE
				select case( n->op )
				case ASTOP_DIV, ASTOP_MOD
					divbyzero = (v2 = 0)
				end select

				if( divbyzero ) then
					if( hComplainAboutExpr( n, "division by zero" ) = FALSE ) then
						exit function
					end if
				end if
			end if
		end if

	case ASTCLASS_IIF
		if( hCheckUnsolvedExpr( n->expr ) = FALSE ) then
			exit function
		end if
		if( hCheckUnsolvedExpr( n->l ) = FALSE ) then
			exit function
		end if
		if( hCheckUnsolvedExpr( n->r ) = FALSE ) then
			exit function
		end if

	end select

	function = TRUE
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
	t = astFold( astOpsC2FB( astClone( t ) ), @eval.knowns, TRUE )
	tkSetAst( x, t )

	'' 2. Check the condition
	if( (t->class <> ASTCLASS_CONST) or typeIsFloat( t->dtype ) ) then
		'' If there's an unsolved defined() in the expression, try to
		'' show an error pointing to that defined(), otherwise fallback
		'' to showing the error for the whole #if condition.
		''
		'' Note: This is done here, after astFold() has fully finished,
		'' instead of inside astFold() in the ASTOP_DEFINED handler,
		'' because there it could be too early to show an error about
		'' an unsolved defined(), afterall NOP optimizations for parent
		'' nodes may cause that unsolved defined() to be solved out
		'' completely because its result doesn't matter (can happen with
		'' short-curcuiting operators and iif).
		if( hCheckUnsolvedExpr( t ) = FALSE ) then
			if( t->location.file ) then
				oopsLocation( @t->location, "couldn't evaluate #if condition" )
			else
				tkOops( x, "couldn't evaluate #if condition" )
			end if
		end if
	end if

	function = (t->vali <> 0)
end function

private sub hAddKnownSym( byval id as zstring ptr, byval is_defined as integer )
	hashAddOverwrite( @eval.knowns, id, cptr( any ptr, is_defined ) )
end sub

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
	case TK_PPDEFINE
		last = hSkipFromBeginToEnd( x + 1 )
	end select

	tkRemove( x, last )
	x -= 1
end sub

type IFSTACKNODE
	saw_true	as integer
	saw_else	as integer
end type

const MAXIFBLOCKS = 128
dim shared ifstack(0 to MAXIFBLOCKS-1) as IFSTACKNODE

sub ppEval( )
	var x = 0
	var level = -1
	var skiplevel = MAXIFBLOCKS
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_PPIF
			if( level >= MAXIFBLOCKS-1 ) then
				tkOops( x, "#if stack too small, MAXIFBLOCKS=" & MAXIFBLOCKS )
			end if
			level += 1
			ifstack(level).saw_true = FALSE
			ifstack(level).saw_else = FALSE

			'' Not skipping? Then evaluate
			if( skiplevel = MAXIFBLOCKS ) then
				if( hEvalIfCondition( x ) ) then
					'' #if TRUE, don't skip
					ifstack(level).saw_true = TRUE
				else
					'' #if FALSE, start skipping
					skiplevel = level
				end if
			end if

			hRemoveTokenAndTkBeginEnd( x )

		case TK_PPELSEIF
			if( level < 0 ) then
				tkOops( x, "#elseif without #if" )
			elseif( ifstack(level).saw_else ) then
				tkOops( x, "#elseif after #else" )
			end if

			'' Not skipping, or skipping due to previous #if/#elseif FALSE?
			'' Then evaluate the #elseif to check whether to continue skipping or not
			if( (skiplevel = MAXIFBLOCKS) or (skiplevel = level) ) then
				'' If there was a previous #if/#elseif TRUE on this level,
				'' then this #elseif must be skipped no matter what its condition is.
				if( ifstack(level).saw_true ) then
					'' Start/continue skipping
					skiplevel = level
				else
					if( hEvalIfCondition( x ) ) then
						'' #elseif TRUE, don't skip
						ifstack(level).saw_true = TRUE
						skiplevel = MAXIFBLOCKS
					else
						'' #elseif FALSE, start/continue skipping
						skiplevel = level
					end if
				end if
			end if

			hRemoveTokenAndTkBeginEnd( x )

		case TK_PPELSE
			if( level < 0 ) then
				tkOops( x, "#else without #if" )
			end if

			if( ifstack(level).saw_else ) then
				tkOops( x, "#else after #else" )
			end if
			ifstack(level).saw_else = TRUE

			'' Not skipping, or skipping due to previous #if/#elseif FALSE?
			'' Then check whether to skip this #else block or not.
			if( (skiplevel = MAXIFBLOCKS) or (skiplevel = level) ) then
				if( ifstack(level).saw_true ) then
					'' Previous #if/#elseif TRUE, skip #else
					skiplevel = level
				else
					'' Previous #if/#elseif FALSE, don't skip #else
					skiplevel = MAXIFBLOCKS
				end if
			end if

			hRemoveTokenAndTkBeginEnd( x )

		case TK_PPENDIF
			if( level < 0 ) then
				tkOops( x, "#endif without #if" )
			end if

			'' If skipping due to current level, then stop skipping.
			if( skiplevel = level ) then
				skiplevel = MAXIFBLOCKS
			end if

			hRemoveTokenAndTkBeginEnd( x )

			level -= 1

		case TK_PPDEFINE
			if( skiplevel <> MAXIFBLOCKS ) then
				hRemoveTokenAndTkBeginEnd( x )
			else
				'' Register/overwrite as known defined symbol
				var t = tkGetAst( x )
				assert( t->class = ASTCLASS_PPDEFINE )
				hAddKnownSym( t->text, TRUE )

				'' Register #define for expansion if it's a precious symbol
				if( hLookupExpandSym( t->text ) ) then
					hAddMacro( t, x )
				end if

				'' Don't preserve the #define if the symbol was registed for removal
				if( hLookupRemoveSym( t->text ) ) then
					'' #define body may still be needed for expansion,
					'' so can't be removed yet. Instead, tell the C parser to
					'' ignore this #define instead of trying to parse it.
					''hRemoveTokenAndTkBeginEnd( x )
					t->attrib or= ASTATTRIB_REMOVE
				end if
			end if

		case TK_PPUNDEF
			if( skiplevel <> MAXIFBLOCKS ) then
				hRemoveTokenAndTkBeginEnd( x )
			else
				'' Register/overwrite as known undefined symbol
				var id = tkGetText( x )
				hAddKnownSym( id, FALSE )

				'' Forget previous #define if it's a precious symbol
				if( hLookupExpandSym( id ) ) then
					hUndefMacro( id )
				end if

				'' Don't preserve the #undef if the symbol was registed for removal
				if( hLookupRemoveSym( id ) ) then
					hRemoveTokenAndTkBeginEnd( x )
				end if
			end if

		case TK_ID
			if( skiplevel <> MAXIFBLOCKS ) then
				hRemoveTokenAndTkBeginEnd( x )
			else
				hMaybeExpandId( x )
			end if

		case else
			'' Remove tokens if skipping
			if( skiplevel <> MAXIFBLOCKS ) then
				hRemoveTokenAndTkBeginEnd( x )
			end if
		end select

		x += 1
	loop
end sub

sub ppParseIfExprOnly( byval do_fold as integer )
	var x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_PPIF, TK_PPELSEIF
			var xif = x
			var t = tkGetAst( x )

			'' No #if expression yet?
			if( t = NULL ) then
				x += 1

				t = hParseIfCondition( x )

				x = xif

				'' Remove the TK_BEGIN/END, but not the #if/#elseif token,
				'' since this pass is supposed to just parse #if expressions,
				'' but not evaluate the #if blocks.
				tkRemove( x + 1, hSkipFromBeginToEnd( x + 1 ) )
			end if

			t = astOpsC2FB( astClone( t ) )
			if( do_fold ) then
				t = astFold( t, @eval.knowns, TRUE )
			end if
			tkSetAst( x, t )

		case TK_PPDEFINE
			'' Register/overwrite as known defined symbol
			var t = tkGetAst( x )
			assert( t->class = ASTCLASS_PPDEFINE )
			hAddKnownSym( t->text, TRUE )

			'' Don't preserve the #define if the symbol was registed for removal
			if( hLookupRemoveSym( t->text ) ) then
				'' #define body may still be needed for expansion,
				'' so can't be removed yet. Instead, tell the C parser to
				'' ignore this #define instead of trying to parse it.
				''hRemoveTokenAndTkBeginEnd( x )
				t->attrib or= ASTATTRIB_REMOVE
			end if

		case TK_PPUNDEF
			'' Register/overwrite as known undefined symbol
			var id = tkGetText( x )
			hAddKnownSym( id, FALSE )

			'' Don't preserve the #undef if the symbol was registed for removal
			if( hLookupRemoveSym( id ) ) then
				hRemoveTokenAndTkBeginEnd( x )
			end if

		end select

		x += 1
	loop
end sub

sub ppRemoveEOLs( )
	var x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do
		case TK_EOL
			tkRemove( x, x )
			x -= 1
		end select
		x += 1
	loop
end sub

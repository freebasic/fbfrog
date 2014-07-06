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
'' #define bodies and #if expressions are not yet parsed, but only enclosed in
'' TK_BEGIN and TK_END tokens. #define bodies are needed for macro expansion and
'' also later for the C parser. #if expressions are macro-expanded later before
'' being parsed and evaluated.
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

declare sub hMaybeExpandMacro( byref x as integer, byval inside_ifexpr as integer )

private function hMatch( byref x as integer, byval tk as integer ) as integer
	if( tkGet( x ) = tk ) then
		x += 1
		function = TRUE
	end if
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
	case TK_LBRACE   : closing = TK_RBRACE
	case TK_LBRACKET : closing = TK_RBRACKET
	case TK_LPAREN   : closing = TK_RPAREN
	case else
		assert( FALSE )
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

		case TK_BEGIN
			x = hSkipToTK_END( x )

		case TK_END, TK_EOF
			x -= 1
			exit do
		end select
	loop

	function = x
end function

function hSkipStatement( byval x as integer ) as integer
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

private sub cppComments( byval first as integer )
	var x = first
	while( tkGet( x ) <> TK_EOF )
		if( tkGet( x ) = TK_COMMENT ) then
			x = cppComment( x )
		end if
		x += 1
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private sub cppDividers( byval first as integer )
	var x = first
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
	x = first
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
		a = astTakeLoc( astNew( op, cppExpression( x, cprecedence(op) ) ), uopx )
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
			a = astTakeLoc( astNewID( tkSpellId( x ) ), x )
			x += 1

		'' DEFINED ['('] Identifier [')']
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
			if( tkGet( x ) < TK_ID ) then
				tkExpect( x, TK_ID, "as operand of DEFINED" )
			end if
			a = astTakeLoc( astNewID( tkSpellId( x ) ), x )
			x += 1

			if( have_parens ) then
				'' ')'
				tkExpect( x, TK_RPAREN, "for DEFINED(...)" )
				x += 1
			end if

			a = astTakeLoc( astNew( ASTCLASS_CDEFINED, a ), definedx )

		case else
			tkOopsExpected( x, "expression" )
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
			a = astNew( op, a, b )
		end if
		astTakeLoc( a, bopx )
	loop

	function = a
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' MacroParameter =
''      Identifier         (named parameter)
''    | Identifier '...'   (named + variadic)
''    | '...'              (variadic, using __VA_ARGS__)
'' ('...' can only appear on the last parameter)
private function hMacroParam _
	( _
		byref x as integer, _
		byval macro as ASTNODE ptr _
	) as integer

	dim id as zstring ptr
	if( tkGet( x ) >= TK_ID ) then
		id = tkSpellId( x )
		x += 1
	end if

	'' Shouldn't have seen a '...' yet
	assert( (macro->attrib and ASTATTRIB_VARIADIC) = 0 )
	var maybevariadic = 0

	if( tkGet( x ) = TK_ELLIPSIS ) then
		x += 1
		maybevariadic = ASTATTRIB_VARIADIC
		if( id = NULL ) then
			''    #define m(a, ...)
			'' must become
			''    #define m(a, __VA_ARGS__...)
			'' in FB, because in FB, the '...' parameter of variadic macros must always have a name,
			'' and using __VA_ARGS__ for that is the easiest solution, because then we don't need to
			'' go replacing that in the macro body.
			id = @"__VA_ARGS__"
		end if
	elseif( id = NULL ) then
		'' No identifier and no '...'
		exit function
	end if

	var param = astNew( ASTCLASS_MACROPARAM, id )
	param->attrib or= maybevariadic
	astAppend( macro, param )
	macro->attrib or= maybevariadic
	macro->paramcount += 1
	function = TRUE
end function

'' Identifier <no whitespace> '(' MacroParameters ')'
private sub hMacroParamList( byref x as integer, byval macro as ASTNODE ptr )
	assert( tkGet( x ) >= TK_ID )
	assert( macro->paramcount = -1 )
	x += 1

	'' '(' following directly behind the macro id, no spaces in between?
	if( (tkGet( x ) = TK_LPAREN) and _
	    ((tkGetFlags( x ) and TKFLAG_BEHINDSPACE) = 0) ) then
		'' '('
		x += 1
		macro->paramcount = 0

		'' List of macro parameters =
		''    MacroParam (',' MacroParam)*
		do
			if( hMacroParam( x, macro ) = FALSE ) then
				exit do
			end if

			'' ','?
		loop while( hMatch( x, TK_COMMA ) )

		'' ')'?
		tkExpect( x, TK_RPAREN, "to close the parameter list in this macro declaration" )
		x += 1
	end if
end sub

private sub hEncloseTokensUntilEol( byref x as integer )
	tkInsert( x, TK_BEGIN )
	x += 1

	do
		select case( tkGet( x ) )
		case TK_EOL, TK_EOF
			exit do
		end select
		x += 1
	loop

	tkInsert( x, TK_END )
	x += 1
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
		elseif( tkGet( x ) = KW_DEFINED ) then
			tkOops( x, "'defined' cannot be used as macro name" )
		end if
		var macro = astNewPPDEFINE( tkSpellId( x ) )

		hMacroParamList( x, macro )

		tkFold( begin, x - 1, TK_PPDEFINE )
		tkSetAst( begin, macro )
		x = begin + 1

		'' Enclose body tokens in TK_BEGIN/END
		hEncloseTokensUntilEol( x )

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
		hEncloseTokensUntilEol( x )

	case KW_IFDEF, KW_IFNDEF
		x += 1

		'' Identifier
		if( tkGet( x ) < TK_ID ) then
			tkExpect( x, TK_ID, iif( tk = KW_IFNDEF, @"behind #ifndef", @"behind #ifdef" ) )
		end if

		'' Build up "[!]defined id" expression
		var expr = astTakeLoc( astNewID( tkSpellId( x ) ), x )
		expr = astTakeLoc( astNew( ASTCLASS_CDEFINED, expr ), x - 1 )
		expr->location.column += 2  '' ifdef -> def, ifndef -> ndef
		expr->location.length = 3
		if( tk = KW_IFNDEF ) then
			expr->location.column += 1  '' ndef -> def
			expr = astTakeLoc( astNew( ASTCLASS_CLOGNOT, expr ), x - 1 )
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

		'' Identifier
		if( tkGet( x ) < TK_ID ) then
			tkExpect( x, TK_ID, "behind #undef" )
		end if

		tkFold( begin, x, TK_PPUNDEF, tkSpellId( x ) )
		x = begin + 1

	case KW_PRAGMA
		x += 1

		select case( tkGet( x ) )
		case TK_ID
			'' #pragma message("...")
			select case( *tkSpellId( x ) )
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

private sub cppIdentifyDirectives( byval first as integer )
	var x = first
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

namespace eval
	'' Lists of known macros etc. Initial symbols can be added before
	'' cppMain(), then it will use and adjust the lists while parsing.

	'' Lookup table of known #define token positions (or -1 if known but
	'' undefined)
	dim shared macros		as THASH

	dim shared noexpands		as THASH
	dim shared removes		as THASH
end namespace

sub cppInit( )
	hashInit( @eval.macros, 4, TRUE )
	hashInit( @eval.noexpands, 4, TRUE )
	hashInit( @eval.removes, 4, TRUE )
end sub

private sub cppEnd( )
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

private function hLookupMacro( byval id as zstring ptr ) as integer
	var item = hashLookup( @eval.macros, id, hashHash( id ) )
	if( item->s ) then
		function = cint( item->data )
	else
		function = -1
	end if
end function

private function hIsKnownSymbol( byval id as zstring ptr ) as integer
	function = (hashLookup( @eval.macros, id, hashHash( id ) )->s <> NULL)
end function

private function hIsMacroCurrentlyDefined( byval id as zstring ptr ) as integer
	function = (hLookupMacro( id ) >= 0)
end function

private sub hRegisterMacro( byval id as zstring ptr, byval x as integer )
	assert( iif( x >= 0, tkGet( x ) = TK_PPDEFINE, TRUE ) )
	hashAddOverwrite( @eval.macros, id, cptr( any ptr, x ) )
end sub

private sub hMaybeReportConflictingDefine( byval a as ASTNODE ptr, byval b as ASTNODE ptr )
	if( astIsEqual( a, b ) ) then
		exit sub
	end if

	'' Existing #define wasn't reported yet?
	if( (b->attrib and ASTATTRIB_REPORTED) = 0 ) then
		astReport( b, "conflicting #define for '" + *a->text + "', first one:", FALSE )
		b->attrib or= ASTATTRIB_REPORTED
	end if

	assert( (a->attrib and ASTATTRIB_REPORTED) = 0 )
	astReport( a, "conflicting #define for '" + *a->text + "', new one:", FALSE )
	a->attrib or= ASTATTRIB_REPORTED
end sub

private function hShouldExpandSym( byval id as zstring ptr ) as integer
	function = (hashLookup( @eval.noexpands, id, hashHash( id ) )->s = NULL)
end function

private function hCheckForMacroCall( byval x as integer ) as integer
	assert( tkGet( x ) >= TK_ID )
	var id = tkSpellId( x )

	'' Is this id a macro?
	var xmacro = hLookupMacro( id )
	if( xmacro < 0 ) then
		return -1
	end if

	'' Only expand if not marked otherwise
	assert( tkGet( xmacro ) = TK_PPDEFINE )
	var macro = tkGetAst( xmacro )
	if( (not hShouldExpandSym( id )) or _
	    (tkGetFlags( x ) and TKFLAG_NOEXPAND) or _
	    (macro->attrib and ASTATTRIB_POISONED) ) then
		return -1
	end if

	function = xmacro
end function

const MAXARGS = 128

private sub hParseMacroCallArgs _
	( _
		byref x as integer, _
		byval macro as ASTNODE ptr, _
		byval argbegin as integer ptr, _
		byval argend as integer ptr, _
		byref argcount as integer _
	)

	'' Note: The macro call argument list must be parsed without doing
	'' macro expansion. Each argument individually must be expanded later,
	'' but not before the list has been parsed & split up into individual
	'' arguments. I.e. the commas or closing ')' cannot come from macro
	'' expansions.

	var is_variadic = ((macro->attrib and ASTATTRIB_VARIADIC) <> 0)

	'' For each arg in the input...
	var reached_lastarg = FALSE
	do
		if( argcount >= MAXARGS ) then
			tkOops( x, "macro call arg buffer too small, MAXARGS=" & MAXARGS )
		end if

		argbegin[argcount] = x

		'' Is this the argument for the last parameter of a variadic macro?
		'' We're going to read all the remaining tokens into this last argument,
		'' even commas, thus there won't be any other arguments following after this one.
		assert( (not is_variadic) or (not reached_lastarg) )
		reached_lastarg = (argcount = (macro->paramcount - 1))

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
				'' A toplevel comma ends the current arg, unless it's a "..." vararg,
				'' which just "absorbs" everything until the closing ')'.
				if( level <= 0 ) then
					if( (not is_variadic) or (not reached_lastarg) ) then
						exit do
					end if
				end if

			case TK_EOF
				tkOopsExpected( x, "')' to close macro call argument list" )
			end select

			x += 1
		loop

		argend[argcount] = x - 1
		argcount += 1

		'' ','?
	loop while( hMatch( x, TK_COMMA ) )

	'' It's ok to omit the arg(s) for the variadic parameter of a variadic macro.
	if( is_variadic and (not reached_lastarg) ) then
		if( argcount >= MAXARGS ) then
			tkOops( x, "macro call arg buffer too small, MAXARGS=" & MAXARGS )
		end if
		argbegin[argcount] = x
		argend[argcount] = x - 1
		argcount += 1
	end if

	'' Not the expected amount of args?
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

private function hParseMacroCall _
	( _
		byval x as integer, _
		byval macro as ASTNODE ptr, _
		byval argbegin as integer ptr, _
		byval argend as integer ptr, _
		byref argcount as integer _
	) as integer

	var begin = x

	'' ID
	assert( tkGet( x ) >= TK_ID )
	x += 1

	argcount = -1

	'' Not just "#define m"?
	if( macro->paramcount >= 0 ) then
		'' '('?
		if( tkGet( x ) <> TK_LPAREN ) then
			return -1
		end if
		x += 1

		argcount = 0

		'' Not just "#define m()"?
		if( macro->paramcount > 0 ) then
			'' Parse the argument list and fill the argbegin() and
			'' argend() arrays accordingly
			hParseMacroCallArgs( x, macro, argbegin, argend, argcount )
		end if

		'' ')'?
		tkExpect( x, TK_RPAREN, "to close macro call argument list" )
		x += 1
	end if

	function = x - 1
end function

'' Copy a #define body into some other place, still enclosed in TK_BEGIN/TK_END
private sub hCopyMacroBody( byval x as integer, byval xmacro as integer )
	assert( tkGet( xmacro ) = TK_PPDEFINE )
	var xmacrobegin = xmacro + 1
	var xmacroend = hSkipToTK_END( xmacrobegin )
	assert( x > xmacroend )
	tkCopy( x, xmacrobegin, xmacroend )
end sub

'' DEFINED ['('] Identifier [')']
private sub hSkipDefinedUop( byref x as integer )
	assert( tkGet( x ) = KW_DEFINED )
	x += 1

	'' '('?
	var have_lparen = FALSE
	if( tkGet( x ) = TK_LPAREN ) then
		have_lparen = TRUE
		x += 1
	end if

	'' Identifier? (not doing any expansion here)
	if( tkGet( x ) >= TK_ID ) then
		x += 1
	end if

	'' ')'?
	if( have_lparen ) then
		if( tkGet( x ) = TK_RPAREN ) then
			x += 1
		end if
	end if
end sub

private function hExpandUntilTK_END _
	( _
		byval x as integer, _
		byval inside_ifexpr as integer _
	) as integer

	do
		select case( tkGet( x ) )
		case TK_END
			exit do

		case KW_DEFINED
			'' If inside an #if condition expression, don't expand symbols behind the defined operator.
			'' According to the C standard, the handling of defined's that result from macro expansion
			'' is undefined, but gcc handles them as normal defined's, so we do too.
			if( inside_ifexpr ) then
				hSkipDefinedUop( x )
				x -= 1
			end if

		case is >= TK_ID
			hMaybeExpandMacro( x, inside_ifexpr )
		end select

		x += 1
	loop

	function = x
end function

private function hExpandInRange _
	( _
		byval first as integer, _
		byval last as integer, _
		byval inside_ifexpr as integer _
	) as integer

	if( last < first ) then
		return last
	end if

	'' Insert TK_BEGIN/TK_END around the argument's tokens, to prevent the
	'' macro call parsing functions from reading out-of-bounds.
	tkInsert( first, TK_BEGIN )
	last += 1
	last += 1
	tkInsert( last, TK_END )
	assert( tkGet( first ) = TK_BEGIN )
	assert( tkGet( last ) = TK_END )

	'' Expand anything in the range
	last = hExpandUntilTK_END( first + 1, inside_ifexpr )

	'' Remove TK_BEGIN/TK_END again
	assert( tkGet( first ) = TK_BEGIN )
	assert( tkGet( last ) = TK_END )
	tkRemove( first, first )
	last -= 1
	tkRemove( last, last )
	last -= 1

	function = last
end function

''
'' - Macro arguments must be inserted in place of macro parameters, and fully
''   macro-expanded, but only self-contained without help from tokens outside
''   the argument.
''
'' - Arguments used with # mustn't be macro-expanded, and for arguments used
''   with ##, the last/first token musn't be macro-expanded depending on whether
''   the parameter was on the lhs/rhs of the ## (but the rest of the argument's
''   tokens that aren't used by the ##, if any, must be macro-expanded).
''   I.e. macro expansion mustn't be done when parsing the arguments, but later
''   when inserting them in place of parameters, with the given restrictions.
''
'' - # or ## tokens coming from arguments must not be treated as stringify/merge
''   operators. This must be done only for # or ## in the macro body.
''
'' - #stringify operations must be solved before ## merging (e.g. <L ## #param>
''   becomes <L"argtext">)
''
'' - ## operands may be empty: if an argument is used with ##, but the argument
''   is empty, then the ## doesn't merge anything. ## with 2 empty operands
''   is removed completely. Macro body token(s) preceding/following the ##
''   operand are not taken into account for the merge. Empty ## operand doesn't
''   cause preceding/following tokens to be used instead.
''
'' - If a macro parameter expands to multiple tokens, ## affects the last/first
''   token from the lhs/rhs operands respectively, but not all the tokens
''   inserted in place of the parameter(s).
''
private function hInsertMacroExpansion _
	( _
		byval expansionbegin as integer, _
		byval xmacro as integer, _
		byval macro as ASTNODE ptr, _
		byval argbegin as integer ptr, _
		byval argend as integer ptr, _
		byval argcount as integer, _
		byval inside_ifexpr as integer _
	) as integer

	'' Insert the macro body tokens from AST into the tk buffer, surrounded
	'' with TK_BEGIN/TK_END, to allow the code below to read "out-of-bounds"
	'' by -1 or +1, which simplifies handling of # and ## operators.
	''
	'' Having the TK_END also removes the need to keep track of the end of
	'' the expansion through all the insertions/deletions done here.
	'' Instead, if we need to know the end of the expansion, we can just
	'' look for the TK_END.
	hCopyMacroBody( expansionbegin, xmacro )

	'' Solve #stringify operators (higher priority than ##, and no macro
	'' expansion done for the arg)
	var x = expansionbegin + 1
	while( tkGet( x ) <> TK_END )

		'' '#param'?
		if( tkGet( x ) = TK_HASH ) then
			'' Followed by identifier?
			if( tkGet( x + 1 ) >= TK_ID ) then
				'' Is it a macro parameter?
				var arg = astLookupMacroParam( macro, tkSpellId( x + 1 ) )
				if( arg >= 0 ) then
					'' Remove #param, and insert stringify result instead
					assert( (arg >= 0) and (arg < argcount) )
					tkFold( x, x + 1, TK_STRING, tkSpell( argbegin[arg], argend[arg] ) )
				end if
			end if
		end if

		x += 1
	wend

	'' Replace ## tokens by special internal merge operator tokens, so that
	'' ## tokens from macro arguments aren't mistaken for merge operators.
	x = expansionbegin + 1
	while( tkGet( x ) <> TK_END )

		'' '##'?
		if( tkGet( x ) = TK_HASHHASH ) then
			tkFold( x, x, TK_PPMERGE )
		end if

		x += 1
	wend

	'' Insert args into params, surrounded with TK_ARGBEGIN/END, so that
	'' - we know when an arg was empty when doing ## merging (to avoid
	''   merging with other tokens outside the arg),
	'' - we know the arg's boundaries for macro-expanding it later. (must be
	''   done after merging, because only the unmerged tokens of an arg
	''   shall be macro-expanded)
	x = expansionbegin + 1
	while( tkGet( x ) <> TK_END )

		'' Macro parameter?
		if( tkGet( x ) >= TK_ID ) then
			var arg = astLookupMacroParam( macro, tkSpellId( x ) )
			if( arg >= 0 ) then
				'' >= TK_ID
				tkRemove( x, x )

				'' TK_ARGBEGIN
				tkInsert( x, TK_ARGBEGIN )
				x += 1

				'' arg's tokens
				tkCopy( x, argbegin[arg], argend[arg] )
				x += argend[arg] - argbegin[arg] + 1

				'' TK_ARGEND
				tkInsert( x, TK_ARGEND )
			end if
		end if

		x += 1
	wend

	''
	'' Do '##' merging
	''
	'' It's not clear how <a ## ## b> or <a ## b ## c> should be processed
	'' (undefined behaviour), so fbfrog shows an error about the first
	'' (cannot merge a and ##) and processes the 2nd as (a##b)##c, i.e.
	'' left-associative.
	''
	x = expansionbegin + 1
	while( tkGet( x ) <> TK_END )

		'' '##' from original macro body (and not '##' from a macro argument)?
		if( tkGet( x ) = TK_PPMERGE ) then

			'' 1. If lhs/rhs of '##' were params, then now there will be TK_ARGBEGIN,...,TK_ARGEND sequences.
			'' Move last/first token out of the arg boundaries, so that they end up right next to the '##'.
			'' (can just move the TK_ARGEND/TK_ARGBEGIN respectively, that's easier & faster)
			''
			'' Example with arg on both sides:
			'' from:
			''    [argbegin] a b [argend] ## [argbegin] c d [argend]
			'' to:
			''    [argbegin] a [argend] b ## c [argbegin] d [argend]
			''
			'' If this causes an TK_ARGBEGIN/END to become empty, it must be removed,
			'' so that it won't be misinterpreted as empty arg operand for a following ## operator:
			'' from:
			''    [argbegin] a [argend] ## [argbegin] b [argend] ## [argbegin] c [argend]
			'' to:
			''    a##b ## [argbegin] c [argend]
			'' in order to avoid the situation where the 2nd ##'s lhs seems to be an empty arg:
			''    [argbegin] [argend] a ## b [argbegin] [argend] ## [argbegin] c [argend]
			'' because actually the merged "ab" token is supposed to be 2nd ##'s lhs.

			'' lhs was a non-empty arg?
			if( (tkGet( x - 1 ) = TK_ARGEND) and (tkGet( x - 2 ) <> TK_ARGBEGIN)  ) then
				tkRemove( x - 1, x - 1 )
				tkInsert( x - 2, TK_ARGEND )
				assert( tkGet( x ) = TK_PPMERGE )
				assert( tkGet( x - 1 ) <> TK_ARGEND )
				assert( tkGet( x - 2 ) = TK_ARGEND )

				'' Empty now? Then remove the TK_ARGBEGIN/END
				if( tkGet( x - 3 ) = TK_ARGBEGIN ) then
					tkRemove( x - 3, x - 2 )
					x -= 2
				end if
			end if

			'' rhs was a non-empty arg?
			if( (tkGet( x + 1 ) = TK_ARGBEGIN) and (tkGet( x + 2 ) <> TK_ARGEND) ) then
				tkRemove( x + 1, x + 1 )
				tkInsert( x + 2, TK_ARGBEGIN )
				assert( tkGet( x ) = TK_PPMERGE )
				assert( tkGet( x + 1 ) <> TK_ARGBEGIN )
				assert( tkGet( x + 2 ) = TK_ARGBEGIN )

				'' Empty now? Then remove the TK_ARGBEGIN/END
				if( tkGet( x + 3 ) = TK_ARGEND ) then
					tkRemove( x + 2, x + 3 )
				end if
			end if

			assert( tkGet( x ) = TK_PPMERGE )
			var l = x - 1
			var r = x + 1

			'' If one operand was an empty arg, then no merging needs to be done,
			'' the other operand can just be preserved as-is; or in case both were
			'' empty, the ## just disappears.

			'' Non-empty on both sides?
			if( (tkGet( l ) <> TK_ARGEND) and (tkGet( r ) <> TK_ARGBEGIN) ) then
				if( tkGet( l ) = TK_BEGIN ) then
					tkOops( x, "## merge operator at beginning of macro body, missing operand to merge with" )
				end if
				if( tkGet( r ) = TK_END ) then
					tkOops( x, "## merge operator at end of macro body, missing operand to merge with" )
				end if

				'' Combine the original text representation of both tokens
				var mergetext = tkSpell( l ) + tkSpell( r )

				'' and try to lex them
				var y = tkGetCount( )
				lexLoadC( y, sourcebufferFromZstring( "## merge operation", mergetext, tkGetLocation( x ) ), FALSE )

				'' That should have produced only 1 token. If it produced more, then the merge failed.
				assert( tkGetCount( ) >= (y + 1) )
				if( tkGetCount( ) > (y + 1) ) then
					tkRemove( y, tkGetCount( ) - 1 )
					tkOops( x, "## merge operator cannot merge '" + tkSpell( x - 1 ) + "' and '" + tkSpell( x + 1 ) + "'" )
				end if

				'' Remove the 3 (l ## r) tokens and insert the merged token in place of l
				tkRemove( l, r )
				y -= 3
				x = l

				tkCopy( x, y, y )
				y += 1

				tkRemove( y, y )
			else
				'' Just remove the '##'
				tkRemove( x, x )
				x -= 1
			end if
		end if

		x += 1
	wend

	'' Recursively macro-expand the tokens in each TK_ARGBEGIN/END sequence,
	'' and then remove TK_ARGBEGIN/END.
	x = expansionbegin + 1
	while( tkGet( x ) <> TK_END )

		'' Macro parameter?
		if( tkGet( x ) = TK_ARGBEGIN ) then
			var y = x
			do
				y += 1
			loop while( tkGet( y ) <> TK_ARGEND )

			'' Macro-expand the arg's tokens
			y = hExpandInRange( x, y, inside_ifexpr )

			'' Remove TK_ARGBEGIN/END wrapping
			assert( tkGet( x ) = TK_ARGBEGIN )
			tkRemove( x, x )
			x -= 1
			y -= 1
			assert( tkGet( y ) = TK_ARGEND )
			tkRemove( y, y )
			y -= 1

			x = y
		end if

		x += 1
	wend

	'' Remove the TK_BEGIN/END wrapping around the expansion
	assert( tkGet( expansionbegin ) = TK_BEGIN )
	tkRemove( expansionbegin, expansionbegin )
	x -= 1
	assert( tkGet( x ) = TK_END )
	tkRemove( x, x )
	x -= 1

	function = x
end function

private sub hExpandMacro _
	( _
		byval xmacro as integer, _
		byval macro as ASTNODE ptr, _
		byval callbegin as integer, _
		byval callend as integer, _
		byval argbegin as integer ptr, _
		byval argend as integer ptr, _
		byval argcount as integer, _
		byval inside_ifexpr as integer _
	)

	'' Insert the macro body behind the call (this way the positions
	'' stored in argbegin()/argend() stay valid)
	var expansionbegin = callend + 1
	var expansionend = hInsertMacroExpansion( expansionbegin, xmacro, macro, argbegin, argend, argcount, inside_ifexpr )

	'' Set expansion level on the expansion tokens:
	'' = minlevel from macro call tokens + 1
	'' before doing nested macro expansion in the expansion tokens.
	tkSetExpansionLevel( expansionbegin, expansionend, _
		tkGetExpansionLevel( _
			tkFindTokenWithMinExpansionLevel( callbegin, callend ) _
		) + 1 )

	'' Recursively do macro expansion in the expansion
	'' - Marking the current macro as poisoned, so it won't be expanded
	''   again within the expansion, preventing expansion of complete
	''   recursive calls.
	'' - Incomplete recursive calls need to be marked with NOEXPAND so they
	''   won't be expanded later when they become complete by taking into
	''   account tokens following behind the expansion.
	macro->attrib or= ASTATTRIB_POISONED
	expansionend = hExpandInRange( expansionbegin, expansionend, inside_ifexpr )
	macro->attrib and= not ASTATTRIB_POISONED

	'' Disable future expansion of recursive macro calls to this macro
	'' (those that weren't expanded due to the "poisoning")
	scope
		var x = expansionbegin
		while( x <= expansionend )

			if( tkGet( x ) >= TK_ID ) then
				'' Known macro, and it's the same as this one?
				var xcalledmacro = hCheckForMacroCall( x )
				if( xcalledmacro = xmacro ) then
					'' Can the macro call be parsed successfully,
					'' and is it fully within the expansion?
					dim as integer argbegin(0 to MAXARGS-1)
					dim as integer argend(0 to MAXARGS-1)
					dim as integer argcount
					var callend = hParseMacroCall( x, tkGetAst( xcalledmacro ), @argbegin(0), @argend(0), argcount )
					if( (callend >= 0) and (callend <= expansionend) ) then
						tkAddFlags( x, TKFLAG_NOEXPAND )
					end if
				end if
			end if

			x += 1
		wend
	end scope

	'' Then remove the call tokens
	tkRemove( callbegin, callend )
end sub

private sub hMaybeExpandMacro( byref x as integer, byval inside_ifexpr as integer )
	var begin = x

	var xmacro = hCheckForMacroCall( x )
	if( xmacro < 0 ) then
		exit sub
	end if

	assert( tkGet( xmacro ) = TK_PPDEFINE )
	var macro = tkGetAst( xmacro )

	dim as integer argbegin(0 to MAXARGS-1)
	dim as integer argend(0 to MAXARGS-1)
	dim as integer argcount

	'' Try to parse the macro call (can fail in case of function-like macro
	'' without argument list)
	var callbegin = x
	var callend = hParseMacroCall( callbegin, macro, @argbegin(0), @argend(0), argcount )
	if( callend < 0 ) then
		exit sub
	end if

	hExpandMacro( xmacro, macro, callbegin, callend, @argbegin(0), @argend(0), argcount, inside_ifexpr )

	'' The macro call was replaced with the body, the token at the TK_ID's
	'' position must be re-parsed.
	x -= 1
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

''
'' Evaluation of CPP #if condition expressions
''
'' - directly evaluating instead of doing complex node folding, because that's
''   all that's needed - #if expressions always need to be evaluated anyways.
''
'' - only evaluating &&, || and ?: operands when they're actually reached. This
''    - prevents "assuming undefined" warnings about the unreached code
''    - allows division by zero to be ignored if it doesn't need to be evaluated
''
'' - taking care to produce C's 1|0 boolean values, instead of FB's -1|0
''
private function cppEval( byval n as ASTNODE ptr ) as longint
	if( n = NULL ) then exit function

	select case( n->class )
	case ASTCLASS_CONSTI  : function = n->vali

	case ASTCLASS_CLOGNOT   : function =   -(cppEval( n->head ) = 0)
	case ASTCLASS_NOT       : function = not cppEval( n->head )
	case ASTCLASS_NEGATE    : function =   - cppEval( n->head )
	case ASTCLASS_UNARYPLUS : function =     cppEval( n->head )
	case ASTCLASS_CLOGOR    : function =   -(cppEval( n->head ) orelse  cppEval( n->tail ))
	case ASTCLASS_CLOGAND   : function =   -(cppEval( n->head ) andalso cppEval( n->tail ))

	case ASTCLASS_OR, ASTCLASS_XOR, ASTCLASS_AND, _
	     ASTCLASS_CEQ, ASTCLASS_CNE, _
	     ASTCLASS_CLT, ASTCLASS_CLE, _
	     ASTCLASS_CGT, ASTCLASS_CGE, _
	     ASTCLASS_SHL, ASTCLASS_SHR, _
	     ASTCLASS_ADD, ASTCLASS_SUB, _
	     ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD
		var l = cppEval( n->head )
		var r = cppEval( n->tail )

		select case( n->class )
		case ASTCLASS_DIV, ASTCLASS_MOD
			if( r = 0 ) then
				astOops( n, "division by zero" )
			end if
		end select

		select case( n->class )
		case ASTCLASS_OR  : function =   l or  r
		case ASTCLASS_XOR : function =   l xor r
		case ASTCLASS_AND : function =   l and r
		case ASTCLASS_CEQ : function = -(l =   r)
		case ASTCLASS_CNE : function = -(l <>  r)
		case ASTCLASS_CLT : function = -(l <   r)
		case ASTCLASS_CLE : function = -(l <=  r)
		case ASTCLASS_CGT : function = -(l >   r)
		case ASTCLASS_CGE : function = -(l >=  r)
		case ASTCLASS_SHL : function =   l shl r
		case ASTCLASS_SHR : function =   l shr r
		case ASTCLASS_ADD : function =   l +   r
		case ASTCLASS_SUB : function =   l -   r
		case ASTCLASS_MUL : function =   l *   r
		case ASTCLASS_DIV : function =   l \   r
		case ASTCLASS_MOD : function =   l mod r
		case else         : assert( FALSE )
		end select

	case ASTCLASS_IIF
		function = iif( cppEval( n->expr ), cppEval( n->head ), cppEval( n->tail ) )

	case ASTCLASS_ID
		'' Unexpanded identifier, assume it's a literal 0, like a CPP
		if( frog.verbose ) then
			astReport( n, "treating unexpanded identifier '" + *n->text + "' as literal zero" )
		end if

		'' And register as known undefined (it wasn't expanded so it
		'' must be undefined), so the warning won't be shown again.
		hRegisterMacro( n->text, -1 )

		'' id  ->  0
		function = 0

	case ASTCLASS_CDEFINED
		assert( n->head->class = ASTCLASS_ID )
		var id = n->head->text

		if( hIsKnownSymbol( id ) = FALSE ) then
			'' Unknown symbol, assume it's undefined
			if( frog.verbose ) then
				astReport( n->head, "assuming symbol '" + *n->head->text + "' is undefined" )
			end if

			'' Register as known undefined
			'' This also prevents the above warning from being shown
			'' multiple times for a single symbol.
			hRegisterMacro( n->head->text, -1 )
		end if

		'' defined()  ->  1|0
		function = -hIsMacroCurrentlyDefined( id )

	case else
		astOops( n, "couldn't evaluate #if condition" )
	end select
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
		assert( tkGet( x ) = TK_BEGIN )
		hExpandUntilTK_END( x + 1, TRUE )

		t = hParseIfCondition( x )

		x = xif
	end if

	'' Evaluate the condition
	function = (cppEval( t ) <> 0)
end function

private function hLookupRemoveSym( byval id as zstring ptr ) as integer
	function = (hashLookup( @eval.removes, id, hashHash( id ) )->s <> NULL)
end function

private sub hSkipIfAndSetRemove( byref x as integer )
	var last = x
	if( tkGet( x + 1 ) = TK_BEGIN ) then
		last = hSkipToTK_END( x + 1 )
	end if
	tkSetRemove( x, last )
	x = last
end sub

private sub hSetRemoveOnMacro( byval x as integer )
	assert( tkGet( x ) = TK_PPDEFINE )
	tkSetRemove( x, hSkipToTK_END( x + 1 ) )
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

private sub hPreprocessTokens( byval x as integer, byval whitespace as integer )
	if( whitespace ) then
		cppComments( x )
		cppDividers( x )
	end if
	cppIdentifyDirectives( x )
end sub

private sub hLoadFile _
	( _
		byval x as integer, _
		byval includeloc as TKLOCATION ptr, _
		byval filename as zstring ptr, _
		byval whitespace as integer _
	)

	lexLoadC( x, sourcebufferFromFile( filename, includeloc ), whitespace )
	hPreprocessTokens( x, whitespace )

end sub

'' Search for #included files in one of the parent directories of the context
'' file. Usually the #include will refer to a file in the same directory or in
'' a sub-directory at the same level or some levels up.
private function hSearchHeaderFile _
	( _
		byref contextfile as string, _
		byref inctext as string _
	) as string

	if( frog.verbose ) then
		frogPrint( "searching: " + inctext + " (context = " + contextfile + ")" )
	end if

	'' 1. If #including by absolute path, use it as-is
	if( pathIsAbsolute( inctext ) ) then
		return inctext
	end if

	'' 2. Relative to context file
	var incfile = pathAddDiv( pathOnly( contextfile ) ) + inctext
	if( frog.verbose ) then
		frogPrint( "trying: " + incfile )
	end if
	if( hFileExists( incfile ) ) then
		return incfile
	end if

	var i = frog.incdirs->head
	while( i )

		incfile = pathAddDiv( *i->text ) + inctext
		if( frog.verbose ) then
			frogPrint( "trying: " + incfile )
		end if
		if( hFileExists( incfile ) ) then
			return incfile
		end if

		i = i->next
	wend

	function = ""
end function

private function hFindBeginInclude( byval x as integer ) as integer
	var level = 0

	do
		x -= 1

		select case( tkGet( x ) )
		case TK_BEGININCLUDE
			if( level = 0 ) then
				exit do
			end if
			level -= 1
		case TK_ENDINCLUDE
			level += 1
		end select
	loop

	function = x
end function

sub cppMain( byval whitespace as integer, byval nomerge as integer )
	'' Identify pre-#define directives, if any
	hPreprocessTokens( 0, whitespace )

	var x = 0
	var skiplevel = MAXPPSTACK
	var level = 0
	ppstack(0) = 0
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

			var begin = hFindBeginInclude( x )

			if( nomerge ) then
				'' #include expansion wasn't requested, so mark all the #included tokens for removal.
				'' (i.e. all #defines/#undefs were seen, but the code won't be preserved)
				tkSetRemove( begin, x )
			else
				'' Remove just the TK_BEGININCLUDE/TK_ENDINCLUDE, keep the #included tokens.
				tkSetRemove( begin )
				tkSetRemove( x )
			end if

		case TK_PPINCLUDE
			if( skiplevel <> MAXPPSTACK ) then
				tkSetRemove( x )
			else
				var location = tkGetLocation( x )
				dim as string contextfile
				if( location->source ) then
					contextfile = *location->source->name
				end if
				var inctext = *tkGetText( x )
				var incfile = hSearchHeaderFile( contextfile, inctext )

				if( len( incfile ) > 0 ) then
					frogPrint( incfile )

					level += 1
					hCheckStackLevel( x, level )
					ppstack(level) = 0

					'' Remove the #include token
					assert( tkGet( x ) = TK_PPINCLUDE )
					tkSetRemove( x )
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
					hLoadFile( x, location, incfile, whitespace )

					'' Start parsing the #included content
					'' (starting behind the EOL inserted above)
					x -= 1
					assert( tkGet( x ) = TK_EOL )
				else
					frogPrint( inctext + " (not found)" )
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

			hSkipIfAndSetRemove( x )

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

			hSkipIfAndSetRemove( x )

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

			tkSetRemove( x )

		case TK_PPENDIF
			if( ppstack(level) < PPSTACK_IF ) then
				tkOops( x, "#endif without #if" )
			end if

			'' If skipping due to current level, then stop skipping.
			if( skiplevel = level ) then
				skiplevel = MAXPPSTACK
			end if

			tkSetRemove( x )

			level -= 1

		case TK_PPDEFINE
			if( skiplevel <> MAXPPSTACK ) then
				hSetRemoveOnMacro( x )
			else
				var macro = tkGetAst( x )

				'' Check for previous #define
				var xprevmacro = hLookupMacro( macro->text )
				if( xprevmacro >= 0 ) then
					if( frog.verbose ) then
						hMaybeReportConflictingDefine( macro, tkGetAst( xprevmacro ) )
					end if

					'' Don't preserve previous #define
					hSetRemoveOnMacro( xprevmacro )
				end if

				'' Register/overwrite as known defined symbol
				hRegisterMacro( macro->text, x )

				'' Don't preserve the #define if the symbol was registed for removal
				if( hLookupRemoveSym( macro->text ) ) then
					hSetRemoveOnMacro( x )
				end if
			end if

			'' Skip over TK_BEGIN/TK_END macro body
			x = hSkipToTK_END( x + 1 )

		case TK_PPUNDEF
			if( skiplevel <> MAXPPSTACK ) then
				tkSetRemove( x )
			else
				var id = tkGetText( x )

				'' If #undeffing an existing #define, don't preserve it
				var xmacro = hLookupMacro( id )
				if( xmacro >= 0 ) then
					assert( tkGet( xmacro ) = TK_PPDEFINE )
					assert( *tkGetAst( xmacro )->text = *id )
					hSetRemoveOnMacro( xmacro )
				end if

				'' Register/overwrite as known undefined symbol
				hRegisterMacro( id, -1 )

				'' Don't preserve #undefs
				tkSetRemove( x )
			end if

		case TK_PPERROR
			if( skiplevel <> MAXPPSTACK ) then
				tkSetRemove( x )
			else
				'' Not using the #error's text as error message,
				'' otherwise it would be mistaken for being generated by fbfrog.
				tkOops( x, "#error" )
			end if

		case TK_PPWARNING
			if( skiplevel <> MAXPPSTACK ) then
				tkSetRemove( x )
			else
				'' ditto
				print tkReport( x, "#warning" )
			end if

		case is >= TK_ID
			if( skiplevel <> MAXPPSTACK ) then
				tkSetRemove( x )
			else
				hMaybeExpandMacro( x, FALSE )
			end if

		case else
			'' Remove tokens if skipping
			if( skiplevel <> MAXPPSTACK ) then
				tkSetRemove( x )
			end if
		end select

		x += 1
	loop

	'' 2nd pass that actually removes directives/tokens marked for removal
	'' (doing this in separate steps allows error reports during the 1st
	'' pass to still view the complete input)
	tkApplyRemoves( )

	cppEnd( )
end sub

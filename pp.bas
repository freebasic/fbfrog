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
'' ppDirectives1() merges PP directives into TK_PP* tokens, except that #define
'' bodies and #if expressions are not yet parsed, but only enclosed in TK_BEGIN
'' and TK_END tokens.
''
'' ppDirectives2() goes through all #defines and finishes the parsing job by
'' parsing the #define bodies into ASTs and removing the TK_BEGIN/END parts.
''
'' ppDirectives3() does the same for #if conditions.
''
'' With this separation it's possible to identify PP directives, possibly do
'' macro expansion in #define bodies, and finally do macro expansion in #if
'' conditions while knowing all #defines completely.
''
''
'' #if evaluation
'' --------------
''
'' ppAddSym() can be used to register symbols as initially "defined" or
'' "undefined" for #if defined() or #ifdef checks. defined() checks on unknown
'' symbols are not solved out.
''
'' 1. ppSplitElseIfs():
''    - splits #elseifs blocks up into normal #else/#if/#endif blocks
'' 2. ppEvalIfs():
''    - goes through all #ifs and tries to simplify the expressions
''    - checks each #if whether the condition is known to be true or false,
''      i.e. whether the expression could be simplified down to a constant,
''      and if so, deletes the tokens from the #if or #else blocks accordingly
''      before continuing to the next #if.
''    - not having to worry about #elseifs greatly simplifies this step
''    - #defines and #undefs are taken into account for defined() checks too
'' 3. ppMergeElseIfs():
''    - merges #else/#if/#endif blocks back into #elseifs
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

		case TK_PPINCLUDE, TK_PPDEFINE, TK_PPIF, TK_PPELSEIF, _
		     TK_PPELSE, TK_PPENDIF, TK_PPUNDEF, TK_PPUNKNOWN, TK_DIVIDER
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
		byval level as integer = 0, _
		byval allow_id_atoms as integer _
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
		a = astNew( astclass, ppExpression( x, ppopinfo(astclass).level, allow_id_atoms ) )
	else
		'' Atoms
		select case( tkGet( x ) )
		'' '(' Expression ')'
		case TK_LPAREN
			'' '('
			x = ppSkip( x )

			'' Expression
			a = ppExpression( x, , allow_id_atoms )
			if( a = NULL ) then
				exit function
			end if

			'' ')'
			if( tkGet( x ) <> TK_RPAREN ) then
				astDelete( a )
				exit function
			end if
			x = ppSkip( x )

		'' Number literals
		case TK_OCTNUM, TK_DECNUM, TK_HEXNUM, TK_DECFLOAT
			a = hNumberLiteral( x )
			x = ppSkip( x )

		'' Identifier
		case TK_ID
			if( allow_id_atoms = FALSE ) then
				exit function
			end if

			'' Accepting identifiers as atoms to allow more PP
			'' expressions to be parsed, such as
			''    defined FOO && FOO == 123
			'' without having to expand FOO.
			a = astNew( ASTCLASS_ID, tkGetText( x ) )
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
		var b = ppExpression( x, oplevel, allow_id_atoms )
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

			c = ppExpression( x, oplevel, allow_id_atoms )
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
	case TK_EOL, TK_EOF, TK_END, _
	     TK_PPINCLUDE, TK_PPDEFINE, TK_PPIF, TK_PPELSEIF, _
	     TK_PPELSE, TK_PPENDIF, TK_PPUNDEF, TK_PPUNKNOWN, TK_DIVIDER

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
		t->paramcount = -1

		'' '('? (+1 instead of ppSkip() so TK_SPACE won't be ignored)
		if( tkGet( x + 1 ) = TK_LPAREN ) then
			'' #define's Identifier
			x += 1

			'' '('
			x = ppSkip( x )
			t->paramcount = 0

			'' List of macro parameters:
			'' Identifier (',' Identifier)*
			do
				'' macro parameter's Identifier
				if( tkGet( x ) <> TK_ID ) then
					exit do
				end if
				astAddChild( t, astNew( ASTCLASS_MACROPARAM, tkGetText( x ) ) )
				t->paramcount += 1
				x = ppSkip( x )

				'' ','?
				if( tkGet( x ) <> TK_COMMA ) then
					exit do
				end if
				x = ppSkip( x )
			loop

			'' ')'?
			if( tkGet( x ) <> TK_RPAREN ) then
				return -1
			end if
			x = ppSkip( x )
		else
			'' #define's Identifier
			x = ppSkip( x )
		end if

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

	case KW_UNDEF
		x = ppSkip( x )

		'' Identifier?
		if( tkGet( x ) <> TK_ID ) then
			return -1
		end if
		t = astNew( ASTCLASS_ID, tkGetText( x ) )
		x = ppSkip( x )

		t = astNew( ASTCLASS_PPUNDEF, t )

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

	dim tkid as integer
	select case( t->class )
	case ASTCLASS_PPINCLUDE : tkid = TK_PPINCLUDE
	case ASTCLASS_PPDEFINE  : tkid = TK_PPDEFINE
	case ASTCLASS_PPIF      : tkid = TK_PPIF
	case ASTCLASS_PPELSEIF  : tkid = TK_PPELSEIF
	case ASTCLASS_PPELSE    : tkid = TK_PPELSE
	case ASTCLASS_PPENDIF   : tkid = TK_PPENDIF
	case ASTCLASS_PPUNDEF   : tkid = TK_PPUNDEF
	case else               : assert( FALSE )
	end select

	if( keepbegin >= 0 ) then
		tkRemove( begin, keepbegin - 1 )
		x -= keepbegin - begin
		keepbegin -= keepbegin - begin

		tkInsert( begin, tkid, , t )
		begin += 1
		keepbegin += 1
		x += 1

		tkInsert( keepbegin, TK_BEGIN )
		x += 1
		tkInsert( x, TK_END )
		x += 1
	else
		tkRemove( begin, x - 1 )
		tkInsert( begin, tkid, , t )
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
	case TK_PPINCLUDE, TK_PPDEFINE, TK_PPIF, TK_PPELSEIF, _
	     TK_PPELSE, TK_PPENDIF, TK_PPUNDEF, TK_PPUNKNOWN, TK_DIVIDER
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
	case TK_EOL, TK_EOF, TK_END, _
	     TK_PPINCLUDE, TK_PPDEFINE, TK_PPIF, TK_PPELSEIF, _
	     TK_PPELSE, TK_PPENDIF, TK_PPUNDEF, TK_PPUNKNOWN, TK_DIVIDER

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
	tkInsert( begin, TK_PPUNKNOWN, , expr )
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
''
'' Example of recording the body of a #define:
''
''      #define m(a, b) a foo #b a##b a##foo
''                     |------- body ------|
''
'' This sequence of macro body nodes will be created:
''
''      tk TK_SPACE
''      param 0
''      tk TK_SPACE
''      tk TK_ID "foo"
''      param 1 (stringify)
''      tk TK_SPACE
''      param 0 (merge right)
''      param 1 (merge left)
''      tk TK_SPACE
''      param 0 (merge right)
''      tk TK_ID "foo" (merge left)
''
namespace record
	dim shared as ASTNODE ptr macro
	dim shared as integer x, stringify, merge
end namespace

private sub hTakeMergeAttrib _
	( _
		byval macrobody as ASTNODE ptr, _
		byval n as ASTNODE ptr _
	)

	if( record.merge ) then
		'' Remove TK_SPACE's preceding the '##'
		while( macrobody->tail )
			if( macrobody->tail->class <> ASTCLASS_TK ) then exit while
			if( macrobody->tail->tk <> TK_SPACE ) then exit while
			astRemoveChild( macrobody, macrobody->tail )
		wend

		n->attrib or= ASTATTRIB_MERGEWITHPREV
		record.merge = FALSE
	end if
end sub

'' Used for uninteresting tokens
private sub hRecordToken( )
	'' Don't add TK_SPACE's behind a '##'
	if( record.merge and (tkGet( record.x ) = TK_SPACE) ) then
		exit sub
	end if

	var n = astNew( ASTCLASS_TK, tkGetText( record.x ) )
	n->tk = tkGet( record.x )
	hTakeMergeAttrib( record.macro->initializer, n )
	astAddChild( record.macro->initializer, n )
end sub

'' Used when a 'param' or '#param' was found in the macro body.
private sub hRecordParam _
	( _
		byval id as zstring ptr, _
		byval paramindex as integer, _
		byval stringify as integer _
	)

	var n = astNew( ASTCLASS_MACROPARAM, id )
	n->paramindex = paramindex

	if( stringify ) then
		n->attrib or= ASTATTRIB_STRINGIFY
	end if
	hTakeMergeAttrib( record.macro->initializer, n )

	astAddChild( record.macro->initializer, n )
end sub

private sub hRecordMerge( )
	'' The next token/param (if any) will recieve the '##' merge attribute
	record.merge = TRUE
end sub

private function hLookupMacroParam( byval id as zstring ptr ) as integer
	var index = 0
	var param = record.macro->head
	while( param )
		assert( param->class = ASTCLASS_MACROPARAM )
		if( *param->text = *id ) then
			return index
		end if
		index += 1
		param = param->next
	wend
	function = -1
end function

private sub hRecordBody( )
	do
		select case( tkGet( record.x ) )
		case TK_END
			exit do

		case TK_ID
			'' Is it one of the #define's parameters?
			var id = tkGetText( record.x )
			var paramindex = hLookupMacroParam( id )
			if( paramindex >= 0 ) then
				hRecordParam( id, paramindex, FALSE )
			else
				hRecordToken( )
			end if

		'' '#'?
		case TK_HASH
			'' '#param'?
			if( tkGet( record.x + 1 ) = TK_ID ) then
				'' Is it one of the #define's parameters?
				var id = tkGetText( record.x + 1 )
				var paramindex = hLookupMacroParam( id )
				if( paramindex >= 0 ) then
					'' '#'
					record.x += 1

					hRecordParam( id, paramindex, TRUE )
				else
					'' '#'
					hRecordToken( )
					record.x += 1

					hRecordToken( )
				end if
			else
				'' '#'
				hRecordToken( )
			end if

		'' '##'?
		case TK_HASHHASH
			hRecordMerge( )

		case else
			hRecordToken( )
		end select

		record.x += 1
	loop
end sub

sub ppDirectives2( )
	var x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_PPDEFINE
			var t = tkGetAst( x )
			x += 1

			'' BEGIN
			assert( tkGet( x ) = TK_BEGIN )
			var begin = x
			x += 1

			'' Body tokens
			assert( t->initializer = NULL )

			'' Try to parse the body as expression
			var expr = ppExpression( x, , FALSE )

			'' Expression found and TK_END reached?
			if( (expr <> NULL) and (tkGet( x ) = TK_END) ) then
				t->initializer = expr
			else
				'' Then either no expression could be parsed at all,
				'' or it was an expression followed by more tokens.
				x = begin + 1
				astDelete( expr )
				expr = NULL

				'' Parse macro body properly
				t->initializer = astNew( ASTCLASS_MACROBODY )
				record.macro = t
				record.x = x
				record.stringify = FALSE
				record.merge = FALSE
				hRecordBody( )
				x = record.x
			end if

			'' END
			assert( tkGet( x ) = TK_END )
			tkRemove( begin, x )
			x = begin

		case else
			x += 1
		end select
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private sub hExprToTokens( byval n as ASTNODE ptr, byref x as integer )
	if( n = NULL ) then
		exit sub
	end if

	select case as const( n->class )
	case ASTCLASS_CONST
		dim s as string
		dim tk as integer
		if( typeIsFloat( n->dtype ) ) then
			tk = TK_DECFLOAT
			s = str( n->val.f )
		elseif( n->attrib and ASTATTRIB_OCT ) then
			tk = TK_OCTNUM
			s = "0" + oct( n->val.i )
		elseif( n->attrib and ASTATTRIB_HEX ) then
			tk = TK_HEXNUM
			s = "0x" + hex( n->val.i )
		else
			tk = TK_DECNUM
			s = str( n->val.i )
		end if
		tkInsert( x, tk, s ) : x += 1

	case ASTCLASS_ID
		tkInsert( x, TK_ID, n->text ) : x += 1

	case ASTCLASS_DEFINED
		'' defined id
		tkInsert( x, KW_DEFINED ) : x += 1
		tkInsert( x, TK_SPACE )   : x += 1
		hExprToTokens( n->head, x )

	case ASTCLASS_IIF
		'' condition ? l : r
		hExprToTokens( n->head, x )
		tkInsert( x, TK_SPACE )   : x += 1
		tkInsert( x, TK_QUEST )   : x += 1  '' ?
		tkInsert( x, TK_SPACE )   : x += 1
		hExprToTokens( n->head->next, x )
		tkInsert( x, TK_SPACE )   : x += 1
		tkInsert( x, TK_COLON )   : x += 1  '' :
		tkInsert( x, TK_SPACE )   : x += 1
		hExprToTokens( n->tail, x )

	case ASTCLASS_LOGOR, ASTCLASS_LOGAND, _
	     ASTCLASS_BITOR, ASTCLASS_BITXOR, ASTCLASS_BITAND, _
	     ASTCLASS_EQ, ASTCLASS_NE, _
	     ASTCLASS_LT, ASTCLASS_LE, _
	     ASTCLASS_GT, ASTCLASS_GE, _
	     ASTCLASS_SHL, ASTCLASS_SHR, _
	     ASTCLASS_ADD, ASTCLASS_SUB, _
	     ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD

		hExprToTokens( n->head, x )
		tkInsert( x, TK_SPACE ) : x += 1

		dim as integer tk
		select case as const( n->class )
		case ASTCLASS_LOGOR  : tk = TK_PIPEPIPE  '' ||
		case ASTCLASS_LOGAND : tk = TK_AMPAMP    '' &&
		case ASTCLASS_BITOR  : tk = TK_PIPE      '' |
		case ASTCLASS_BITXOR : tk = TK_CIRC      '' ^
		case ASTCLASS_BITAND : tk = TK_AMP       '' &
		case ASTCLASS_EQ     : tk = TK_EQEQ      '' ==
		case ASTCLASS_NE     : tk = TK_EXCLEQ    '' !=
		case ASTCLASS_LT     : tk = TK_LT        '' <
		case ASTCLASS_LE     : tk = TK_LTEQ      '' <=
		case ASTCLASS_GT     : tk = TK_GT        '' >
		case ASTCLASS_GE     : tk = TK_GTEQ      '' >=
		case ASTCLASS_SHL    : tk = TK_LTLT      '' <<
		case ASTCLASS_SHR    : tk = TK_GTGT      '' >>
		case ASTCLASS_ADD    : tk = TK_PLUS      '' +
		case ASTCLASS_SUB    : tk = TK_MINUS     '' -
		case ASTCLASS_MUL    : tk = TK_STAR      '' *
		case ASTCLASS_DIV    : tk = TK_SLASH     '' /
		case ASTCLASS_MOD    : tk = TK_PERCENT   '' %
		case else
			assert( FALSE )
		end select
		tkInsert( x, tk ) : x += 1

		tkInsert( x, TK_SPACE ) : x += 1
		hExprToTokens( n->tail, x )

	case ASTCLASS_LOGNOT, ASTCLASS_BITNOT, _
	     ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS

		dim as integer tk
		select case as const( n->class )
		case ASTCLASS_LOGNOT    : tk = TK_EXCL  '' !
		case ASTCLASS_BITNOT    : tk = TK_TILDE '' ~
		case ASTCLASS_NEGATE    : tk = TK_MINUS '' -
		case ASTCLASS_UNARYPLUS : tk = TK_PLUS  '' +
		case else
			assert( FALSE )
		end select
		tkInsert( x, tk ) : x += 1

		tkInsert( x, TK_SPACE ) : x += 1
		hExprToTokens( n->head, x )

	case else
		assert( FALSE )
	end select
end sub

private function hMacroCall _
	( _
		byval macro as ASTNODE ptr, _
		byval x as integer _
	) as integer

	const MAXARGS = 32
	dim as integer argbegin(0 to MAXARGS-1), argend(0 to MAXARGS-1)
	var args = 0
	var begin = x

	'' ID
	assert( tkGet( x ) = TK_ID )
	x += 1

	'' Not just "#define m"?
	if( macro->paramcount >= 0 ) then
		'' '('?
		if( tkGet( x ) <> TK_LPAREN ) then
			return FALSE
		end if
		x += 1

		'' Not just "#define m()"?
		if( macro->paramcount > 0 ) then
			'' For each arg...
			do
				if( args = MAXARGS ) then
					return FALSE
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
						return FALSE
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

			'' As many args as params?
			if( macro->paramcount <> args ) then
				return FALSE
			end if
		end if

		'' ')'?
		if( tkGet( x ) <> TK_RPAREN ) then
			return FALSE
		end if
		x += 1
	end if

	var bodybegin = x

	'' Not an empty #define?
	var macrobody = macro->initializer
	if( macrobody ) then
		'' Insert the macro body behind the call

		'' #define body parsed as tokens?
		if( macrobody->class = ASTCLASS_MACROBODY ) then
			var child = macrobody->head
			while( child )
				var y = x

				select case( child->class )
				case ASTCLASS_TK
					tkInsert( x, child->tk, child->text )
					x += 1

				case ASTCLASS_MACROPARAM
					var arg = child->paramindex
					assert( (arg >= 0) and (arg < args) )

					if( child->attrib and ASTATTRIB_STRINGIFY ) then
						'' Turn the arg's tokens into a string and insert it as string literal
						tkInsert( x, TK_STRING, tkManyToCText( argbegin(arg), argend(arg) ) )
						x += 1
					else
						'' Copy the arg's tokens into the body
						tkCopy( x, argbegin(arg), argend(arg) )
						x += argend(arg) - argbegin(arg) + 1
					end if

				case else
					assert( FALSE )
				end select

				if( child->attrib and ASTATTRIB_MERGEWITHPREV ) then
					'' Not the first token? (it wouldn't have a previous token to merge with)
					if( bodybegin < y ) then
						'' Assuming every token >= TK_ID is mergable (i.e. an identifier or keyword)
						'' Assuming TK_SPACE's around '##' have already been removed
						if( (tkGet( y - 1 ) >= TK_ID) and _
						    (tkGet( y     ) >= TK_ID) ) then
							tkInsert( x, TK_ID, *tkGetIdOrKw( y - 1 ) + *tkGetIdOrKw( y ) )
							x += 1
							tkRemove( y - 1, y )
							x -= 2
						else
							print tkDumpOne( y - 1 )
							print tkDumpOne( y )
							oops( "cannot merge these two tokens when expanding macro '" & *macro->text & "'" )
						end if
					end if
				end if

				child = child->next
			wend
		'' #define body is an expression
		else
			hExprToTokens( macrobody, x )
		end if
	end if

	'' Then remove the call tokens
	tkRemove( begin, bodybegin - 1 )
	x -= (bodybegin - 1) - begin + 1

	function = TRUE
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

namespace eval
	'' Initial list of symbols that are known to be defined or undefined
	'' (individual passes have to maintain their custom list, initially
	'' filled with the symbols from here, because the list changes when
	'' #defines or #undefs are found)
	dim shared as ASTNODE ptr initialknownsyms

	'' List of precious symbols: symbols registered for expansion
	dim shared as ASTNODE ptr expandsyms
	dim shared expandsymhash as THASH

	'' Initially declared macros
	dim shared as ASTNODE ptr initialmacros
end namespace

sub ppEvalInit( )
	eval.initialknownsyms = astNew( ASTCLASS_GROUP )
	eval.expandsyms = astNew( ASTCLASS_GROUP )
	hashInit( @eval.expandsymhash, 4 )
	eval.initialmacros = astNew( ASTCLASS_GROUP )
end sub

sub ppEvalEnd( )
	astDelete( eval.initialmacros )
	hashEnd( @eval.expandsymhash )
	astDelete( eval.expandsyms )
	astDelete( eval.initialknownsyms )
end sub

#if __FB_DEBUG__
private function hSymNotYetAdded _
	( _
		byval group as ASTNODE ptr, _
		byval id as zstring ptr _
	) as integer

	var found = FALSE

	var child = group->head
	while( child )
		if( *child->text = *id ) then
			found = TRUE
			exit while
		end if
		child = child->next
	wend

	function = not found
end function
#endif

sub ppAddSym( byval id as zstring ptr, byval is_defined as integer )
	assert( hSymNotYetAdded( eval.initialknownsyms, id ) )
	astAddChild( eval.initialknownsyms, astNewID( id, is_defined ) )
end sub

sub ppExpandSym( byval id as zstring ptr )
	assert( hSymNotYetAdded( eval.expandsyms, id ) )
	var n = astNewID( id )
	astAddChild( eval.expandsyms, n )
	hashAddOverwrite( @eval.expandsymhash, n->text, NULL )
end sub

sub ppMacroBegin( byval id as zstring ptr, byval paramcount as integer )
	assert( paramcount >= -1 )
	ppAddSym( id, TRUE )
	ppExpandSym( id )
	var macro = astNew( ASTCLASS_PPDEFINE, id )
	macro->paramcount = paramcount
	macro->initializer = astNew( ASTCLASS_MACROBODY )
	astAddChild( eval.initialmacros, macro )
end sub

sub ppMacroToken( byval tk as integer, byval text as zstring ptr )
	var macro = eval.initialmacros->tail
	astAddChild( macro->initializer, astNewTK( tk, text ) )
end sub

sub ppMacroParam( byval index as integer )
	var macro = eval.initialmacros->tail
	assert( macro->paramcount > 0 )
	assert( (index >= 0) and (index < macro->paramcount) )
	astAddChild( macro->initializer, astNewMACROPARAM( index ) )
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

		case TK_PPIF
			level += 1

		case TK_PPELSE
			if( level = 0 ) then
				xelse = x
			end if

		case TK_PPENDIF
			if( level = 0 ) then
				xendif = x
				exit do
			end if
			level -= 1

		end select

		x = ppSkip( x )
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
	var x = -1
	do
		x = ppSkip( x )

		select case( tkGet( x ) )
		case TK_EOF
			exit do

		'' Found an #elseif? Replace it by #else #if
		case TK_PPELSEIF
			tkRemove( x, x )
			tkInsert( x, TK_PPELSE, , astNew( ASTCLASS_PPELSE ) )
			x += 1
			tkInsert( x, TK_PPIF, , astNew( ASTCLASS_PPIF ) )
			'' Note: there may be TK_BEGIN/END following, but that's ok

			'' Find the corresponding #endif,
			'' and insert another #endif in front of it
			dim as integer xelse, xendif
			hFindElseEndIf( ppSkip( x ), xelse, xendif )
			tkInsert( xendif, TK_PPENDIF, , astNew( ASTCLASS_PPENDIF ) )

		end select
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Check whether the given id is a precious symbol
private function hLookupExpandSym( byval id as zstring ptr ) as integer
	function = (hashLookup( @eval.expandsymhash, id, hashHash( id ) )->s <> NULL)
end function

'' Check whether the given range of tokens includes #defines/#undefs for a
'' precious symbol
private function hContainsPreciousDefine _
	( _
		byval first as integer, _
		byval last as integer _
	) as integer

	for i as integer = first to last
		select case( tkGet( i ) )
		case TK_PPDEFINE
			if( hLookupExpandSym( tkGetAst( i )->text ) ) then
				return TRUE
			end if
		case TK_PPUNDEF
			var t = tkGetAst( i )
			assert( t->head->class = ASTCLASS_ID )
			if( hLookupExpandSym( t->head->text ) ) then
				return TRUE
			end if
		end select
	next

	function = FALSE
end function

private sub hIntegrateTrailCodeIntoIfElseBlocks( )
	const STACKSIZE = 512
	static xelse(0 to STACKSIZE-1) as integer
	static xendif(0 to STACKSIZE-1) as integer

	var x = -1
	var level = -1
	do
		x = ppSkip( x )

		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_PPIF
			'' Note: there may be TK_BEGIN/END following, but that's ok

			level += 1
			if( level >= STACKSIZE ) then
				oops( __FUNCTION__ & "(" & __LINE__ & "): stack size too small" )
			end if

			hFindElseEndIf( ppSkip( x ), xelse(level), xendif(level) )

			if( hContainsPreciousDefine( ppSkip( x ), xendif(level) - 1 ) ) then
				'' If there's no #else, add it
				if( xelse(level) = xendif(level) ) then
					tkInsert( xelse(level), TK_PPELSE, , astNew( ASTCLASS_PPELSE ) )
					xendif(level) += 1
					for i as integer = 0 to level-1
						xelse(i) += 1
						xendif(i) += 1
					next
				end if

				'' Duplicate all tokens following the #endif
				'' into each of the #if and #else code paths,
				'' then remove them from behind the #endif.

				var first = xendif(level) + 1

				'' If it's the top-most #if, copy all tokens
				'' from #endif until EOF.
				var last = tkGetCount( )-1

				'' Otherwise, if it's a nested #if, copy only
				'' the tokens from #endif to the next #else or
				'' #endif, depending on whether this #if block
				'' is inside an #if or #else block.
				if( level > 0 ) then
					'' In front of parent's #else?
					'' (or #endif if there is no #else)
					if( x < xelse(level-1) ) then
						last = xelse(level-1)-1
					else
						last = xendif(level-1)-1
					end if
				end if

				var delta = last - first + 1

				if( delta > 0 ) then
					tkCopy( xelse(level), first, last )
					xelse(level) += delta
					xendif(level) += delta
					first += delta
					last += delta
					for i as integer = 0 to level-1
						xelse(i) += delta
						xendif(i) += delta
					next

					tkCopy( xendif(level), first, last )
					xendif(level) += delta
					first += delta
					last += delta
					for i as integer = 0 to level-1
						xelse(i) += delta
						xendif(i) += delta
					next

					tkRemove( first, last )
					for i as integer = 0 to level-1
						xelse(i) -= delta
						xendif(i) -= delta
					next
				end if
			end if

		case TK_PPENDIF
			assert( level >= 0 )
			assert( x = xendif(level) )
			level -= 1
		end select
	loop
end sub

type MACROSTATUS
	macro		as ASTNODE ptr
	level		as integer
	undeflevel	as integer
end type

namespace expand
	dim shared statusstack as TLIST
end namespace

#if __FB_DEBUG__
private function hFindMaxMacroLevel( ) as integer
	var level = -1
	dim as MACROSTATUS ptr n = listGetHead( @expand.statusstack )
	while( n )
		if( level < n->level ) then
			level = n->level
		end if
		n = listGetNext( n )
	wend
	function = level
end function
#endif

private sub hAddMacro( byval macro as ASTNODE ptr, byval level as integer )
	assert( macro->class = ASTCLASS_PPDEFINE )
	assert( level >= hFindMaxMacroLevel( ) )
	dim as MACROSTATUS ptr n = listAppend( @expand.statusstack )
	n->macro = macro
	n->level = level
	n->undeflevel = -1
end sub

private function hLookupMacro _
	( _
		byval id as zstring ptr, _
		byval level as integer _
	) as ASTNODE ptr

	dim as MACROSTATUS ptr n = listGetTail( @expand.statusstack )
	while( n )
		if( *n->macro->text = *id ) then
			'' Not #undeffed?
			if( n->undeflevel < 0 ) then
				return n->macro
			end if
		end if
		n = listGetPrev( n )
	wend

	function = NULL
end function

private sub hForgetMacros( byval level as integer )
	'' Drop all macros >= level from the top of the stack
	do
		dim as MACROSTATUS ptr n = listGetTail( @expand.statusstack )
		if( n = NULL ) then exit do
		if( n->level < level ) then exit do
		listDelete( @expand.statusstack, n )
	loop
	assert( hFindMaxMacroLevel( ) < level )
end sub

'' Reset #undef status for macros #undeffed in this #if level
private sub hResetUndefStatus( byval level as integer )
	dim as MACROSTATUS ptr n = listGetTail( @expand.statusstack )
	while( n )
		if( n->undeflevel = level ) then
			n->undeflevel = -1
		end if
		assert( n->undeflevel < level )
		n = listGetPrev( n )
	wend
end sub

private sub hUndefMacro( byval id as zstring ptr, byval level as integer )
	dim as MACROSTATUS ptr n = listGetTail( @expand.statusstack )
	do
		if( *n->macro->text = *id ) then
			n->undeflevel = level
			exit do
		end if
		n = listGetPrev( n )
	loop while( n )
end sub

sub ppExpand( )
	hIntegrateTrailCodeIntoIfElseBlocks( )

	listInit( @expand.statusstack, sizeof( MACROSTATUS ) )
	scope
		'' Add initial macros
		var child = eval.initialmacros->head
		while( child )
			hAddMacro( astClone( child ), 0 )
			child = child->next
		wend
	end scope

	var level = 0
	var x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_PPIF
			level += 1

		case TK_PPELSE, TK_PPENDIF
			'' Forget about #defines/#undefs from the #if block
			hForgetMacros( level )
			hResetUndefStatus( level )

			if( tkGet( x ) = TK_PPENDIF ) then
				level -= 1
			end if

		case TK_PPDEFINE
			if( hLookupExpandSym( tkGetAst( x )->text ) ) then
				'' Push macro to the stack, using current #if
				'' nesting level
				hAddMacro( astClone( tkGetAst( x ) ), level )
			end if

		case TK_PPUNDEF
			var t = tkGetAst( x )
			assert( t->head->class = ASTCLASS_ID )
			var id = t->head->text
			if( hLookupExpandSym( id ) ) then
				hUndefMacro( id, level )
			end if

		case TK_ID
			var id = tkGetText( x )
			if( hLookupExpandSym( id ) ) then
				var macro = hLookupMacro( id, level )
				if( macro ) then
					if( hMacroCall( macro, x ) ) then
						'' The macro call will be replaced with the body,
						'' the token at TK_ID's position must be re-parsed.
						x -= 1
					end if
				end if
			end if

		end select

		x += 1
	loop

	scope
		dim n as MACROSTATUS ptr = listGetHead( @expand.statusstack )
		while( n )
			astDelete( n->macro )
			n = listGetNext( n )
		wend
	end scope
	listEnd( @expand.statusstack )
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub ppDirectives3( )
	var x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_PPIF, TK_PPELSEIF
			var t = tkGetAst( x )
			x += 1

			'' No #if expression yet?
			if( t->head = NULL ) then
				'' BEGIN
				assert( tkGet( x ) = TK_BEGIN )
				var begin = x
				x += 1

				'' Expression tokens
				var expr = ppExpression( x, , TRUE )
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

		case else
			x += 1
		end select
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

enum
	COND_UNKNOWN = 0
	COND_TRUE
	COND_FALSE
end enum

namespace evalif
	dim shared knownsyms as ASTNODE ptr
	dim shared knownsymhash as THASH
end namespace

private sub hAddKnownSym( byval id as zstring ptr, byval is_defined as integer )
	var n = astNewID( id, is_defined )
	astAddChild( evalif.knownsyms, n )
	hashAddOverwrite( @evalif.knownsymhash, n->text, cptr( any ptr, is_defined ) )
end sub

private function hLookupKnownSym( byval id as zstring ptr ) as integer
	var item = hashLookup( @evalif.knownsymhash, id, hashHash( id ) )
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
		var cond = hLookupKnownSym( n->head->text )
		if( cond <> COND_UNKNOWN ) then
			'' defined()    ->    1|0
			function = astNewCONST( iif( cond = COND_TRUE, 1, 0 ), 0, TYPE_LONG )
			astDelete( n )
		end if

	case ASTCLASS_IIF
		if( n->head->class = ASTCLASS_CONST ) then
			if( typeIsFloat( n->head->dtype ) = FALSE ) then
				if( n->head->val.i ) then
					function = astClone( n->head->next )
				else
					function = astClone( n->tail )
				end if
				astDelete( n )
			end if
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
			if( (not typeIsFloat( n->head->dtype )) and _
			    (not typeIsFloat( n->tail->dtype )) ) then
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
			end if

		'' Only the lhs is a CONST? Check for NOPs
		elseif( (n->head->class = ASTCLASS_CONST) and _
		        (n->tail->class <> ASTCLASS_CONST) ) then

			if( typeIsFloat( n->head->dtype ) = FALSE ) then
				var v1 = n->head->val.i

				select case( n->class )

				'' true  || x   = 1
				'' false || x   = x
				case ASTCLASS_LOGOR
					if( v1 ) then
						function = astNewCONST( 1, 0, TYPE_LONG )
					else
						function = astClone( n->tail )
					end if
					astDelete( n )

				'' true  && x   = x
				'' false && x   = 0
				case ASTCLASS_LOGAND
					if( v1 ) then
						function = astClone( n->tail )
					else
						function = astNewCONST( 0, 0, TYPE_LONG )
					end if
					astDelete( n )

				'' 0 | x = x
				'' 0 + x = x
				case ASTCLASS_BITOR, ASTCLASS_ADD
					if( v1 = 0 ) then
						function = astClone( n->tail )
						astDelete( n )
					end if

				'' 0 &  x = 0
				'' 0 << x = 0
				'' 0 >> x = 0
				'' 0 /  x = 0
				'' 0 %  x = 0
				case ASTCLASS_BITAND, ASTCLASS_SHL, ASTCLASS_SHR, _
				     ASTCLASS_DIV, ASTCLASS_MOD
					if( v1 = 0 ) then
						function = astNewCONST( 0, 0, TYPE_LONG )
						astDelete( n )
					end if

				'' 0 * x = 0
				'' 1 * x = x
				case ASTCLASS_MUL
					select case( v1 )
					case 0
						function = astNewCONST( 0, 0, TYPE_LONG )
						astDelete( n )
					case 1
						function = astClone( n->tail )
						astDelete( n )
					end select

				'' 0 - x = -x
				case ASTCLASS_SUB
					if( v1 = 0 ) then
						function = astNew( ASTCLASS_NEGATE, astClone( n->tail ) )
						astDelete( n )
					end if

				end select
			end if

		'' Only the rhs is a CONST? Check for NOPs
		elseif( (n->head->class <> ASTCLASS_CONST) and _
		        (n->tail->class = ASTCLASS_CONST) ) then

			if( typeIsFloat( n->tail->dtype ) = FALSE ) then
				var v2 = n->tail->val.i

				select case( n->class )

				'' x || true    = 1
				'' x || false   = x
				case ASTCLASS_LOGOR
					if( v2 ) then
						function = astNewCONST( 1, 0, TYPE_LONG )
					else
						function = astClone( n->head )
					end if
					astDelete( n )

				'' x && true    = x
				'' x && false   = 0
				case ASTCLASS_LOGAND
					if( v2 ) then
						function = astClone( n->head )
					else
						function = astNewCONST( 0, 0, TYPE_LONG )
					end if
					astDelete( n )

				'' x | 0 = x
				'' x + 0 = x
				'' x - 0 = x
				'' x << 0 = x
				'' x >> 0 = x
				case ASTCLASS_BITOR, ASTCLASS_ADD, ASTCLASS_SUB, _
				     ASTCLASS_SHL, ASTCLASS_SHR
					if( v2 = 0 ) then
						function = astClone( n->head )
						astDelete( n )
					end if

				'' x & 0 = 0
				case ASTCLASS_BITAND
					if( v2 = 0 ) then
						function = astNewCONST( 0, 0, TYPE_LONG )
						astDelete( n )
					end if

				'' x * 0 = 0
				'' x * 1 = x
				case ASTCLASS_MUL
					select case( v2 )
					case 0
						function = astNewCONST( 0, 0, TYPE_LONG )
						astDelete( n )
					case 1
						function = astClone( n->head )
						astDelete( n )
					end select

				end select
			end if
		end if

	case ASTCLASS_LOGNOT, ASTCLASS_BITNOT, _
	     ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS

		if( n->head->class = ASTCLASS_CONST ) then
			if( typeIsFloat( n->head->dtype ) = FALSE ) then
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
		end if

	end select
end function

'' Solve out #if/#else/#endif blocks if the condition is known to be TRUE/FALSE
sub ppEvalIfs( )
	evalif.knownsyms = astNew( ASTCLASS_GROUP )
	hashInit( @evalif.knownsymhash, 4 )
	scope
		var child = eval.initialknownsyms->head
		while( child )
			assert( child->class = ASTCLASS_ID )
			hAddKnownSym( child->text, child->is_defined )
			child = child->next
		wend
	end scope

	var x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_PPDEFINE
			'' Register/overwrite #define as known defined symbol
			var t = tkGetAst( x )
			hAddKnownSym( t->text, TRUE )

		case TK_PPUNDEF
			'' Register/overwrite #define as known undefined symbol
			var t = tkGetAst( x )
			assert( t->head->class = ASTCLASS_ID )
			hAddKnownSym( t->head->text, FALSE )

		case TK_PPIF
			'' Assuming TK_BEGIN/END have been solved out by now
			assert( tkGet( x + 1 ) <> TK_BEGIN )

			'' 1. Try to evaluate the condition
			tkSetAst( x, hFold( astClone( tkGetAst( x ) ) ) )

			'' 2. Check the condition
			var t = tkGetAst( x )
			var cond = COND_UNKNOWN
			if( t->head->class = ASTCLASS_CONST ) then
				if( typeIsFloat( t->head->dtype ) = FALSE ) then
					cond = iif( t->head->val.i <> 0, COND_TRUE, COND_FALSE )
				end if
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

		end select

		x += 1
	loop

	hashEnd( @evalif.knownsymhash )
	astDelete( evalif.knownsyms )
end sub

'' Merge #else #if back into #elseifs
sub ppMergeElseIfs( )
	var x = 0

	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		'' Found an #else followed by an #if?
		case TK_PPELSE
			if( tkGet( x + 1 ) = TK_PPIF ) then
				'' Find the #endif corresponding to the #if
				dim as integer xelse, xendif
				hFindElseEndIf( x + 1, xelse, xendif )

				'' Followed immediately by the #endif
				'' corresponding to the #else?
				if( tkGet( xendif + 1 ) = TK_PPENDIF ) then
					'' #else #if expr -> #elseif expr
					var t = astClone( tkGetAst( x + 1 )->head )
					tkRemove( x, x + 1 )
					tkInsert( x, TK_PPELSEIF, , astNew( ASTCLASS_PPELSEIF, t ) )

					'' Remove the #endif
					tkRemove( xendif, xendif )
				end if
			end if

		end select

		x += 1
	loop
end sub

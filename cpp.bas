''
'' C pre-processor
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
'' cppMain() goes through the token buffer much like a C preprocessor would do,
'' parsing CPP directives keeping track of #defines and #undefs, doing macro
'' expansion, evaluating #if blocks, and expanding #includes.
''
'' All tokens that shouldn't be preserved for the C parser later are marked via
'' tkSetRemove() (for tkApplyRemoves() later). This affects most directives and
'' all tokens skipped in #if 0 blocks. As a special case, #defines and
'' unexpanded #includes are not deleted, but preserved for the C parser, because
'' we want to parse them there too.
''
'' Since directives are not deleted from the token buffer immediately, we can
'' leave #define bodies in place, and copy the tokens from there whenever they
'' are needed for a macro expansion. Otherwise it would be necessary to load the
'' tokens into some AST.
''
'' In various places (especially during macro expansion), we're temporarily
'' inserting helper tokens such as TK_ARGBEGIN/TK_ARGEND to enclose a range of
'' tokens. Then, instead of having to keep track of token indices which change
'' on every insertion/deletion, we just have to watch out for those specific
'' helper tokens to detect begin/end of the range.
''
'' Tokens inserted due to #include expansion are enclosed in TK_BEGININCLUDE and
'' TK_ENDINCLUDE. This allows detecting #include EOF for the #if/#include stack,
'' and allows the C parser to identify include boundaries later. If the include
'' file should be filtered out (according to -filterout/-filterin options), the
'' TK_BEGININCLUDE will be marked with TKFLAG_FILTEROUT. This in turn allows
'' cPreParse() to mark all tokens between TK_BEGININCUDE/TK_ENDINCLUDE with
'' TKFLAG_FILTEROUT, which lets the C parser know which constructs should be
'' marked with ASTATTRIB_FILTEROUT. The include tokens can only be marked this
'' way once the CPP is finished, because macro expansion may result in new
'' tokens being inserted which wouldn't automatically have the TKFLAG_FILTEROUT
'' flag too.
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
#include once "crt.bi"

declare sub hMaybeExpandMacro( byref x as integer, byval inside_ifexpr as integer )

private function hIsBeforeEol( byval x as integer, byval delta as integer ) as integer
	'' Can we reach EOL before hitting any non-space token?
	x = tkSkipComment( x, delta )
	select case( tkGet( x ) )
	case TK_EOL, TK_EOF
		function = TRUE
	end select
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
				x = tkSkipCommentEol( hSkipToEol( x ) )
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
			tkRemove( begin, x - 1 )
			tkInsert( begin, TK_DIVIDER, blockcomment )
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

function hSkipToEol( byval x as integer ) as integer
	while( (tkGet( x ) <> TK_EOL) and (tkGet( x ) <> TK_EOF) )
		x += 1
	wend
	function = x
end function

''
'' Check whether the number literal token (TK_NUMBER) is a valid number literal,
'' and build a CONSTI/CONSTF ASTNODE representing its value (the literal saved
'' as-is in textual form, without type suffixes) and data type.
''
'' As a special case, in CPP expressions, number literals are always treated as
'' 64bit signed or unsigned, no matter what dtype was given as suffix and
'' ignoring the "int" default.
''
'' These are covered:
''    123        (decimal)
''    .123       (decimal float)
''    123.123    (decimal float)
''    0xAABBCCDD (hexadecimal)
''    010        (octal)
''    010.0      (decimal float, not octal float)
'' There also is some simple float exponent and type suffix parsing.
''
'' We have to parse the number literal (but without type suffix) first before we
'' can decide whether it's an integer or float. This decides whether a leading
'' zero indicates octal or not.
''
function hNumberLiteral( byval x as integer, byval is_cpp as integer, byref errmsg as string ) as ASTNODE ptr
	assert( tkGet( x ) = TK_NUMBER )
	dim as ubyte ptr p = tkGetText( x )

	var numbase = 10
	var is_float = FALSE
	var have_nonoct_digit = FALSE

	'' 0 or 0x prefix?
	if( p[0] = CH_0 ) then '' 0
		if( (p[1] = CH_L_X) or (p[1] = CH_X) ) then '' 0x or 0X
			p += 2
			numbase = 16
		elseif( (p[1] >= CH_0) and (p[1] <= CH_9) ) then
			p += 1
			numbase = 8
		end if
	end if

	'' Body (integer part + fractional part, if any)
	var begin = p
	do
		select case as const( p[0] )
		case CH_0 to CH_7
			'' These digits are allowed in all number literals

		case CH_8, CH_9
			'' These digits are allowed in dec/hex literals, but not
			'' oct literals, but we don't know which it is yet.
			have_nonoct_digit = TRUE

		case CH_A to CH_F, CH_L_A to CH_L_F
			'' These digits can only appear in hex literals
			if( numbase <> 16 ) then
				exit do
			end if

		case CH_DOT
			'' Only one dot allowed
			if( is_float ) then exit do
			is_float = TRUE

		case else
			exit do
		end select

		p += 1
	loop

	'' Exponent? (can be used even without fractional part, e.g. '1e1')
	select case( p[0] )
	case CH_E, CH_L_E   '' 'E', 'e'
		is_float = TRUE
		p += 1

		'' ['+' | '-']
		select case( p[0] )
		case CH_PLUS, CH_MINUS
			p += 1
		end select

		'' ['0'-'9']*
		while( (p[0] >= CH_0) and (p[0] <= CH_9) )
			p += 1
		wend
	end select

	if( is_float ) then
		if( numbase = 16 ) then
			errmsg = "TODO: hex-floats not yet supported"
			exit function
		end if
		'' Override octal in case there was a leading zero. There are no
		'' octal floats.
		numbase = 10
	end if

	if( have_nonoct_digit and (numbase = 8) ) then
		errmsg = "invalid digit in octal number literal"
		exit function
	end if

	'' Save the number literal body (we don't want to include type suffixes here)
	var old = p[0]
	p[0] = 0  '' temporary null terminator
	var n = astNew( ASTCLASS_CONSTI, cptr( zstring ptr, begin ) )
	p[0] = old

	'' Float type suffixes
	select case( p[0] )
	case CH_F, CH_L_F    '' 'F' | 'f'
		p += 1
		is_float = TRUE
		n->dtype = TYPE_SINGLE
	case CH_D, CH_L_D    '' 'D' | 'd'
		p += 1
		is_float = TRUE
		n->dtype = TYPE_DOUBLE
	end select

	if( is_float ) then
		n->class = ASTCLASS_CONSTF
	else
		'' Integer type suffixes:
		''  l, ll, ul, ull, lu, llu
		'' MSVC-specific ones:
		''  [u]i{8|16|32|64}
		'' All letters can also be upper-case.
		var have_u = FALSE
		var have_l = FALSE
		var have_ll = FALSE

		select case( p[0] )
		case CH_U, CH_L_U       '' u
			p += 1
			have_u = TRUE
		end select

		select case( p[0] )
		case CH_L, CH_L_L       '' l, [u]l
			p += 1
			select case( p[0] )
			case CH_L, CH_L_L       '' l, [u]ll
				p += 1
				have_ll = TRUE
			case else
				have_l = TRUE
			end select

			if( have_u = FALSE ) then
				select case( p[0] )
				case CH_U, CH_L_U       '' u, llu
					p += 1
					have_u = TRUE
				end select
			end if

		case CH_I, CH_L_I       '' i, [u]i
			select case( p[1] )
			case CH_8 : p += 2 '' i8
			case CH_1 : if( p[2] = CH_6 ) then : p += 3 : end if  '' i16
			case CH_3 : if( p[2] = CH_2 ) then : p += 3 : end if  '' i32
			case CH_6 : if( p[2] = CH_4 ) then : p += 3 : have_ll = TRUE : end if  '' i64
			end select
		end select

		'' In CPP mode, all integer literals are 64bit, the 'l' suffix is ignored
		if( is_cpp ) then
			n->dtype = iif( have_u, TYPE_ULONGINT, TYPE_LONGINT )
		'' In C mode, integer literals default to 'int', and suffixes are respected
		elseif( have_ll ) then
			n->dtype = iif( have_u, TYPE_ULONGINT, TYPE_LONGINT )
		elseif( have_l ) then
			n->dtype = iif( have_u, TYPE_CULONG, TYPE_CLONG )
		else
			n->dtype = iif( have_u, TYPE_ULONG, TYPE_LONG )
		end if

		select case( numbase )
		case 16 : n->attrib or= ASTATTRIB_HEX
		case  8 : n->attrib or= ASTATTRIB_OCT
		end select
	end if

	'' Show error if we didn't reach the end of the number literal
	if( p[0] <> 0 ) then
		errmsg = "invalid suffix on number literal: '" + *cptr( zstring ptr, p ) + "'"
		astDelete( n )
		exit function
	end if

	function = n
end function

'' MacroParameter =
''      Identifier         (named parameter)
''    | Identifier '...'   (named + variadic)
''    | '...'              (variadic, using __VA_ARGS__)
'' ('...' can only appear on the last parameter)
private sub hMacroParam( byref x as integer, byval macro as ASTNODE ptr )
	'' Identifier?
	dim id as zstring ptr
	if( tkGet( x ) >= TK_ID ) then
		id = tkSpellId( x )
		x += 1
	end if

	'' Shouldn't have seen a '...' yet
	assert( (macro->attrib and ASTATTRIB_VARIADIC) = 0 )
	var maybevariadic = 0

	'' '...'?
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
		tkOopsExpected( x, "macro parameter (identifier or '...')" )
	end if

	var param = astNew( ASTCLASS_MACROPARAM, id )
	param->attrib or= maybevariadic
	astAppend( macro, param )
	macro->attrib or= maybevariadic
	macro->paramcount += 1
end sub

'' <no whitespace> '(' MacroParameters ')'
private sub hMacroParamList( byref x as integer, byval macro as ASTNODE ptr )
	assert( macro->paramcount = -1 )

	'' '(' directly behind #define identifier, no spaces in between?
	if( (tkGet( x ) = TK_LPAREN) and _
	    ((tkGetFlags( x ) and TKFLAG_BEHINDSPACE) = 0) ) then
		x += 1

		macro->paramcount = 0

		'' Not just '()'?
		if( tkGet( x ) <> TK_RPAREN ) then
			'' MacroParam (',' MacroParam)*
			do
				hMacroParam( x, macro )

				'' ','?
				if( tkGet( x ) <> TK_COMMA ) then
					exit do
				end if
				x += 1
			loop
		end if

		'' ')'?
		tkExpect( x, TK_RPAREN, "to close the parameter list in this macro declaration" )
		x += 1
	end if
end sub

function hDefineHead( byref x as integer ) as ASTNODE ptr
	'' Identifier? (keywords should be allowed to, so anything >= TK_ID)
	select case( tkGet( x ) )
	case is < TK_ID
		tkExpect( x, TK_ID, "behind #define" )
	case KW_DEFINED
		tkOops( x, "'defined' cannot be used as macro name" )
	end select
	var macro = astTakeLoc( astNewPPDEFINE( tkSpellId( x ) ), x )
	x += 1

	hMacroParamList( x, macro )

	function = macro
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type DEFINEINFO
	xdefine	as integer  '' 1st token of the #define directive (the '#')
	xbody	as integer  '' 1st token of the #define body
	xeol	as integer  '' eol/eof token behind the #define body

	'' PPDEFINE node with information about the macro's parameters etc.
	macro	as ASTNODE ptr
end type

private function definfoNew( ) as DEFINEINFO ptr
	function = callocate( sizeof( DEFINEINFO ) )
end function

private sub definfoDelete( byval definfo as DEFINEINFO ptr )
	if( definfo ) then
		astDelete( definfo->macro )
		deallocate( definfo )
	end if
end sub

private function definfoClone( byval a as DEFINEINFO ptr ) as DEFINEINFO ptr
	var b = definfoNew( )
	b->xdefine = a->xdefine
	b->xbody   = a->xbody
	b->xeol    = a->xeol
	b->macro   = astClone( a->macro )
	function = b
end function

private sub definfoSetRemove( byval definfo as DEFINEINFO ptr )
	tkSetRemove( definfo->xdefine, definfo->xeol )
end sub

'' Copy a #define body into some other place
private sub definfoCopyBody( byval definfo as DEFINEINFO ptr, byval x as integer )
	assert( x > definfo->xeol )
	tkCopy( x, definfo->xbody, definfo->xeol - 1 )
end sub

'' Compare two #defines and determine whether they are equal
private function definfoDefinesAreEqual( byval a as DEFINEINFO ptr, byval b as DEFINEINFO ptr ) as integer
	'' Check name and parameters
	if( astIsEqual( a->macro, b->macro ) = FALSE ) then
		exit function
	end if

	'' Check body
	if( tkSpell( a->xbody, a->xeol ) <> tkSpell( b->xbody, b->xeol ) ) then
		exit function
	end if

	function = TRUE
end function

type SAVEDMACRO
	id		as zstring ptr		'' Macro's name

	'' A DEFINEINFO object if the macro was defined when being saved,
	'' or NULL if it was undefined.
	definfo		as DEFINEINFO ptr
end type

private sub savedmacroDtor( byval macro as SAVEDMACRO ptr )
	deallocate( macro->id )
	definfoDelete( macro->definfo )
end sub

enum
	'' If stack states:
	STATE_FILE = 0  '' file context (fresh toplevel/#include file, no #if yet)
	STATE_IF        '' #if context, fresh
	STATE_TRUE      '' #if context, saw #if/#elseif TRUE (and thus, further #elseif TRUE's must be skipped)
	STATE_ELSE      '' #if context, saw #else (and no further #elseif/#else can be allowed)
end enum

const MAXSTACK = 128

namespace cpp
	dim shared as integer x  '' Current token index

	type STACKNODE
		state		as integer  '' STATE_*
		knownfile	as integer  '' Index into cpp.files array, if it's an #include context, or -1
	end type

	'' #if/file context stack
	'' * starts out with only the toplevel file context
	'' * #if blocks and #include contexts are put on this stack
	'' * an #endif found in an #include won't be able to close an #if from
	''   the parent file, since the #include stack node is in the way, and
	''   must be popped first.
	dim shared stack(0 to MAXSTACK-1) as STACKNODE
	dim shared as integer level  '' Current top of stack

	'' Stack level which caused #if 0 skipping
	'' * Allows us to continue parsing #ifs/#endifs even while skipping an
	''   #if 0 block, and this way, to determine which #endif ends skipping
	dim shared as integer skiplevel

	'' Lookup table of macros known to be either defined or undefined.
	''   defined <=> data = a DEFINEINFO object
	'' undefined <=> data = NULL
	'' Even though unregistered symbols are implicitly undefined,
	'' registering them is useful to show the "assuming undefined" warning
	'' (and to only show it once).
	dim shared macros		as THASH

	'' Array of macros saved by #pragma push_macro, for use by #pragma
	'' pop_macro later.
	''  * push_macro saves the macro's current state - whether it's defined
	''    or not, and if it's defined, it's body/value.
	''  * push_macro even saves duplicate copies of the macro state: two
	''    equal push_macro's directly following each other create two stack
	''    entries, not one.
	''  * pop_macro restores the macro to the last saved state, overwriting
	''    the current value, #defining the macro again if it was #undef'ed
	''    in the meantime, or even #undef'ing it.
	''  * pop_macro does nothing if the stack for that macro is empty.
	'' Macros are appended to the array in the order they are saved. Popping
	'' a macro requires searching the array backwards for an entry for the
	'' given macro name. A hash table could be used instead, but that does
	'' not seem to be worth it.
	dim shared savedmacros		as SAVEDMACRO ptr
	dim shared savedmacrocount	as integer

	dim shared noexpands		as THASH
	dim shared removes		as THASH

	'' #include exclude/include filters: GROUP of FILTEROUT/FILTERIN nodes,
	'' in the order they should be applied.
	dim shared filters		as ASTNODE ptr

	type KNOWNFILE
		incfile as zstring ptr  '' Normalized file name
		'' Include guard symbol, if this file has a pure include guard,
		'' otherwise NULL
		guardid as zstring ptr
	end type

	'' Information about known files
	dim shared files as KNOWNFILE ptr
	dim shared filecount as integer
	dim shared filetb as THASH  '' data = index into files array
end namespace

sub cppInit( )
	cpp.x = 0

	'' Toplevel file context
	with( cpp.stack(0) )
		.state = STATE_FILE
		.knownfile = -1
	end with
	cpp.level = 0

	'' No skipping yet
	cpp.skiplevel = MAXSTACK

	hashInit( @cpp.macros, 4, TRUE )
	cpp.savedmacros = NULL
	cpp.savedmacrocount = 0
	hashInit( @cpp.noexpands, 4, TRUE )
	hashInit( @cpp.removes, 4, TRUE )
	cpp.filters = astNewGROUP( )
	cpp.files = NULL
	cpp.filecount = 0
	hashInit( @cpp.filetb, 4, FALSE )
end sub

private sub cppEnd( )
	'' If anything is left on the stack at EOF, it can only be #ifs
	'' (#includes should be popped due to TK_ENDINCLUDE's already)
	if( cpp.level > 0 ) then
		assert( cpp.stack(cpp.level).state >= STATE_IF )
		tkOops( cpp.x, "missing #endif" )
	end if

	scope
		for i as integer = 0 to cpp.macros.room - 1
			definfoDelete( cpp.macros.items[i].data )
		next
		hashEnd( @cpp.macros )
	end scope
	scope
		for i as integer = 0 to cpp.savedmacrocount - 1
			savedmacroDtor( @cpp.savedmacros[i] )
		next
		deallocate( cpp.savedmacros )
	end scope
	hashEnd( @cpp.noexpands )
	hashEnd( @cpp.removes )
	astDelete( cpp.filters )
	hashEnd( @cpp.filetb )
	scope
		for i as integer = 0 to cpp.filecount - 1
			deallocate( cpp.files[i].incfile )
			deallocate( cpp.files[i].guardid )
		next
		deallocate( cpp.files )
	end scope
end sub

#define cppSkipping( ) (cpp.skiplevel <> MAXSTACK)

sub cppNoExpandSym( byval id as zstring ptr )
	hashAddOverwrite( @cpp.noexpands, id, NULL )
end sub

sub cppRemoveSym( byval id as zstring ptr )
	hashAddOverwrite( @cpp.removes, id, NULL )
end sub

sub cppAddFilter( byval filter as ASTNODE ptr )
	astAppend( cpp.filters, astClone( filter ) )
end sub

private function cppKeepIncludeContent( byref includefilename as string ) as integer
	var keep = TRUE

	var i = cpp.filters->head
	while( i )

		'' Apply filter if it would make a difference. No need to check
		'' FILTEROUT if already excluding the file; no need to check
		'' FILTERIN if already including it.
		if( keep = (i->class = ASTCLASS_FILTEROUT) ) then
			if( strMatch( includefilename, *i->text ) ) then
				keep = not keep
			end if
		end if

		i = i->next
	wend

	function = keep
end function

private function cppLookupMacro( byval id as zstring ptr ) as DEFINEINFO ptr
	var item = hashLookup( @cpp.macros, id, hashHash( id ) )
	if( item->s ) then
		function = item->data
	else
		function = NULL
	end if
end function

private function cppIsKnownSymbol( byval id as zstring ptr ) as integer
	function = (hashLookup( @cpp.macros, id, hashHash( id ) )->s <> NULL)
end function

private function cppIsMacroCurrentlyDefined( byval id as zstring ptr ) as integer
	function = (cppLookupMacro( id ) <> NULL)
end function

'' Add/overwrite a known macro definition (or register it as known undefined)
private sub cppAddMacro( byval id as zstring ptr, byval definfo as DEFINEINFO ptr )
	var hash = hashHash( id )
	var item = hashLookup( @cpp.macros, id, hash )
	if( item->s ) then
		definfoDelete( item->data )
		item->data = definfo
	else
		hashAdd( @cpp.macros, item, hash, id, definfo )
	end if
end sub

private sub cppAddKnownUndefined( byval id as zstring ptr )
	cppAddMacro( id, NULL )
end sub

private function cppShouldExpandSym( byval id as zstring ptr ) as integer
	function = (hashLookup( @cpp.noexpands, id, hashHash( id ) )->s = NULL)
end function

private function cppShouldRemoveSym( byval id as zstring ptr ) as integer
	function = (hashLookup( @cpp.removes, id, hashHash( id ) )->s <> NULL)
end function

private sub hSetRemoveOnCurrentDefine( byval id as zstring ptr )
	var definfo = cppLookupMacro( id )
	if( definfo ) then
		definfoSetRemove( definfo )
	end if
end sub

private sub cppDefineMacro( byval id as zstring ptr, byval definfo as DEFINEINFO ptr )
	'' If overwriting an existing #define, don't preserve it
	hSetRemoveOnCurrentDefine( id )

	'' Register/overwrite as known defined symbol
	cppAddMacro( id, definfo )
end sub

private sub cppUndefMacro( byval id as zstring ptr )
	'' If #undeffing an existing #define, don't preserve it
	hSetRemoveOnCurrentDefine( id )

	'' Register/overwrite as known undefined symbol
	cppAddKnownUndefined( id )
end sub

'' Append a new entry to the array of saved macros
private sub cppAppendSavedMacro( byval id as zstring ptr, byval definfo as DEFINEINFO ptr )
	cpp.savedmacrocount += 1
	cpp.savedmacros = reallocate( cpp.savedmacros, _
			cpp.savedmacrocount * sizeof( SAVEDMACRO ) )
	with( cpp.savedmacros[cpp.savedmacrocount-1] )
		.id = strDuplicate( id )
		.definfo = definfo
	end with
end sub

private sub cppRemoveSavedMacro( byval i as integer )
	assert( (i >= 0) and (i < cpp.savedmacrocount) )

	var p = cpp.savedmacros + i
	savedmacroDtor( p )
	cpp.savedmacrocount -= 1

	'' Remove array element from the middle of the array: move all elements
	'' behind it to the front, by 1 slot, to close the gap.
	var tail = cpp.savedmacrocount - i
	if( tail > 0 ) then
		memmove( p, p + 1, tail * sizeof( SAVEDMACRO ) )
	end if
end sub

private function cppLookupSavedMacro( byval id as zstring ptr ) as integer
	for i as integer = cpp.savedmacrocount - 1 to 0 step -1
		if( *cpp.savedmacros[i].id = *id ) then
			return i
		end if
	next
	function = -1
end function

private sub cppSaveMacro( byval id as zstring ptr )
	'' Check the macro's current state.
	'' If it's defined, we need to duplicate the DEFINEINFO object;
	'' otherwise, if it's undefined, we use NULL.
	var definfo = cppLookupMacro( id )
	if( definfo ) then
		definfo = definfoClone( definfo )
	end if

	cppAppendSavedMacro( id, definfo )
end sub

private sub cppRestoreMacro( byval id as zstring ptr )
	'' Search for the last saved macro for this id
	var i = cppLookupSavedMacro( id )
	if( i < 0 ) then
		exit sub
	end if

	'' Restore the macro state
	var savedmacro = @cpp.savedmacros[i]
	if( savedmacro->definfo ) then
		'' It was defined when saved, (re)-#define the macro
		cppDefineMacro( id, savedmacro->definfo )
		savedmacro->definfo = NULL
	else
		'' It was undefined when saved, #undef the macro
		cppUndefMacro( id )
	end if

	'' Remove the entry from the saved macros stack
	cppRemoveSavedMacro( i )
end sub

private function cppAppendKnownFile( byval incfile as zstring ptr, byval guardid as zstring ptr ) as integer
	incfile = strDuplicate( incfile )
	guardid = strDuplicate( guardid )

	var i = cpp.filecount
	cpp.filecount += 1
	cpp.files = reallocate( cpp.files, cpp.filecount * sizeof( *cpp.files ) )
	with( cpp.files[i] )
		.incfile = incfile
		.guardid = guardid
	end with

	hashAddOverwrite( @cpp.filetb, incfile, cptr( any ptr, i ) )
	function = i
end function

private function cppLookupKnownFile( byval incfile as zstring ptr ) as integer
	var item = hashLookup( @cpp.filetb, incfile, hashHash( incfile ) )
	if( item->s ) then
		function = cint( item->data )
	else
		function = -1
	end if
end function

private sub cppEol( )
	if( tkGet( cpp.x ) <> TK_EOL ) then
		tkOopsExpected( cpp.x, "end-of-line behind CPP directive" )
	end if
	cpp.x += 1
end sub

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
private function cppExpression( byval level as integer = 0 ) as ASTNODE ptr
	'' Unary prefix operators
	var op = -1
	select case( tkGet( cpp.x ) )
	case TK_EXCL  : op = ASTCLASS_CLOGNOT   '' !
	case TK_TILDE : op = ASTCLASS_NOT       '' ~
	case TK_MINUS : op = ASTCLASS_NEGATE    '' -
	case TK_PLUS  : op = ASTCLASS_UNARYPLUS '' +
	end select

	dim as ASTNODE ptr a
	if( op >= 0 ) then
		cpp.x += 1
		a = astNew( op, cppExpression( cprecedence(op) ) )
	else
		'' Atoms
		select case( tkGet( cpp.x ) )
		'' '(' Expression ')'
		case TK_LPAREN
			'' '('
			cpp.x += 1

			'' Expression
			a = cppExpression( )

			'' ')'
			tkExpect( cpp.x, TK_RPAREN, "for '(...)' parenthesized expression" )
			cpp.x += 1

		'' Number literals
		case TK_NUMBER
			dim errmsg as string
			a = hNumberLiteral( cpp.x, TRUE, errmsg )
			if( a = NULL ) then
				tkOops( cpp.x, errmsg )
			end if
			a = astTakeLoc( a, cpp.x )
			cpp.x += 1

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
			a = astTakeLoc( astNewID( tkSpellId( cpp.x ) ), cpp.x )
			cpp.x += 1

		'' DEFINED ['('] Identifier [')']
		case KW_DEFINED
			cpp.x += 1

			'' '('
			var have_parens = FALSE
			if( tkGet( cpp.x ) = TK_LPAREN ) then
				have_parens = TRUE
				cpp.x += 1
			end if

			'' Identifier
			if( tkGet( cpp.x ) < TK_ID ) then
				tkExpect( cpp.x, TK_ID, "as operand of DEFINED" )
			end if
			a = astTakeLoc( astNewID( tkSpellId( cpp.x ) ), cpp.x )
			cpp.x += 1

			if( have_parens ) then
				'' ')'
				tkExpect( cpp.x, TK_RPAREN, "for DEFINED(...)" )
				cpp.x += 1
			end if

			a = astNew( ASTCLASS_CDEFINED, a )

		case else
			tkOopsExpected( cpp.x, "expression" )
		end select
	end if

	'' Infix operators
	do
		select case as const( tkGet( cpp.x ) )
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
		var bopx = cpp.x
		cpp.x += 1

		'' rhs
		var b = cppExpression( oplevel )

		'' Handle ?: special case
		if( op = ASTCLASS_IIF ) then
			'' ':'?
			tkExpect( cpp.x, TK_COLON, "for a?b:c iif operator" )
			cpp.x += 1

			var c = cppExpression( oplevel )

			a = astNewIIF( a, b, c )
		else
			a = astNew( op, a, b )
		end if
		astTakeLoc( a, bopx )
	loop

	function = a
end function

private function hCheckForMacroCall( byval x as integer ) as DEFINEINFO ptr
	assert( tkGet( x ) >= TK_ID )
	var id = tkSpellId( x )

	'' Is this id a macro?
	var definfo = cppLookupMacro( id )
	if( definfo = NULL ) then
		return NULL
	end if

	'' Only expand if not marked otherwise
	if( (not cppShouldExpandSym( id )) or _
	    (tkGetFlags( x ) and TKFLAG_NOEXPAND) or _
	    (definfo->macro->attrib and ASTATTRIB_POISONED) ) then
		return NULL
	end if

	function = definfo
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
		if( tkGet( x ) <> TK_COMMA ) then
			exit do
		end if
		x += 1
	loop

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

private sub hWrapInTkBeginEnd( byval first as integer, byval last as integer )
	assert( first <= last )
	tkInsert( first, TK_BEGIN )
	last += 1
	tkInsert( last + 1, TK_END )
end sub

private sub hUnwrapTkBeginEnd( byval first as integer, byval last as integer )
	assert( tkGet( first ) = TK_BEGIN )
	assert( tkGet( last ) = TK_END )
	tkRemove( first, first )
	last -= 1
	tkRemove( last, last )
end sub

private function hExpandInTkBeginEnd _
	( _
		byval x as integer, _
		byval inside_ifexpr as integer _
	) as integer

	assert( tkGet( x ) = TK_BEGIN )

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

	'' Do nothing if range is empty - happens when expanding in a macro
	'' expansion but the expansion is empty, or when expanding in an #if
	'' condition but it's missing.
	if( first > last ) then
		return last
	end if

	'' Insert TK_BEGIN/TK_END around the argument's tokens, to prevent the
	'' macro call parsing functions from reading out-of-bounds.
	hWrapInTkBeginEnd( first, last )
	last += 2
	assert( tkGet( last ) = TK_END )

	'' Expand anything in the range
	last = hExpandInTkBeginEnd( first, inside_ifexpr )

	'' Remove TK_BEGIN/TK_END again
	hUnwrapTkBeginEnd( first, last )
	last -= 2

	function = last
end function

'' Set or unset the BEHINDSPACE flag of a token
private sub hOverrideBehindspace( byval x as integer, byval flag as integer )
	tkSetFlags( x, (tkGetFlags( x ) and (not TKFLAG_BEHINDSPACE)) or flag )
end sub

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
		byval callbehindspace as integer, _
		byval expansionbegin as integer, _
		byval definfo as DEFINEINFO ptr, _
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
	tkInsert( expansionbegin, TK_END )
	definfoCopyBody( definfo, expansionbegin )
	tkInsert( expansionbegin, TK_BEGIN )

	'' Update the BEHINDSPACE status of the first token in the expansion to
	'' be the same as that of the macro name which we're expanding
	hOverrideBehindspace( expansionbegin + 1, callbehindspace )

	'' Solve #stringify operators (higher priority than ##, and no macro
	'' expansion done for the arg)
	var x = expansionbegin + 1
	while( tkGet( x ) <> TK_END )

		'' '#param'?
		if( tkGet( x ) = TK_HASH ) then
			'' Followed by identifier?
			if( tkGet( x + 1 ) >= TK_ID ) then
				'' Is it a macro parameter?
				var arg = astLookupMacroParam( definfo->macro, tkSpellId( x + 1 ) )
				if( arg >= 0 ) then
					'' Remove #param, and insert stringify result instead
					'' but preserve BEHINDSPACE status.
					assert( (arg >= 0) and (arg < argcount) )
					var behindspace = tkGetFlags( x ) and TKFLAG_BEHINDSPACE
					tkRemove( x, x + 1 )
					tkInsert( x, TK_STRING, tkSpell( argbegin[arg], argend[arg] ) )
					hOverrideBehindspace( x, behindspace )
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
			tkInsert( x, TK_PPMERGE )
			var y = x + 1
			tkSetLocation( x, tkGetLocation( y ) )
			tkRemove( y, y )
		end if

		x += 1
	wend

	'' Insert args into params, surrounded with TK_ARGBEGIN/END, so that
	'' - we know when an arg was empty when doing ## merging (to avoid
	''   merging with other tokens outside the arg),
	'' - we know the arg's boundaries for macro-expanding it later. (must be
	''   done after merging, because only the unmerged tokens of an arg
	''   shall be macro-expanded, and not the ones involved in merging)
	x = expansionbegin + 1
	while( tkGet( x ) <> TK_END )

		'' Macro parameter?
		if( tkGet( x ) >= TK_ID ) then
			var arg = astLookupMacroParam( definfo->macro, tkSpellId( x ) )
			if( arg >= 0 ) then
				'' >= TK_ID
				var behindspace = tkGetFlags( x ) and TKFLAG_BEHINDSPACE
				tkRemove( x, x )

				'' TK_ARGBEGIN
				tkInsert( x, TK_ARGBEGIN )
				x += 1

				'' arg's tokens
				tkCopy( x, argbegin[arg], argend[arg] )
				hOverrideBehindspace( x, behindspace )
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

				'' Combine the original text representation of both tokens,
				'' and prepend a space if the lhs was BEHINDSPACE, such that
				'' the merged token will also be BEHINDSPACE.
				dim mergetext as string
				if( tkGetFlags( l ) and TKFLAG_BEHINDSPACE ) then
					mergetext += " "
				end if
				mergetext += tkSpell( l ) + tkSpell( r )

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
		byval definfo as DEFINEINFO ptr, _
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
	var expansionend = hInsertMacroExpansion( _
			tkGetFlags( callbegin ) and TKFLAG_BEHINDSPACE, _
			expansionbegin, definfo, argbegin, argend, argcount, inside_ifexpr )

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
	definfo->macro->attrib or= ASTATTRIB_POISONED
	expansionend = hExpandInRange( expansionbegin, expansionend, inside_ifexpr )
	definfo->macro->attrib and= not ASTATTRIB_POISONED

	'' Disable future expansion of recursive macro calls to this macro
	'' (those that weren't expanded due to the "poisoning")
	scope
		var x = expansionbegin
		while( x <= expansionend )

			if( tkGet( x ) >= TK_ID ) then
				'' Known macro, and it's the same as this one?
				var calldefinfo = hCheckForMacroCall( x )
				if( calldefinfo = definfo ) then
					'' Can the macro call be parsed successfully,
					'' and is it fully within the expansion?
					dim as integer argbegin(0 to MAXARGS-1)
					dim as integer argend(0 to MAXARGS-1)
					dim as integer argcount
					var callend = hParseMacroCall( x, definfo->macro, @argbegin(0), @argend(0), argcount )
					if( (callend >= 0) and (callend <= expansionend) ) then
						tkAddFlags( x, x, TKFLAG_NOEXPAND )
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

	var definfo = hCheckForMacroCall( x )
	if( definfo = NULL ) then
		exit sub
	end if

	dim as integer argbegin(0 to MAXARGS-1)
	dim as integer argend(0 to MAXARGS-1)
	dim as integer argcount

	'' Try to parse the macro call (can fail in case of function-like macro
	'' without argument list)
	var callbegin = x
	var callend = hParseMacroCall( callbegin, definfo->macro, @argbegin(0), @argend(0), argcount )
	if( callend < 0 ) then
		exit sub
	end if

	hExpandMacro( definfo, callbegin, callend, @argbegin(0), @argend(0), argcount, inside_ifexpr )

	'' The macro call was replaced with the body, the token at the TK_ID's
	'' position must be re-parsed.
	x -= 1
end sub

''
'' Pre-calculate the result data types of UOPs/BOPs in a CPP expression
''
'' For the ?: operator, the result dtype depends on the l/r operands but at the
'' same time only one of them must be evaluated. Thus it makes sense to handle
'' this in 2 separate steps.
''
'' This isn't a problem with &&/||, the relational BOPs and the unary ! because
'' they always produce a signed int.
''
private sub cppEvalResultDtypes( byval n as ASTNODE ptr )
	select case( n->class )
	case ASTCLASS_CONSTI
		assert( (n->dtype = TYPE_LONGINT) or (n->dtype = TYPE_ULONGINT) )

	case ASTCLASS_CLOGNOT
		cppEvalResultDtypes( n->head )

		'' ! operator always produces a signed int
		n->dtype = TYPE_LONGINT

	case ASTCLASS_NOT, ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS
		cppEvalResultDtypes( n->head )
		n->dtype = n->head->dtype

	case ASTCLASS_OR, ASTCLASS_XOR, ASTCLASS_AND, _
	     ASTCLASS_SHL, ASTCLASS_SHR, _
	     ASTCLASS_ADD, ASTCLASS_SUB, _
	     ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD, _
	     ASTCLASS_IIF
		if( n->class = ASTCLASS_IIF ) then
			cppEvalResultDtypes( n->expr )
		end if
		cppEvalResultDtypes( n->head )
		cppEvalResultDtypes( n->tail )

		'' If one operand is unsigned, the result is too.
		if( (n->head->dtype = TYPE_ULONGINT) or _
		    (n->tail->dtype = TYPE_ULONGINT) ) then
			n->dtype = TYPE_ULONGINT
		else
			n->dtype = TYPE_LONGINT
		end if

	case ASTCLASS_CEQ, ASTCLASS_CNE, _
	     ASTCLASS_CLT, ASTCLASS_CLE, _
	     ASTCLASS_CGT, ASTCLASS_CGE, _
	     ASTCLASS_CLOGOR, ASTCLASS_CLOGAND
		cppEvalResultDtypes( n->head )
		cppEvalResultDtypes( n->tail )

		'' Relational BOPs and &&/|| always produce a signed int
		n->dtype = TYPE_LONGINT

	case ASTCLASS_ID, ASTCLASS_CDEFINED
		'' Unexpanded identifier -> literal 0
		'' defined()  ->  1|0
		n->dtype = TYPE_LONGINT

	case else
		astOops( n, "couldn't evaluate #if condition" )
	end select
end sub

type CPPVALUE
	vali as longint
	dtype as integer
end type

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
private sub cppEval( byref v as CPPVALUE, byval n as ASTNODE ptr )
	select case( n->class )
	case ASTCLASS_CONSTI
		assert( (n->dtype = TYPE_LONGINT) or (n->dtype = TYPE_ULONGINT) )
		v.vali = astEvalConstiAsInt64( n )

	case ASTCLASS_CLOGNOT, ASTCLASS_NOT, ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS
		cppEval( v, n->head )
		select case( n->class )
		case ASTCLASS_CLOGNOT : v.vali =   -(v.vali = 0)
		case ASTCLASS_NOT     : v.vali = not v.vali
		case ASTCLASS_NEGATE  : v.vali =   - v.vali
		end select

	case ASTCLASS_OR, ASTCLASS_XOR, ASTCLASS_AND, _
	     ASTCLASS_CEQ, ASTCLASS_CNE, _
	     ASTCLASS_CLT, ASTCLASS_CLE, _
	     ASTCLASS_CGT, ASTCLASS_CGE, _
	     ASTCLASS_SHL, ASTCLASS_SHR, _
	     ASTCLASS_ADD, ASTCLASS_SUB, _
	     ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD
		dim as CPPVALUE r
		cppEval( v, n->head )
		cppEval( r, n->tail )

		select case( n->class )
		case ASTCLASS_DIV, ASTCLASS_MOD
			if( r.vali = 0 ) then
				astOops( n, "division by zero" )
			end if
		end select

		'' If one operand is unsigned, promote both operands to unsigned
		if( (v.dtype = TYPE_ULONGINT) or (r.dtype = TYPE_ULONGINT) ) then
			select case( n->class )
			case ASTCLASS_OR  : v.vali =   cunsg( v.vali ) or  cunsg( r.vali )
			case ASTCLASS_XOR : v.vali =   cunsg( v.vali ) xor cunsg( r.vali )
			case ASTCLASS_AND : v.vali =   cunsg( v.vali ) and cunsg( r.vali )
			case ASTCLASS_CEQ : v.vali = -(cunsg( v.vali ) =   cunsg( r.vali ))
			case ASTCLASS_CNE : v.vali = -(cunsg( v.vali ) <>  cunsg( r.vali ))
			case ASTCLASS_CLT : v.vali = -(cunsg( v.vali ) <   cunsg( r.vali ))
			case ASTCLASS_CLE : v.vali = -(cunsg( v.vali ) <=  cunsg( r.vali ))
			case ASTCLASS_CGT : v.vali = -(cunsg( v.vali ) >   cunsg( r.vali ))
			case ASTCLASS_CGE : v.vali = -(cunsg( v.vali ) >=  cunsg( r.vali ))
			case ASTCLASS_SHL : v.vali =   cunsg( v.vali ) shl cunsg( r.vali )
			case ASTCLASS_SHR : v.vali =   cunsg( v.vali ) shr cunsg( r.vali )
			case ASTCLASS_ADD : v.vali =   cunsg( v.vali ) +   cunsg( r.vali )
			case ASTCLASS_SUB : v.vali =   cunsg( v.vali ) -   cunsg( r.vali )
			case ASTCLASS_MUL : v.vali =   cunsg( v.vali ) *   cunsg( r.vali )
			case ASTCLASS_DIV : v.vali =   cunsg( v.vali ) \   cunsg( r.vali )
			case ASTCLASS_MOD : v.vali =   cunsg( v.vali ) mod cunsg( r.vali )
			case else         : assert( FALSE )
			end select
		else
			select case( n->class )
			case ASTCLASS_OR  : v.vali =   v.vali or  r.vali
			case ASTCLASS_XOR : v.vali =   v.vali xor r.vali
			case ASTCLASS_AND : v.vali =   v.vali and r.vali
			case ASTCLASS_CEQ : v.vali = -(v.vali =   r.vali)
			case ASTCLASS_CNE : v.vali = -(v.vali <>  r.vali)
			case ASTCLASS_CLT : v.vali = -(v.vali <   r.vali)
			case ASTCLASS_CLE : v.vali = -(v.vali <=  r.vali)
			case ASTCLASS_CGT : v.vali = -(v.vali >   r.vali)
			case ASTCLASS_CGE : v.vali = -(v.vali >=  r.vali)
			case ASTCLASS_SHL : v.vali =   v.vali shl r.vali
			case ASTCLASS_SHR : v.vali =   v.vali shr r.vali
			case ASTCLASS_ADD : v.vali =   v.vali +   r.vali
			case ASTCLASS_SUB : v.vali =   v.vali -   r.vali
			case ASTCLASS_MUL : v.vali =   v.vali *   r.vali
			case ASTCLASS_DIV : v.vali =   v.vali \   r.vali
			case ASTCLASS_MOD : v.vali =   v.vali mod r.vali
			case else         : assert( FALSE )
			end select
		end if

	case ASTCLASS_CLOGOR
		cppEval( v, n->head )
		if( v.vali ) then
			v.vali = 1
		else
			cppEval( v, n->tail )
			v.vali = iif( v.vali, 1, 0 )
		end if

	case ASTCLASS_CLOGAND
		cppEval( v, n->head )
		if( v.vali ) then
			cppEval( v, n->tail )
			v.vali = iif( v.vali, 1, 0 )
		else
			v.vali = 0
		end if

	case ASTCLASS_IIF
		cppEval( v, n->expr )
		if( v.vali ) then
			cppEval( v, n->head )
		else
			cppEval( v, n->tail )
		end if

	case ASTCLASS_ID
		'' Unexpanded identifier, assume it's a literal 0, like a CPP
		if( frog.verbose ) then
			astReport( n, "treating unexpanded identifier '" + *n->text + "' as literal zero" )
		end if

		'' And register as known undefined (it wasn't expanded so it
		'' must be undefined), so the warning won't be shown again.
		cppAddKnownUndefined( n->text )

		'' id  ->  0
		v.vali = 0

	case ASTCLASS_CDEFINED
		assert( n->head->class = ASTCLASS_ID )
		var id = n->head->text

		if( cppIsKnownSymbol( id ) = FALSE ) then
			'' Unknown symbol, assume it's undefined

			'' Show a warning if it seems to be useful; i.e. if it's
			'' not a reserved symbol, but one intended to be defined
			'' by the user.
			if( frog.verbose ) then
				if( strIsReservedIdInC( id ) = FALSE ) then
					astReport( n->head, "assuming symbol '" + *id + "' is undefined" )
				end if
			end if

			'' Register as known undefined
			'' This also prevents the above warning from being shown
			'' multiple times for a single symbol.
			cppAddKnownUndefined( id )
		end if

		'' defined()  ->  1|0
		v.vali = -cppIsMacroCurrentlyDefined( id )

	case else
		astOops( n, "couldn't evaluate #if condition" )
	end select

	v.dtype = n->dtype
end sub

'' Evaluates a CPP expression to true/false
private function cppEvalIfExpr( byval expr as ASTNODE ptr ) as integer
	cppEvalResultDtypes( expr )
	dim v as CPPVALUE
	cppEval( v, expr )
	function = (v.vali <> 0)
end function

private sub cppPush( byval state as integer )
	cpp.level += 1
	if( cpp.level >= MAXSTACK ) then
		tkOops( cpp.x, "#if/#include stack too small, MAXSTACK=" & MAXSTACK )
	end if
	with( cpp.stack(cpp.level) )
		.state = state
		.knownfile = -1
	end with
end sub

private sub cppPop( )
	cpp.level -= 1
end sub

private sub cppApplyIf( byval expr as ASTNODE ptr )
	if( cppEvalIfExpr( expr ) ) then
		'' #if TRUE, don't skip
		cpp.stack(cpp.level).state = STATE_TRUE
	else
		'' #if FALSE, start skipping
		cpp.skiplevel = cpp.level
	end if
end sub

private function cppIfExpr( ) as ASTNODE ptr
	'' Expand macros in the #if condition before parsing it
	'' * but don't expand operands of the "defined" operator
	'' * we allow "defined" operators to be produced by
	''   macro expansion, like gcc
	hExpandInRange( cpp.x, hSkipToEol( cpp.x ) - 1, TRUE )

	'' Try parsing an expression
	function = cppExpression( )
end function

private sub cppIf( )
	cppPush( STATE_IF )
	cpp.x += 1

	if( cppSkipping( ) ) then
		exit sub
	end if

	'' Condition expression
	cppApplyIf( cppIfExpr( ) )

	cppEol( )
end sub

private sub cppIfdef( byval directivekw as integer )
	cppPush( STATE_IF )
	cpp.x += 1

	if( cppSkipping( ) ) then
		exit sub
	end if

	'' Identifier
	if( tkGet( cpp.x ) < TK_ID ) then
		tkExpect( cpp.x, TK_ID, "behind " + tkInfoPretty( directivekw ) )
	end if
	var expr = astTakeLoc( astNewID( tkSpellId( cpp.x ) ), cpp.x )
	cpp.x += 1

	'' [!]defined id
	expr = astNew( ASTCLASS_CDEFINED, expr )
	if( directivekw = KW_IFNDEF ) then
		expr = astNew( ASTCLASS_CLOGNOT, expr )
	end if
	cppApplyIf( expr )

	cppEol( )
end sub

'' Forget the guardid (if any) for the current file context (if any).
private sub cppDisableIncludeGuardOptimization( )
	if( cpp.level >= 1 ) then
		with( cpp.stack(cpp.level-1) )
			if( .knownfile >= 0 ) then
				assert( .state = STATE_FILE )
				deallocate( cpp.files[.knownfile].guardid )
				cpp.files[.knownfile].guardid = NULL
			end if
		end with
	end if
end sub

private sub cppElseIf( )
	'' Verify #elif usage even if skipping
	select case( cpp.stack(cpp.level).state )
	case is < STATE_IF
		tkOops( cpp.x, "#elif without #if" )
	case STATE_ELSE
		tkOops( cpp.x, "#elif after #else" )
	end select
	cpp.x += 1

	cppDisableIncludeGuardOptimization( )

	'' Evaluate condition in case it matters:
	''    a) not yet skipping,
	''    b) skipping due to a previous #if/#elif FALSE
	if( (cpp.skiplevel = MAXSTACK) or (cpp.skiplevel = cpp.level) ) then
		'' But not if there already was an #if/#elif TRUE on this level
		'' (then this #elif isn't reached)
		if( cpp.stack(cpp.level).state = STATE_TRUE ) then
			'' Start/continue skipping
			cpp.skiplevel = cpp.level
		else
			'' Condition expression
			if( cppEvalIfExpr( cppIfExpr( ) ) ) then
				'' #elif TRUE, don't skip
				cpp.stack(cpp.level).state = STATE_TRUE
				cpp.skiplevel = MAXSTACK
			else
				'' #elif FALSE, start/continue skipping
				cpp.skiplevel = cpp.level
			end if

			cppEol( )
		end if
	end if
end sub

private sub cppElse( )
	'' Verify #else usage even if skipping
	select case( cpp.stack(cpp.level).state )
	case is < STATE_IF
		tkOops( cpp.x, "#else without #if" )
	case STATE_ELSE
		tkOops( cpp.x, "#else after #else" )
	end select
	cpp.x += 1

	cppDisableIncludeGuardOptimization( )

	cppEol( )

	'' Check whether to skip this #else or not, if
	''    a) not yet skipping,
	''    b) skipping due to a previous #if/#elif FALSE
	if( (cpp.skiplevel = MAXSTACK) or (cpp.skiplevel = cpp.level) ) then
		if( cpp.stack(cpp.level).state = STATE_TRUE ) then
			'' Previous #if/#elseif TRUE, skip #else
			cpp.skiplevel = cpp.level
		else
			'' Previous #if/#elseif FALSE, don't skip #else
			cpp.skiplevel = MAXSTACK
		end if
	end if

	cpp.stack(cpp.level).state = STATE_ELSE
end sub

private sub cppEndIf( )
	if( cpp.stack(cpp.level).state < STATE_IF ) then
		tkOops( cpp.x, "#endif without #if" )
	end if
	cpp.x += 1

	cppEol( )

	'' If skipping due to current level, then stop skipping.
	if( cpp.skiplevel = cpp.level ) then
		cpp.skiplevel = MAXSTACK
	end if

	cppPop( )
end sub

private sub cppWhitespace( )
	if( frog.whitespace ) then
		cppComments( cpp.x )
		cppDividers( cpp.x )
	end if
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

	'' 3. In any of the include search directories
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

private function hDetectIncludeGuard( byval first as integer, byval last as integer ) as zstring ptr
	assert( tkGet( first - 2 ) = TK_BEGININCLUDE )
	assert( tkGet( first - 1 ) = TK_EOL )
	assert( tkGet(  last + 1 ) = TK_ENDINCLUDE )

	var x = tkSkipCommentEol( first - 1 )

	'' Does it have the following at the beginning?
	''    #ifndef ID <EOL> #define ID ...
	if( tkGet( x ) <> TK_HASH ) then exit function
	x = tkSkipComment( x )
	if( tkGet( x ) <> KW_IFNDEF ) then exit function
	x = tkSkipComment( x )
	if( tkGet( x ) <> TK_ID ) then exit function
	var id1 = tkGetText( x )
	x = tkSkipComment( x )
	if( tkGet( x ) <> TK_EOL ) then exit function
	x = tkSkipComment( x )
	if( tkGet( x ) <> TK_HASH ) then exit function
	x = tkSkipComment( x )
	if( tkGet( x ) <> KW_DEFINE ) then exit function
	x = tkSkipComment( x )
	if( tkGet( x ) <> TK_ID ) then exit function
	var id2 = tkGetText( x )
	if( *id1 <> *id2 ) then exit function

	'' and an <EOL>#endif at the end?
	x = tkSkipCommentEol( last + 1, -1 )
	if( tkGet( x ) <> KW_ENDIF ) then exit function
	x = tkSkipComment( x, -1 )
	if( tkGet( x ) <> TK_HASH ) then exit function
	x = tkSkipComment( x, -1 )
	if( tkGet( x ) <> TK_EOL ) then exit function

	function = id1
end function

private sub cppInclude( byval begin as integer )
	cpp.x += 1

	assert( cppSkipping( ) = FALSE )

	'' "filename"
	tkExpect( cpp.x, TK_STRING, "containing the #include file name" )

	'' Save the location for later, across the insertions/deletions below
	var location = *tkGetLocation( cpp.x )

	'' Internal #include for a root file? Don't apply -filterin/-filterout,
	'' don't do #include file search...
	var is_rootfile = ((tkGetFlags( cpp.x ) and TKFLAG_ROOTFILE) <> 0)

	dim as string contextfile
	if( location.source ) then
		contextfile = *location.source->name
	end if
	var inctext = *tkGetText( cpp.x )

	cpp.x += 1

	cppEol( )

	dim incfile as string
	if( is_rootfile ) then
		incfile = inctext
	else
		incfile = hSearchHeaderFile( contextfile, inctext )
		if( len( incfile ) = 0 ) then
			frogPrint( inctext + " (not found)" )
			exit sub
		end if
	end if

	'' Get the normalized representation of the path, for use in hash tables
	'' etc. Otherwise foo.h from the root dir and ../foo.h from a subdir
	'' would be seen as different files.
	incfile = pathNormalize( pathMakeAbsolute( incfile ) )

	'' For display and -filterin/-filterout matching we use the version
	'' relative to curdir() though.
	var prettyfile = pathStripCurdir( incfile )
	var message = prettyfile

	'' Check -filterin/-filterout
	var keep = is_rootfile orelse cppKeepIncludeContent( prettyfile )
	if( keep = FALSE ) then
		message += " (filtered out)"
	end if
	frogPrint( message )

	'' Push the #include file context
	cppPush( STATE_FILE )

	'' Mark #include directive for removal
	tkSetRemove( begin, cpp.x - 1 )

	'' Insert this helper token so we can identify the start of #included
	'' tokens later during cPreParse().
	tkInsert( cpp.x, TK_BEGININCLUDE )
	if( keep = FALSE ) then
		tkAddFlags( cpp.x, cpp.x, TKFLAG_FILTEROUT )
	end if
	cpp.x += 1

	'' Insert EOL behind TK_BEGININCLUDE so we can can detect BOL there
	tkInsert( cpp.x, TK_EOL )
	tkSetRemove( cpp.x )
	cpp.x += 1

	'' Before loading the include file content, do the #include guard optimization.
	'' If this file had an #include guard last time we saw it, and the guard symbol
	'' is now defined, then we don't need to bother loading (lex + tkInsert()...)
	'' the file at all now.
	var load_include_file = TRUE
	'' Is the file known already?
	var knownfile = cppLookupKnownFile( incfile )
	if( knownfile >= 0 ) then
		'' Did it have an #include guard?
		var guardid = cpp.files[knownfile].guardid
		if( guardid ) then
			'' Only load the file if the guard symbol isn't defined (anymore) now.
			load_include_file = not cppIsMacroCurrentlyDefined( guardid )
		end if
	end if

	var y = cpp.x
	if( load_include_file ) then
		'' Read the include file and insert its tokens
		y = lexLoadC( y, sourcebufferFromFile( incfile, @location ), frog.whitespace )
	end if

	'' Put TK_ENDINCLUDE behind the #include file content, so we can detect
	'' the included EOF and pop the #include context from the cpp.stack.
	tkInsert( y, TK_ENDINCLUDE )
	y += 1

	'' Insert EOL behind the TK_ENDINCLUDE so we can detect BOL there
	tkInsert( y, TK_EOL )
	tkSetRemove( y )
	y += 1

	'' Start parsing the #included content
	'' (starting behind the EOL inserted above)
	assert( tkGet( cpp.x - 2 ) = TK_BEGININCLUDE )
	assert( tkGet( cpp.x - 1 ) = TK_EOL )
	assert( y <= tkGetCount( ) )
	assert( tkGet( y - 2 ) = TK_ENDINCLUDE )

	'' #include file not yet known?
	if( knownfile < 0 ) then
		'' Prepare for the include guard optimization: We have to check
		'' whether this include has ...
		''  * an include guard, #ifndef FOO + #define FOO + #endif
		''  * no tokens outside the include guard
		''  * no #elif/#else in that if block
		'' Check the first two points now:
		var guardid = hDetectIncludeGuard( cpp.x, y - 3 )

		'' Make the #include file known, with the guardid (if any),
		'' for the time being. Track it in the cpp.stack, so if we find
		'' an #elif/#else later, we can mark the include guard
		'' optimization as impossible by setting the guardid to NULL.
		'' (see cppDisableIncludeGuardOptimization())
		cpp.stack(cpp.level).knownfile = cppAppendKnownFile( incfile, guardid )
	end if

	if( load_include_file ) then
		cppWhitespace( )
	end if
end sub

private sub cppEndInclude( )
	assert( cpp.skiplevel = MAXSTACK )
	assert( cpp.level > 0 )
	if( cpp.stack(cpp.level).state >= STATE_IF ) then
		tkOops( cpp.x - 1, "missing #endif" )
	end if
	cppPop( )
	cpp.x += 1
end sub

private sub hReportConflictingDefine( byval a as ASTNODE ptr, byval b as ASTNODE ptr )
	'' Existing #define wasn't reported yet?
	if( (b->attrib and ASTATTRIB_REPORTED) = 0 ) then
		astReport( b, "conflicting #define for '" + *a->text + "', first one:", FALSE )
		b->attrib or= ASTATTRIB_REPORTED
	end if

	assert( (a->attrib and ASTATTRIB_REPORTED) = 0 )
	astReport( a, "conflicting #define for '" + *a->text + "', new one:", FALSE )
	a->attrib or= ASTATTRIB_REPORTED
end sub

'' DEFINE Identifier ['(' ParameterList ')'] Body Eol
private sub cppDefine( byval begin as integer, byref setremove as integer )
	cpp.x += 1

	assert( cppSkipping( ) = FALSE )

	'' Identifier ['(' ParameterList ')']
	var macro = hDefineHead( cpp.x )

	'' Body
	var xbody = cpp.x
	cpp.x = hSkipToEol( cpp.x )

	'' Eol
	var xeol = cpp.x
	assert( tkGet( xeol ) = TK_EOL )
	cpp.x += 1

	var definfo = definfoNew( )
	definfo->xdefine = begin
	definfo->xbody = xbody
	definfo->xeol = xeol
	definfo->macro = macro

	'' Report conflicting #defines
	var prevdef = cppLookupMacro( macro->text )
	if( prevdef ) then
		if( definfoDefinesAreEqual( prevdef, definfo ) = FALSE ) then
			hReportConflictingDefine( macro, prevdef->macro )
		end if
	end if

	cppDefineMacro( macro->text, definfo )

	'' Normally, we preserve #define directives (unlike the other CPP directives),
	'' thus no generic tkSetRemove() here. Unless the symbol was registed for removal.
	setremove = cppShouldRemoveSym( macro->text )
end sub

private sub cppUndef( )
	cpp.x += 1

	assert( cppSkipping( ) = FALSE )

	'' Identifier
	if( tkGet( cpp.x ) < TK_ID ) then
		tkExpect( cpp.x, TK_ID, "behind #undef" )
	end if
	var id = tkSpellId( cpp.x )
	cpp.x += 1

	cppUndefMacro( id )

	cppEol( )
end sub

private sub cppPragmaPushPopMacro( byval is_push as integer )
	cpp.x += 1

	var whatfor = iif( is_push, _
		@"for #pragma push_macro(""..."")", _
		@"for #pragma pop_macro(""..."")" )

	'' '('
	tkExpect( cpp.x, TK_LPAREN, whatfor )
	cpp.x += 1

	'' "..."
	tkExpect( cpp.x, TK_STRING, whatfor )
	var id = *tkGetText( cpp.x )
	cpp.x += 1

	'' ')'
	tkExpect( cpp.x, TK_RPAREN, whatfor )
	cpp.x += 1

	if( is_push ) then
		cppSaveMacro( id )
	else
		cppRestoreMacro( id )
	end if
end sub

private function cppPragma( byref setremove as integer ) as integer
	select case( tkSpell( cpp.x ) )
	'' #pragma message("...")
	case "message"
		cpp.x += 1
		var whatfor = @"for #pragma message(""..."")"

		'' '('
		tkExpect( cpp.x, TK_LPAREN, whatfor )
		cpp.x += 1

		'' "..."
		tkExpect( cpp.x, TK_STRING, whatfor )
		cpp.x += 1

		'' ')'
		tkExpect( cpp.x, TK_RPAREN, whatfor )
		cpp.x += 1

	'' MSVC:
	'' #pragma comment(lib, "<library file name>")
	case "comment"
		cpp.x += 1

		'' '('
		tkExpect( cpp.x, TK_LPAREN, "for #pragma comment(...)" )
		cpp.x += 1

		select case( tkSpell( cpp.x ) )
		case "lib"
			cpp.x += 1

			'' ','
			tkExpect( cpp.x, TK_COMMA, "for #pragma comment(lib, ""..."")" )
			cpp.x += 1

			'' "..."
			tkExpect( cpp.x, TK_STRING, "for #pragma comment(lib, ""..."")" )
			cpp.x += 1

			'' Preserve the #pragma comment(lib, "...") for the C parser
			setremove = FALSE

		case else
			exit function
		end select

		'' ')'
		tkExpect( cpp.x, TK_RPAREN, "for #pragma comment(...)" )
		cpp.x += 1

	case "GCC"
		cpp.x += 1

		select case( tkSpell( cpp.x ) )
		case "system_header"
			cpp.x += 1

		case else
			exit function
		end select

	'' #pragma pack(N)
	'' #pragma pack()
	'' #pragma pack(push, N)
	'' #pragma pack(pop)
	case "pack"
		cpp.x += 1

		'' Just skip to EOL and let the C parser worry about checking
		'' the syntax
		cpp.x = hSkipToEol( cpp.x )

		'' Preserve the #pragma pack for the C parser
		setremove = FALSE

	case "push_macro"
		cppPragmaPushPopMacro( TRUE )

	case "pop_macro"
		cppPragmaPushPopMacro( FALSE )

	case else
		exit function
	end select

	cppEol( )
	function = TRUE
end function

private sub cppDirective( )
	'' '#'
	var begin = cpp.x
	assert( tkGet( cpp.x ) = TK_HASH )
	cpp.x += 1

	var directivekw = tkGet( cpp.x )

	'' When skipping, only #if/#elif/#else/#endif directives are handled,
	'' anything else (even invalid directives) must be ignored.
	if( cppSkipping( ) ) then
		select case( directivekw )
		case KW_IF, KW_IFDEF, KW_IFNDEF, KW_ELIF, KW_ELSE, KW_ENDIF

		case else
			tkSetRemove( begin, cpp.x )
			cpp.x += 1
			exit sub
		end select
	end if

	var setremove = TRUE

	select case( directivekw )
	case KW_IF
		cppIf( )

	case KW_IFDEF, KW_IFNDEF
		cppIfdef( directivekw )

	case KW_ELIF
		cppElseIf( )

	case KW_ELSE
		cppElse( )

	case KW_ENDIF
		cppEndIf( )

	case KW_INCLUDE
		cppInclude( begin )
		setremove = FALSE

	case KW_DEFINE
		cppDefine( begin, setremove )

	case KW_UNDEF
		cppUndef( )

	case KW_PRAGMA
		cpp.x += 1
		if( cppPragma( setremove ) = FALSE ) then
			tkOops( cpp.x, "unknown #pragma" )
		end if

	case KW_ERROR
		'' Not using the #error's text as error message,
		'' otherwise it would be mistaken for being generated by fbfrog.
		tkOops( cpp.x, "#error" )

	case KW_WARNING
		cpp.x += 1
		'' ditto
		print tkReport( cpp.x, "#warning" )
		cpp.x = hSkipToEol( cpp.x ) + 1

	case TK_EOL
		'' '#' followed by EOL (accepted by gcc/clang too)
		cpp.x += 1

	case else
		tkOops( cpp.x, "unknown PP directive" )
	end select

	if( setremove ) then
		tkSetRemove( begin, cpp.x - 1 )
	end if
end sub

private function hIsAtBOL( byval x as integer ) as integer
	select case( tkGet( x - 1 ) )
	case TK_EOL, TK_EOF, TK_DIVIDER
		function = TRUE
	end select
end function

private sub cppNext( )
	select case( tkGet( cpp.x ) )
	case TK_ENDINCLUDE
		cppEndInclude( )
		exit sub

	'' '#'
	case TK_HASH
		'' Parse directive if at BOL and the '#' token isn't the result of a macro expansion
		'' We do this for every "toplevel" '#', before ever doing macro expansion behind it,
		'' so it should be safe to assume that if the '#' isn't coming from a macro expansion,
		'' the rest isn't either.
		if( hIsAtBOL( cpp.x ) and (tkGetExpansionLevel( cpp.x ) = 0) ) then
			cppDirective( )
			exit sub
		end if

	'' _Pragma("...")
	case KW__PRAGMA
		if( cppSkipping( ) = FALSE ) then
			var begin = cpp.x
			cpp.x += 1

			'' '('
			tkExpect( cpp.x, TK_LPAREN, "behind _Pragma" )
			cpp.x += 1

			'' StringLiteral
			tkExpect( cpp.x, TK_STRING, "inside _Pragma()" )
			var text = *tkGetText( cpp.x )
			cpp.x += 1

			'' ')'
			tkExpect( cpp.x, TK_RPAREN, "to close _Pragma" )
			cpp.x += 1

			'' Insert #pragma corresponding to the _Pragma(),
			'' while ensuring to have EOL in front of and behind it,
			'' mark the _Pragma() for removal, then we can parse the
			'' #pragma as usual.
			tkSetRemove( begin, cpp.x - 1 )
			var pragma = !"\n#pragma " + text
			if( tkGet( cpp.x ) <> TK_EOL ) then
				pragma += !"\n"
			end if
			lexLoadC( cpp.x, sourcebufferFromZstring( "_Pragma(" + text + ")", pragma, tkGetLocation( begin ) ), FALSE )
			exit sub
		end if

	'' Identifier/keyword? Check whether it needs to be macro-expanded
	case is >= TK_ID
		if( cppSkipping( ) = FALSE ) then
			hMaybeExpandMacro( cpp.x, FALSE )
			cpp.x += 1
			exit sub
		end if

	'' Remove standalone EOLs, so the C parser doesn't have to handle them
	case TK_EOL
		tkSetRemove( cpp.x )
		cpp.x += 1
		exit sub
	end select

	'' Some token that doesn't matter to the CPP
	if( cppSkipping( ) ) then
		tkSetRemove( cpp.x )
	end if
	cpp.x += 1
end sub

sub cppMain( )
	cppWhitespace( )
	while( tkGet( cpp.x ) <> TK_EOF )
		cppNext( )
	wend
	cppEnd( )
end sub

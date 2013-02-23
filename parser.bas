'' C parsing passes

#include once "fbfrog.bi"

type UNDOSTEP
	is_insert	as integer  '' Was it an insertion or deletion?
	x		as integer  '' position where the modification happened

	'' For deletions only: data of the token that was deleted
	tk		as ONETOKEN
end type

type UNDOSTACK
	begin		as integer
	steps		as TLIST  '' UNDOSTEPs
end type

type PARSERSTUFF
	x		as integer  '' current position

	'' Stack of history stacks
	'' The undo stack allows token buffer modifications to be reverted.
	'' This allows the parser to remove/insert tokens while trying to parse
	'' a certain statement. If one parsing try fails, it can just revert
	'' and try another construct.
	undostacks	as TLIST  '' UNDOSTACKs
end type

dim shared as PARSERSTUFF pass

private sub hDropSteps( byval stack as UNDOSTACK ptr )
	dim as UNDOSTEP ptr stp = any

	do
		stp = listGetTail( @stack->steps )
		if( stp = NULL ) then
			exit do
		end if

		'' Free memory
		if( stp->is_insert = FALSE ) then
			tkDtor( @stp->tk )
		end if

		listDelete( @stack->steps, stp )
	loop
end sub

sub passPush( )
	dim as UNDOSTACK ptr stack = any

	stack = listAppend( @pass.undostacks )
	stack->begin = pass.x
	listInit( @stack->steps, sizeof( UNDOSTEP ) )
end sub

sub passPop( )
	dim as UNDOSTACK ptr stack = any

	stack = listGetTail( @pass.undostacks )
	hDropSteps( stack )
	listEnd( @stack->steps )
	listDelete( @pass.undostacks, stack )
end sub

sub passInit( )
	pass.x = 0
	listInit( @pass.undostacks, sizeof( UNDOSTACK ) )

	passPush( )
end sub

sub passEnd( )
	passPop( )

	assert( listGetTail( @pass.undostacks ) = NULL )
	listEnd( @pass.undostacks )
end sub

sub passTryBegin( )
	dim as UNDOSTACK ptr stack = any

	stack = listGetTail( @pass.undostacks )
	stack->begin = pass.x
	hDropSteps( stack )
end sub

sub passTryAgain( )
	dim as UNDOSTACK ptr stack = any
	dim as UNDOSTEP ptr stp = any

	stack = listGetTail( @pass.undostacks )

	'' Apply inverse operations
	stp = listGetTail( @stack->steps )
	while( stp )
		if( stp->is_insert ) then
			'' It was an insertion, delete the token
			tkRemove( stp->x, stp->x )
		else
			'' It was a deletion, re-insert the token
			tkInsert( stp->x, stp->tk.id, stp->tk.text, _
					stp->tk.dtype, stp->tk.subtype, _
					stp->tk.location )
		end if
		stp = listGetPrev( stp )
	wend

	hDropSteps( listGetTail( @pass.undostacks ) )

	pass.x = stack->begin
end sub

function passGet( ) as integer
	function = tkGet( pass.x )
end function

function passGetLookAhead( byval n as integer ) as integer
	function = tkGet( pass.x + n )
end function

function passGetText( ) as zstring ptr
	function = tkGetText( pass.x )
end function

sub passSkip( )
	dim as UNDOSTACK ptr stack = any
	dim as UNDOSTEP ptr stp = any
	dim as ONETOKEN ptr tk = any

	stack = listGetTail( @pass.undostacks )
	stp = listAppend( @stack->steps )

	stp->is_insert = FALSE
	stp->x = pass.x
	tk = tkAccess( pass.x )
	tkCtor( @stp->tk, tk->id, tk->text, tk->dtype, tk->subtype, tk->location )

	tkRemove( pass.x, pass.x )
end sub

sub passInsert _
	( _
		byval tk as integer, _
		byval text as zstring ptr = NULL, _
		byval dtype as integer = TYPE_NONE, _
		byval subtype as zstring ptr = NULL _
	)

	dim as UNDOSTACK ptr stack = any
	dim as UNDOSTEP ptr stp = any

	stack = listGetTail( @pass.undostacks )
	stp = listAppend( @stack->steps )

	stp->is_insert = TRUE
	stp->x         = pass.x

	tkInsert( pass.x, tk, text, dtype, subtype )
	pass.x += 1

end sub

function passMatch( byval tk as integer ) as integer
	if( passGet( ) = tk ) then
		passSkip( )
		function = TRUE
	else
		function = FALSE
	end if
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

declare function cStructCompound( ) as integer
declare function cMultDecl( byval decl as integer ) as integer

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function cFindEOL( byval x as integer ) as integer
	while( tkIsStmtSep( x ) = FALSE )
		x += 1
	wend
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
		x += 1

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
			x += 1
			exit do

		case TK_LPAREN, TK_LBRACKET, TK_LBRACE
			x = cFindClosingParen( x )

		case else
			x += 1
		end select
	loop

	function = x
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Remove all comments unless they're at EOL
sub cPurgeInlineComments( )
	dim as integer x = any

	x = 0
	do
		select case( tkGet( x ) )
		case TK_COMMENT
			if( tkIsStmtSep( x + 1 ) = FALSE ) then
				tkRemove( x, x )
				x -= 1
			end if

		case TK_EOF
			exit do
		end select

		x += 1
	loop
end sub

private function cPPDirective( byval x as integer ) as integer
	dim as integer begin = any

	'' '#'
	begin = x
	x += 1

	select case( tkGet( x ) )
	'' DEFINE Identifier ['(' ParameterList ')'] Body Eol .
	case KW_DEFINE
		'' DEFINE
		x += 1

		'' Identifier?
		if( tkGet( x ) <> TK_ID ) then
			return begin
		end if

		tkInsert( begin, TK_PPDEFINEBEGIN, tkGetText( x ) )
		begin += 1
		tkRemove( begin, x )
		x = begin

		'' '(' and not separated from the Identifier with spaces?
		'' TODO

		'' Body
		x = cFindEOL( x )
		tkInsert( x, TK_PPDEFINEEND )
		x += 1

	case KW_INCLUDE
		'' INCLUDE
		x += 1

		if( tkGet( x ) <> TK_STRING ) then
			return begin
		end if

		tkInsert( begin, TK_PPINCLUDE, tkGetText( x ) )
		begin += 1
		tkRemove( begin, x )
		x = begin

	case else
		return begin
	end select

	function = x
end function

sub cPPDirectives( )
	dim as integer x = any, old = any

	x = 0
	do
		old = x

		select case( tkGet( x ) )
		'' '#'?
		case TK_HASH
			'' BOL?
			if( tkIsStmtSep( x - 1 ) ) then
				x = cPPDirective( x )
				if( x <> old ) then
					x -= 1
				end if
			end if

		case TK_EOF
			exit do

		end select

		x += 1
	loop
end sub

'' After PP directives were parsed, EOLs can be removed completely
sub cPurgeEOLs( )
	dim as integer x = any

	x = 0
	do
		select case( tkGet( x ) )
		case TK_EOL
			tkRemove( x, x )
			x -= 1

		case TK_EOF
			exit do
		end select

		x += 1
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' (MultDecl{Field} | StructCompound)*
private sub cStructBody( )
	passPush( )

	do
		passTryBegin( )
		if( cMultDecl( TK_FIELD ) = FALSE ) then
			passTryAgain( )
			if( cStructCompound( ) = FALSE ) then
				passTryAgain( )
				exit do
			end if
		end if
	loop

	passPop( )
end sub

'' {STRUCT|UNION} [Identifier] '{' StructBody '}'
private function cStructCompound( ) as integer
	dim as string id

	function = FALSE

	'' {STRUCT|UNION}
	select case( passGet( ) )
	case KW_STRUCT, KW_UNION

	case else
		exit function
	end select
	passSkip( )

	'' [Identifier]
	if( passGet( ) = TK_ID ) then
		id = *passGetText( )
		passSkip( )
	end if

	passInsert( TK_STRUCTBEGIN, id )

	'' '{'
	if( passMatch( TK_LBRACE ) = FALSE ) then
		exit function
	end if

	cStructBody( )

	passInsert( TK_STRUCTEND )

	'' '}'
	if( passMatch( TK_RBRACE ) = FALSE ) then
		exit function
	end if

	'' ';'
	function = passMatch( TK_SEMI )
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' (CONST)*
private sub cConstMod( byref dtype as integer )
	while( passMatch( KW_CONST ) )
		dtype = typeSetIsConst( dtype )
	wend
end sub

private function cBaseType _
	( _
		byref dtype as integer, _
		byref subtype as string _
	) as integer

	dim as integer sign = any

	function = FALSE
	dtype = TYPE_NONE
	subtype = ""
	sign = 0

	'' [CONST]
	cConstMod( dtype )

	'' [SIGNED|UNSIGNED]
	select case( passGet( ) )
	case KW_SIGNED
		sign = -1
		passSkip( )
	case KW_UNSIGNED
		sign = 1
		passSkip( )
	end select

	select case( passGet( ) )
	case KW_ENUM, KW_STRUCT, KW_UNION
		if( sign <> 0 ) then
			exit function
		end if

		'' {ENUM|STRUCT|UNION}
		passSkip( )

		'' Identifier
		if( passGet( ) <> TK_ID ) then
			exit function
		end if
		dtype = typeSetDt( dtype, TYPE_UDT )
		subtype = *passGetText( )
		passSkip( )

	case TK_ID
		if( sign <> 0 ) then
			exit function
		end if

		'' Identifier
		dtype = typeSetDt( dtype, TYPE_UDT )
		subtype = *passGetText( )
		passSkip( )

	case KW_VOID
		if( sign <> 0 ) then
			exit function
		end if

		passSkip( )
		dtype = typeSetDt( dtype, TYPE_ANY )

	case KW_CHAR
		passSkip( )
		select case( sign )
		case -1
			dtype = typeSetDt( dtype, TYPE_BYTE )
		case 1
			dtype = typeSetDt( dtype, TYPE_UBYTE )
		case else
			dtype = typeSetDt( dtype, TYPE_ZSTRING )
		end select

	case KW_FLOAT
		passSkip( )
		dtype = typeSetDt( dtype, TYPE_SINGLE )

	case KW_DOUBLE
		passSkip( )
		dtype = typeSetDt( dtype, TYPE_DOUBLE )

	case KW_SHORT
		passSkip( )
		if( sign > 0 ) then
			dtype = typeSetDt( dtype, TYPE_USHORT )
		else
			dtype = typeSetDt( dtype, TYPE_SHORT )
		end if

		'' [INT]
		passMatch( KW_INT )

	case KW_INT
		passSkip( )
		if( sign > 0 ) then
			dtype = typeSetDt( dtype, TYPE_ULONG )
		else
			dtype = typeSetDt( dtype, TYPE_LONG )
		end if

	case KW_LONG
		passSkip( )
		if( sign > 0 ) then
			dtype = typeSetDt( dtype, TYPE_ULONG )
		else
			dtype = typeSetDt( dtype, TYPE_LONG )
		end if

		'' [LONG]
		if( passMatch( KW_LONG ) ) then
			if( sign > 0 ) then
				dtype = typeSetDt( dtype, TYPE_ULONGINT )
			else
				dtype = typeSetDt( dtype, TYPE_LONGINT )
			end if
		end if

		'' [INT]
		passMatch( KW_INT )

	case else
		exit function
	end select

	'' [CONST]
	cConstMod( dtype )

	function = TRUE
end function

private sub cPtrCount( byref dtype as integer )
	'' Pointers: ('*')*
	while( passMatch( TK_STAR ) )
		dtype = typeAddrOf( dtype )

		'' [CONST]
		cConstMod( dtype )
	wend
end sub

''    '...' | MultDecl{Param}
private function cParamDecl( ) as integer
	'' '...'?
	if( passMatch( TK_ELLIPSIS ) ) then
		passInsert( TK_PARAMVARARG )
		function = TRUE
	else
		function = cMultDecl( TK_PARAM )
	end if
end function

private function cParamDeclList( ) as integer
	function = FALSE

	'' [ParamDecl (',' ParamDecl)*]
	do
		if( cParamDecl( ) = FALSE ) then
			exit function
		end if

		'' ','
	loop while( passMatch( TK_COMMA ) )

	function = TRUE
end function

'' plain id: PtrCount Identifier
'' procptr:  PtrCount '(' '*' Identifier ')' '(' ParamList ')'
'' proto:    PtrCount Identifier '(' ParamList ')'
private function cDeclElement _
	( _
		byval decl as integer, _
		byval basedtype as integer, _
		byref basesubtype as string _
	) as integer

	dim as integer dtype = any, is_procptr = any, has_params = any
	dim as string id

	function = FALSE
	dtype = basedtype

	'' PtrCount
	cPtrCount( dtype )

	'' '('?
	if( passMatch( TK_LPAREN ) ) then
		'' '*'
		if( passMatch( TK_STAR ) = FALSE ) then
			exit function
		end if

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
		case else
			return FALSE
		end select
		is_procptr = TRUE
	else
		is_procptr = FALSE
	end if

	'' Identifier (must be there except for params)
	if( passGet( ) = TK_ID ) then
		id = *passGetText( )
		passSkip( )
	else
		select case( decl )
		case TK_PARAM, TK_PARAMPROCPTR, TK_PARAMVARARG

		case else
			return FALSE
		end select
	end if

	if( is_procptr ) then
		'' ')'
		if( passMatch( TK_RPAREN ) = FALSE ) then
			exit function
		end if
	end if

	'' '('? (procedure parameters)
	if( passMatch( TK_LPAREN ) ) then
		select case( decl )
		case TK_GLOBAL, TK_EXTERNGLOBAL, TK_STATICGLOBAL, TK_FIELD
			decl = TK_PROC
		case TK_GLOBALPROCPTR, TK_EXTERNGLOBALPROCPTR, _
		     TK_PARAMPROCPTR, TK_FIELDPROCPTR

		case else
			exit function
		end select

		has_params = TRUE
	else
		'' If it's a function pointer there must also be a parameter list
		if( is_procptr ) then
			exit function
		end if
		has_params = FALSE
	end if

	passInsert( decl, id, dtype, basesubtype )

	if( has_params ) then
		'' Just '(void)'?
		if( (passGet( ) = KW_VOID) and (passGetLookAhead( 1 ) = TK_RPAREN) ) then
			'' VOID
			passSkip( )
		'' Not just '()'?
		elseif( passGet( ) <> TK_RPAREN ) then
			if( cParamDeclList( ) = FALSE ) then
				exit function
			end if
		end if

		'' ')'
		if( passMatch( TK_RPAREN ) = FALSE ) then
			exit function
		end if
	end if

	function = TRUE
end function

''
'' Generic 'type *a, **b;' parsing, used for vars/fields/protos/params
'' ("multiple declaration" syntax)
''    int i;
''    int a, b, c;
''    int *a, ***b, c;
''    int f(void);
''    int (*procptr)(void);
''
''    BaseType DeclElement (',' DeclElement)* [';']
''
private function cMultDecl( byval decl as integer ) as integer
	dim as integer dtype = any
	dim as string subtype

	function = FALSE

	'' BaseType
	if( cBaseType( dtype, subtype ) = FALSE ) then
		exit function
	end if

	'' ... (',' ...)*
	do
		if( cDeclElement( decl, dtype, subtype ) = FALSE ) then
			exit function
		end if

		'' Everything can have a comma and more identifiers,
		'' except for params.
		select case( decl )
		case TK_PARAM, TK_PARAMPROCPTR, TK_PARAMVARARG
			exit do
		end select

		'' ','?
	loop while( passMatch( TK_COMMA ) )

	'' Everything except params must end with a ';'
	select case( decl )
	case TK_PARAM, TK_PARAMPROCPTR, TK_PARAMVARARG

	case else
		'' ';'
		if( passMatch( TK_SEMI ) = FALSE ) then
			exit function
		end if
	end select

	function = TRUE
end function

'' Global variable/procedure declarations
''    [EXTERN|STATIC] MultDecl
private function cGlobalDecl( ) as integer
	dim as integer decl = any

	function = FALSE

	select case( passGet( ) )
	case KW_EXTERN
		decl = TK_EXTERNGLOBAL
		passSkip( )

	case KW_STATIC
		decl = TK_STATICGLOBAL
		passSkip( )

	case else
		decl = TK_GLOBAL
	end select

	function = cMultDecl( decl )
end function

private sub cUnknown( )
	if( passGet( ) = TK_EOF ) then
		exit sub
	end if

	passInsert( TK_TODOBEGIN, "unknown construct (sorry)" )

	pass.x = cSkipStatement( pass.x )

	passInsert( TK_TODOEND )
end sub

sub cToplevel( )
	passInit( )

	while( passGet( ) <> TK_EOF )
		passTryBegin( )
		if( cStructCompound( ) = FALSE ) then
			passTryAgain( )
			if( cGlobalDecl( ) = FALSE ) then
				passTryAgain( )
				cUnknown( )
			end if
		end if
	wend

	passEnd( )
end sub

''
'' C parsing
''
'' cFile() parses the content of the tk buffer and returns the resulting AST.
''
'' The parser is able to recover from parsing errors, such that it can continue
'' parsing the next construct if parsing the current one failed. The bad
'' constructs are preserved in the AST in form of a list of tokens, together
'' with the first error message generated when parsing it.
''
'' Even though the parser can handle a lot, it's still incomplete, and it will
'' never be able to handle all possible #define bodies, so the error recovery
'' will never really be obsolete, unless the user is asked to restart fbfrog
'' everytime, while adding certain command line options to tell fbfrog to remove
'' the bad construct before trying to parse it the next time. But why bother the
'' user with that, when it can be done automatically? It makes the code here
'' more ugly, but that's not really an excuse.
''
'' Rules:
''  * All loops during cConstruct() must check parseok
''

#include once "fbfrog.bi"

enum
	DECL_EXTERNVAR = 0
	DECL_GLOBALVAR
	DECL_GLOBALSTATICVAR
	DECL_LOCALVAR
	DECL_LOCALSTATICVAR
	DECL_FIELD
	DECL_PARAM
	DECL_TYPEDEF
	DECL_CASTTYPE
	DECL_SIZEOFTYPE
	DECL__COUNT
end enum

dim shared as integer decl_to_astclass(0 to DECL__COUNT-1) = _
{ _
	ASTCLASS_VAR    , _ '' DECL_EXTERNVAR
	ASTCLASS_VAR    , _ '' DECL_GLOBALVAR
	ASTCLASS_VAR    , _ '' DECL_GLOBALSTATICVAR
	ASTCLASS_VAR    , _ '' DECL_LOCALVAR
	ASTCLASS_VAR    , _ '' DECL_LOCALSTATICVAR
	ASTCLASS_FIELD  , _ '' DECL_FIELD
	ASTCLASS_PARAM  , _ '' DECL_PARAM
	ASTCLASS_TYPEDEF, _ '' DECL_TYPEDEF
	ASTCLASS_TYPE   , _ '' DECL_CASTTYPE
	ASTCLASS_TYPE     _ '' DECL_SIZEOFTYPE
}

enum
	BODY_TOPLEVEL = 0
	BODY_SCOPE
	BODY_STRUCT
	BODY_ENUM
end enum

declare function cExpression _
	( _
		byval level as integer, _
		byval macro as ASTNODE ptr _
	) as ASTNODE ptr
declare function cDeclaration _
	( _
		byval decl as integer, _
		byval gccattribs as integer, _
		byref comment as string _
	) as ASTNODE ptr
declare function cScope( ) as ASTNODE ptr
declare function cConstruct( byval body as integer ) as ASTNODE ptr
declare function cBody( byval body as integer ) as ASTNODE ptr

dim shared as integer x, parseok
dim shared as string errmsg
dim shared typedefs as THASH

private sub hAddTypedef( byval id as zstring ptr )
	if( frog.verbose ) then
		print "registering typedef '" + *id + "'"
	end if
	hashAddOverwrite( @typedefs, id, NULL )
end sub

private function hIsTypedef( byval id as zstring ptr ) as integer
	function = (hashLookup( @typedefs, id, hashHash( id ) )->s <> NULL)
end function

private function cMatch( byval tk as integer ) as integer
	if( tkGet( x ) = tk ) then
		x += 1
		function = TRUE
	end if
end function

private sub cError( byval message as zstring ptr )
	if( parseok ) then
		parseok = FALSE
		errmsg = tkReport( x, message )
	end if
end sub

private sub cExpectMatch( byval tk as integer, byval message as zstring ptr )
	if( tkGet( x ) = tk ) then
		x += 1
	elseif( parseok ) then
		parseok = FALSE
		errmsg = tkReport( x, tkMakeExpectedMessage( x, tkInfoPretty( tk ) + " " + *message ) )
	end if
end sub

'' Generate place-holder names for unnamed structs/unions/enums when needed.
'' The id should be unique, no name conflicts should be introduced due to this,
'' and multiple structs within one parsing pass or from separate parsing passes
'' shouldn't share the same id.
private function hMakeDummyId( ) as string
	static n as integer
	function = DUMMYID_PREFIX & n
	n += 1
end function

private function hIdentifyCommonTypedef( byval id as zstring ptr ) as integer
	select case( *id )
	case "size_t"			: function = TYPE_UINTEGER
	case "ssize_t", "ptrdiff_t"	: function = TYPE_INTEGER
	case "int8_t", "__int8"		: function = TYPE_BYTE
	case "uint8_t"			: function = TYPE_UBYTE
	case "int16_t", "__int16"	: function = TYPE_SHORT
	case "uint16_t"			: function = TYPE_USHORT
	case "int32_t", "__int32"	: function = TYPE_LONG
	case "uint32_t"			: function = TYPE_ULONG
	case "int64_t", "__int64"	: function = TYPE_LONGINT
	case "uint64_t"			: function = TYPE_ULONGINT
	case "wchar_t"			: function = TYPE_WSTRING
	case else			: function = TYPE_NONE
	end select
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' ("..." | #id)*
private function hStringLiteralSequence( ) as ASTNODE ptr
	dim as ASTNODE ptr a

	while( parseok )
		dim as ASTNODE ptr s

		select case( tkGet( x ) )
		case TK_STRING
			s = astNew( ASTCLASS_STRING, tkGetText( x ) )
			astSetType( s, TYPE_ZSTRING, NULL )

		case TK_WSTRING
			s = astNew( ASTCLASS_STRING, tkGetText( x ) )
			astSetType( s, TYPE_WSTRING, NULL )

		'' '#' stringify operator
		case TK_HASH
			'' #id?
			if( tkGet( x + 1 ) <> TK_ID ) then
				exit while
			end if
			x += 1

			s = astNew( ASTCLASS_STRINGIFY, astNewID( tkGetText( x ) ) )

		case else
			exit while
		end select

		if( a = NULL ) then
			a = s
		else
			a = astNew( ASTCLASS_STRCAT, a, s )
		end if

		x += 1
	wend

	function = a
end function

private function hIdentifierIsMacroParam _
	( _
		byval macro as ASTNODE ptr, _
		byval id as zstring ptr _
	) as integer
	if( macro ) then
		function = (astLookupMacroParam( macro, id ) >= 0)
	else
		function = FALSE
	end if
end function

''
'' Trying to disambiguate between DataType and Expression: Even without being a
'' full C compiler, and even without seeing the whole C source (system #includes
'' etc), good guesses can be made.
''
'' If it starts with a data type keyword, and isn't inside a macro where that's
'' a macro parameter, then it must be a data type, because it couldn't appear in
'' an expression.
''
'' Of course that's an unsafe assumption because any identifier could have been
'' re-#defined to something different than what fbfrog assumes, in #include
'' files that fbfrog doesn't even parse, etc... but for common typedefs such as
'' size_t that shouldn't be a problem in practice.
''
'' If there's just an identifier then it could be a typedef but we can't be
'' sure. Finding out whether it is a typedef would require checking all previous
'' declarations in this file and in #includes, that's not possible currently
'' because #includes aren't always merged in.
''
'' Note: fbfrog could show a warning then making such an unsafe assumption,
'' but on the other hand, that's rather pointless because without seeing
'' all #defines, no C code is safe to parse. If int/void etc. are re-#defined
'' without fbfrog knowing then the for example the declaration parser would
'' make the same mistake, but it doesn't show any warning. That would be crazy
'' to do for every re-#definable keyword...
''
private function hIsDataType _
	( _
		byval y as integer, _
		byval macro as ASTNODE ptr _
	) as integer

	var is_type = FALSE

	select case( tkGet( y ) )
	case KW___CDECL, KW___STDCALL, KW___ATTRIBUTE__, _
	     KW_SIGNED, KW_UNSIGNED, KW_CONST, KW_SHORT, KW_LONG, _
	     KW_ENUM, KW_STRUCT, KW_UNION, _
	     KW_VOID, KW_CHAR, KW_FLOAT, KW_DOUBLE, KW_INT
		is_type = not hIdentifierIsMacroParam( macro, tkSpellId( y ) )
	case TK_ID
		var id = tkSpellId( y )
		if( (hIdentifyCommonTypedef( id ) <> TYPE_NONE) or _
		    hIsTypedef( id ) ) then
			is_type = not hIdentifierIsMacroParam( macro, id )
		end if
	end select

	function = is_type
end function

'' Parse cast expression for '(DataType) foo', or sizeof operand for
'' 'sizeof (DataType)'.
private function hDataTypeInParens( byval decl as integer ) as ASTNODE ptr
	''
	'' Using cDeclaration() to parse:
	''
	''    BaseType Declarator
	''
	'' Parsing just the base data type isn't enough, because it could be a
	'' function pointer cast with parameter list etc. We need to do full
	'' declarator parsing to handle that.
	''
	'' cDeclaration() will have built up a GROUP, for DECL_CASTTYPE there
	'' should be 1 child only though, extract it.
	''
	function = astUngroupOne( cDeclaration( decl, 0, "" ) )

	'' ')'
	cExpectMatch( TK_RPAREN, iif( decl = DECL_CASTTYPE, _
			@"to close '(...)' type cast", _
			@"to close 'sizeof (...)'" ) )
end function

'' Scope block: '{' (Expression ';')* '}'
'' Initializer: '{' Expression (',' Expression)* '}'
private function hScopeBlockOrInitializer( byval macro as ASTNODE ptr ) as ASTNODE ptr
	var a = astNewGROUP( )
	x += 1

	'' '}'?
	while( (tkGet( x ) <> TK_RBRACE) and parseok )
		astAppend( a, cExpression( 0, macro ) )

		select case( a->class )
		case ASTCLASS_STRUCTINIT
			'' '}'?
			if( tkGet( x ) = TK_RBRACE ) then exit while

			'' ','
			cExpectMatch( TK_COMMA, "(expression separator in struct initializer)" )

		case ASTCLASS_SCOPEBLOCK
			'' ';'
			cExpectMatch( TK_SEMI, "(end of statement in scope block)" )

		case else
			'' ',' -> it's a struct initializer
			'' ';' -> it's a scope block
			'' '}' -> end
			select case( tkGet( x ) )
			case TK_COMMA
				a->class = ASTCLASS_STRUCTINIT
				x += 1

			case TK_SEMI
				a->class = ASTCLASS_SCOPEBLOCK
				x += 1

			case TK_RBRACE
				a->class = ASTCLASS_STRUCTINIT

			case else
				cError( "expected ',' (expression separator in struct initializer), or ';' (end of statement in scope block), or '}' (end of block)" + tkButFound( x ) )
			end select
		end select
	wend

	'' If no ',' or ';' was found and we don't know whether it's a struct
	'' initializer or a scope block, make an assumption...
	if( a->class = ASTCLASS_GROUP ) then
		a->class = ASTCLASS_STRUCTINIT
	end if

	cExpectMatch( TK_RBRACE, iif( a->class = ASTCLASS_STRUCTINIT, _
		@"to close struct initializer", _
		@"to close scope block" ) )

	function = a
end function

'' C expression parser based on precedence climbing
private function cExpression _
	( _
		byval level as integer, _
		byval macro as ASTNODE ptr _
	) as ASTNODE ptr

	'' Unary prefix operators
	var op = -1
	select case( tkGet( x ) )
	case TK_EXCL   : op = ASTCLASS_CLOGNOT   '' !
	case TK_TILDE  : op = ASTCLASS_NOT       '' ~
	case TK_MINUS  : op = ASTCLASS_NEGATE    '' -
	case TK_PLUS   : op = ASTCLASS_UNARYPLUS '' +
	case TK_AMP    : op = ASTCLASS_ADDROF    '' &
	case TK_STAR   : op = ASTCLASS_DEREF     '' *
	end select

	dim as ASTNODE ptr a
	if( op >= 0 ) then
		x += 1
		a = astNew( op, cExpression( cprecedence(op), macro ) )
	else
		'' Atoms
		select case( tkGet( x ) )

		''     '(' Expression ')'
		'' or: '(' DataType ')' Expression
		case TK_LPAREN
			'' '('
			x += 1

			var is_cast = hIsDataType( x, macro )

			'' Find the ')' and check the token behind it, in some cases
			'' we can tell that it probably isn't a cast.
			var closingparen = hFindClosingParen( x - 1 )
			select case( tkGet( closingparen + 1 ) )
			case TK_RPAREN, TK_EOF, TK_EOL
				is_cast = FALSE
			end select

			'' Something of the form '(id*)' or just in general a
			'' '*' in front of the closing ')'? It most likely is a pointer cast.
			is_cast or= (tkGet( closingparen - 1 ) = TK_STAR)

			if( is_cast ) then
				'' DataType ')'
				var t = hDataTypeInParens( DECL_CASTTYPE )

				'' Expression
				a = astNew( ASTCLASS_CAST, cExpression( 0, macro ) )

				assert( t->class = ASTCLASS_TYPE )
				astSetType( a, t->dtype, astClone( t->subtype ) )
				astDelete( t )
			else
				'' Expression
				a = cExpression( 0, macro )

				'' ')'
				cExpectMatch( TK_RPAREN, "to close '(...)' parenthesized expression" )

				if( a->class = ASTCLASS_ID ) then
					if( hIdentifierIsMacroParam( macro, a->text ) ) then
						a->attrib or= ASTATTRIB_PARENTHESIZEDMACROPARAM
					end if
				end if
			end if

		case TK_NUMBER
			a = hNumberLiteral( x, FALSE )
			x += 1

		case TK_STRING, TK_WSTRING, TK_HASH
			a = hStringLiteralSequence( )

		case TK_CHAR
			a = astNew( ASTCLASS_CHAR, tkGetText( x ) )
			astSetType( a, TYPE_ZSTRING, NULL )
			x += 1

		case TK_WCHAR
			a = astNew( ASTCLASS_CHAR, tkGetText( x ) )
			astSetType( a, TYPE_WSTRING, NULL )
			x += 1

		'' Identifier ['(' [CallArguments] ')']
		case TK_ID
			a = astNewID( tkSpellId( x ) )
			x += 1

			select case( tkGet( x ) )
			'' '('?
			case TK_LPAREN
				a->class = ASTCLASS_CALL
				x += 1

				'' [CallArguments]
				if( tkGet( x ) <> TK_RPAREN ) then
					'' Expression (',' Expression)*
					do
						astAppend( a, cExpression( 0, macro ) )

						'' ','?
					loop while( cMatch( TK_COMMA ) and parseok )
				end if

				'' ')'?
				cExpectMatch( TK_RPAREN, "to close call argument list" )

			'' '##'?
			case TK_HASHHASH
				var t = astNew( ASTCLASS_PPMERGE )
				astAppend( t, a )
				a = t
				x += 1

				'' Identifier ('##' Identifier)*
				do
					'' Identifier?
					if( tkGet( x ) = TK_ID ) then
						astAppend( a, astNewID( tkSpellId( x ) ) )
						x += 1
					else
						cError( "expected identifier as operand of '##' PP merge operator" + tkButFound( x ) )
					end if

					'' '##'?
				loop while( cMatch( TK_HASHHASH ) and parseok )

			end select

		'' Scope block: '{' (Expression ';')* '}'
		'' Initializer: '{' Expression (',' Expression)* '}'
		case TK_LBRACE
			a = hScopeBlockOrInitializer( macro )

		'' SIZEOF Expression
		'' SIZEOF '(' DataType ')'
		case KW_SIZEOF
			x += 1

			'' ('(' DataType)?
			if( (tkGet( x ) = TK_LPAREN) andalso _
			    hIsDataType( x + 1, macro ) ) then
				'' '('
				x += 1

				'' DataType ')'
				var t = hDataTypeInParens( DECL_SIZEOFTYPE )

				a = astNew( ASTCLASS_SIZEOFTYPE )

				assert( t->class = ASTCLASS_TYPE )
				astSetType( a, t->dtype, astClone( t->subtype ) )
				astDelete( t )
			else
				a = astNew( ASTCLASS_SIZEOF, cExpression( cprecedence(ASTCLASS_SIZEOF), macro ) )
			end if

		'' DEFINED ['('] Identifier [')']
		case KW_DEFINED
			x += 1

			'' '('
			var have_parens = cMatch( TK_LPAREN )

			'' Identifier
			dim as string id
			if( tkGet( x ) = TK_ID ) then
				id = *tkSpellId( x )
			else
				cError( "expected identifier" + tkButFound( x ) )
				id = hMakeDummyId( )
			end if
			a = astNewID( id )
			x += 1

			if( have_parens ) then
				'' ')'
				cExpectMatch( TK_RPAREN, "to finish defined(...) expression" )
			end if

			a = astNew( ASTCLASS_CDEFINED, a )

		case else
			cError( "expected expression" + tkButFound( x ) )
			a = astNewCONSTI( 0, TYPE_INTEGER )
		end select
	end if

	'' Infix operators
	while( parseok )
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
		case TK_LBRACKET : op = ASTCLASS_INDEX   '' [ (a[b])
		case TK_DOT      : op = ASTCLASS_MEMBER  '' .
		case TK_ARROW    : op = ASTCLASS_MEMBERDEREF '' ->
		case else        : exit while
		end select

		'' Higher/same level means process now (takes precedence),
		'' lower level means we're done and the parent call will
		'' continue. The first call will start with level 0.
		var oplevel = cprecedence(op)
		if( oplevel < level ) then
			exit while
		end if
		'' Left associative?
		if( op <> ASTCLASS_IIF ) then
			oplevel += 1
		end if

		'' operator
		x += 1

		'' rhs
		var b = cExpression( oplevel, macro )

		'' Handle ?: special case
		if( op = ASTCLASS_IIF ) then
			'' ':'
			cExpectMatch( TK_COLON, "for a?b:c iif operator" )

			var c = cExpression( oplevel, macro )

			a = astNewIIF( a, b, c )
		else
			'' Handle [] special case
			if( op = ASTCLASS_INDEX ) then
				'' ']'
				cExpectMatch( TK_RBRACKET, "for [] indexing operator" )
			end if

			a = astNew( op, a, b )
		end if
	wend

	function = a
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private sub hCdeclAttribute( byref gccattribs as integer )
	if( gccattribs and ASTATTRIB_STDCALL ) then
		cError( "cdecl attribute specified together with stdcall" )
	end if
	gccattribs or= ASTATTRIB_CDECL
	x += 1
end sub

private sub hStdcallAttribute( byref gccattribs as integer )
	if( gccattribs and ASTATTRIB_CDECL ) then
		cError( "stdcall attribute specified together with cdecl" )
	end if
	gccattribs or= ASTATTRIB_STDCALL
	x += 1
end sub

private sub cGccAttribute( byref gccattribs as integer )
	if( tkGet( x ) < TK_ID ) then
		cError( "expected attribute identifier inside __attribute__((...))" )
		exit sub
	end if

	select case( *tkSpellId( x ) )
	case "warn_unused_result", "__warn_unused_result__", _
	     "noreturn", "__noreturn__", _
	     "malloc", "__malloc__"
		'' Ignore, not interesting for FB bindings
		x += 1

	case "deprecated", "__deprecated__"
		x += 1

		'' optional message:
		'' ['(' StringLiteral ')']
		if( tkGet( x ) = TK_LPAREN ) then
			x += 1

			cExpectMatch( TK_STRING, "message for deprecated attribute" )

			cExpectMatch( TK_RPAREN, "behind deprecated message" )
		end if

	case "cdecl", "__cdecl__"
		hCdeclAttribute( gccattribs )

	case "stdcall", "__stdcall__"
		hStdcallAttribute( gccattribs )

	case "packed", "__packed__"
		gccattribs or= ASTATTRIB_PACKED
		x += 1

	case else
		cError( "unknown attribute '" + *tkSpellId( x ) + "'" )
	end select
end sub

private sub cGccAttributeList( byref gccattribs as integer )
	while( parseok )
		select case( tkGet( x ) )
		case KW_VOLATILE
			x += 1

		'' __cdecl
		case KW___CDECL
			hCdeclAttribute( gccattribs )

		'' __stdcall
		case KW___STDCALL
			hStdcallAttribute( gccattribs )

		'' __attribute__((...)):
		'' __ATTRIBUTE__ '((' Attribute (',' Attribute)* '))'
		case KW___ATTRIBUTE__
			x += 1

			'' '('?
			cExpectMatch( TK_LPAREN, "as 1st '(' in '__attribute__((...))'" )

			'' '('?
			cExpectMatch( TK_LPAREN, "as 2nd '(' in '__attribute__((...))'" )

			'' Attribute (',' Attribute)*
			do
				'' ')'?
				if( tkGet( x ) = TK_RPAREN ) then
					exit do
				end if

				'' Attribute
				cGccAttribute( gccattribs )

				'' ','?
			loop while( cMatch( TK_COMMA ) and parseok )

			'' ')'?
			cExpectMatch( TK_RPAREN, "as 1st ')' in '__attribute__((...))'" )

			'' ')'?
			cExpectMatch( TK_RPAREN, "as 2nd ')' in '__attribute__((...))'" )

		case else
			exit while
		end select
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Enum constant: Identifier ['=' Expression] (',' | '}')
private function cEnumConst( ) as ASTNODE ptr
	var t = astNew( ASTCLASS_CONST )
	t->attrib or= ASTATTRIB_ENUMCONST

	'' Identifier
	if( tkGet( x ) = TK_ID ) then
		astSetText( t, tkSpellId( x ) )
	else
		cError( "expected identifier for an enum constant" + tkButFound( x ) )
	end if
	x += 1

	'' '='?
	if( cMatch( TK_EQ ) ) then
		'' Expression
		t->expr = cExpression( 0, NULL )
	end if

	'' (',' | '}')
	select case( tkGet( x ) )
	case TK_COMMA
		x += 1

	case TK_RBRACE

	case else
		cError( "expected ',' or '}' behind enum constant" + tkButFound( x ) )
	end select

	function = t
end function

'' {STRUCT|UNION|ENUM} [Identifier] '{' StructBody|EnumBody '}'
'' {STRUCT|UNION|ENUM} Identifier
private function cStruct( ) as ASTNODE ptr
	'' {STRUCT|UNION|ENUM}
	dim as integer astclass
	select case( tkGet( x ) )
	case KW_UNION
		astclass = ASTCLASS_UNION
	case KW_ENUM
		astclass = ASTCLASS_ENUM
	case else
		assert( tkGet( x ) = KW_STRUCT )
		astclass = ASTCLASS_STRUCT
	end select
	x += 1

	var struct = astNew( astclass )

	'' __attribute__((...))
	cGccAttributeList( struct->attrib )

	'' [Identifier]
	if( tkGet( x ) = TK_ID ) then
		astSetText( struct, tkSpellId( x ) )
		x += 1
	end if

	'' '{'?
	if( tkGet( x ) = TK_LBRACE ) then
		x += 1

		astAppend( struct, _
			cBody( iif( astclass = ASTCLASS_ENUM, _
				BODY_ENUM, BODY_STRUCT ) ) )

		'' '}'
		cExpectMatch( TK_RBRACE, "to close " + astDumpPrettyDecl( struct ) + " block" )

		'' __attribute__((...))
		cGccAttributeList( struct->attrib )
	else
		if( struct->text = NULL ) then
			cError( "expected '{' or tag name behind " + astDumpPrettyDecl( struct ) + tkButFound( x ) )
			astSetText( struct, hMakeDummyId( ) )
		end if

		'' It's just a tag name, not a body
		struct->class = ASTCLASS_TAGID
	end if

	function = struct
end function

private function cTypedef( ) as ASTNODE ptr
	'' TYPEDEF
	var comment = tkCollectComments( x, x )
	x += 1

	function = cDeclaration( DECL_TYPEDEF, 0, comment )
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function cDefine( ) as ASTNODE ptr
	x += 1

	'' Identifier ['(' ParameterList ')']
	var macro = hDefineHead( x )

	'' Body
	var xbody = x
	assert( macro->expr = NULL )

	'' Non-empty?
	if( tkGet( x ) <> TK_EOL ) then
		'' Try to parse it as expression
		macro->expr = cExpression( 0, macro )

		'' Didn't reach EOL? Then the beginning of the macro body could
		'' be parsed as expression, but not the rest.
		if( tkGet( x ) <> TK_EOL ) then
			cError( "failed to parse full #define body as expression" )
			x = hSkipToEol( x )
		end if
	end if

	'' Eol
	assert( tkGet( x ) = TK_EOL )
	x += 1

	function = macro
end function

private function cInclude( ) as ASTNODE ptr
	x += 1

	var ppinclude = astNew( ASTCLASS_PPINCLUDE )

	'' "..."
	assert( tkGet( x ) = TK_STRING )
	astSetText( ppinclude, tkGetText( x ) )
	x += 1

	assert( tkGet( x ) = TK_EOL )
	x += 1

	function = ppinclude
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

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
''    struct { ...fields... } a;
''    ^^^^^^^^^^^^^^^^^^^^^^^
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
private sub cBaseType _
	( _
		byref dtype as integer, _
		byref subtype as ASTNODE ptr, _
		byref gccattribs as integer, _
		byval decl as integer _
	)

	dtype = TYPE_NONE
	subtype = NULL

	var signedmods = 0
	var unsignedmods = 0
	var constmods = 0
	var shortmods = 0
	var longmods = 0

	''
	'' 1. Parse base type and all modifiers, and count them
	''

	while( parseok )
		'' __ATTRIBUTE__((...))
		cGccAttributeList( gccattribs )

		select case( tkGet( x ) )
		case KW_SIGNED
			if( unsignedmods > 0 ) then
				cError( "mixed SIGNED with previous UNSIGNED modifier" )
			end if
			signedmods += 1

		case KW_UNSIGNED
			if( signedmods > 0 ) then
				cError( "mixed UNSIGNED with previous SIGNED modifier" )
			end if
			unsignedmods += 1

		case KW_CONST
			constmods += 1

		case KW_SHORT
			if( longmods > 0 ) then
				cError( "mixed SHORT with previous LONG modifier" )
			end if
			shortmods += 1
			if( shortmods > 1 ) then
				cError( "more than 1 SHORT modifier" )
			end if

		case KW_LONG
			if( shortmods > 0 ) then
				cError( "mixed LONG with previous SHORT modifier" )
			end if
			longmods += 1
			if( longmods > 2 ) then
				cError( "more than 2 LONG modifiers" )
			end if

		case else
			'' Only one base type is allowed
			if( dtype <> TYPE_NONE ) then
				exit while
			end if

			select case( tkGet( x ) )
			case KW_ENUM, KW_STRUCT, KW_UNION
				dtype = TYPE_UDT
				subtype = cStruct( )
				x -= 1

			case TK_ID
				''
				'' An identifier can be part of the data type if
				'' it's a typedef (the code here doesn't check
				'' for that but just assumes it is).
				''
				'' Modifiers such as CONST can be combined with
				'' such typedefs, others like UNSIGNED can't.
				'' For example:
				''
				''    typedef int myint;
				''
				''    const myint;      // doesn't declare anything
				''    const myint foo;  // CONST combined with myint typedef
				''
				''    unsigned foo;       // ok, foo = unsigned int variable
				''    unsigned myint foo; // invalid code, myint = variable name, foo = unexpected token
				''

				'' Already saw modifiers that themselves are enough to form the type?
				if( signedmods or unsignedmods or longmods or shortmods ) then
					'' Then don't treat this id as the type
					exit while
				end if

				'' Treat the id as the type
				var id = tkSpellId( x )
				dtype = hIdentifyCommonTypedef( id )
				if( dtype = TYPE_NONE ) then
					dtype = TYPE_UDT
					subtype = astNewID( id )
				end if

			case KW_VOID   : dtype = TYPE_ANY
			case KW_FLOAT  : dtype = TYPE_SINGLE
			case KW_DOUBLE : dtype = TYPE_DOUBLE
			case KW_CHAR   : dtype = TYPE_ZSTRING
			case KW_INT    : dtype = TYPE_LONG

			case else
				exit while
			end select
		end select

		x += 1
	wend

	'' Some details can only be decided after parsing the whole thing,
	'' because for example "unsigned int" and "int unsigned" both are allowed.
	select case( dtype )
	case TYPE_DOUBLE
		if( longmods = 1 ) then
			dtype = TYPE_CLONGDOUBLE
		end if

	case TYPE_ZSTRING
		'' SIGNED|UNSIGNED CHAR becomes BYTE|UBYTE,
		'' but plain CHAR probably means ZSTRING
		if( signedmods > 0 ) then
			dtype = TYPE_BYTE
		elseif( unsignedmods > 0 ) then
			dtype = TYPE_UBYTE
		end if

	case TYPE_LONG, TYPE_NONE
		'' Base type is "int" (either explicitly given, or implied
		'' because no other base type was given). Any modifiers are
		'' just added on top of that.
		if( shortmods = 1 ) then
			dtype = iif( unsignedmods > 0, TYPE_USHORT, TYPE_SHORT )
		elseif( longmods = 1 ) then
			dtype = iif( unsignedmods > 0, TYPE_CULONG, TYPE_CLONG )
		elseif( longmods = 2 ) then
			dtype = iif( unsignedmods > 0, TYPE_ULONGINT, TYPE_LONGINT )
		elseif( dtype = TYPE_LONG ) then
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
			var message = "expected a data type "
			select case( decl )
			case DECL_CASTTYPE
				message += "in this '(...)' type cast"
			case DECL_SIZEOFTYPE
				message += "as operand in this 'sizeof(...)'"
			case DECL_PARAM
				message += "starting a parameter declaration"
			case else
				message += "starting a declaration"
			end select
			cError( message + tkButFound( x ) )
		end if
	end select

	select case( dtype )
	case TYPE_ANY, TYPE_SINGLE, TYPE_DOUBLE, TYPE_UDT
		if( signedmods or unsignedmods or shortmods or longmods ) then
			cError( "SIGNED|UNSIGNED|SHORT|LONG modifiers used with void/float/double/typedef/UDT" )
		end if
	case TYPE_ZSTRING, TYPE_BYTE, TYPE_UBYTE
		if( shortmods or longmods ) then
			cError( "SHORT|LONG modifiers used with CHAR type" )
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

	'' __ATTRIBUTE__((...))
	cGccAttributeList( gccattribs )
end sub

'' ParamDeclList = ParamDecl (',' ParamDecl)*
'' ParamDecl = '...' | Declaration{Param}
private function cParamDeclList( ) as ASTNODE ptr
	var group = astNewGROUP( )

	do
		dim as ASTNODE ptr t

		'' '...'?
		if( tkGet( x ) = TK_ELLIPSIS ) then
			t = astNew( ASTCLASS_PARAM )
			astAddComment( t, tkCollectComments( x, x ) )
			x += 1
		else
			t = cDeclaration( DECL_PARAM, 0, "" )
		end if

		astAppend( group, t )

		'' ','?
	loop while( cMatch( TK_COMMA ) and parseok )

	function = group
end function

private function hCanHaveInitializer( byval n as ASTNODE ptr ) as integer
	select case( n->class )
	case ASTCLASS_PARAM
		function = TRUE
	case ASTCLASS_VAR
		function = ((n->attrib and ASTATTRIB_EXTERN) = 0)
	end select
end function

''
'' Declarator =
''    GccAttributeList
''    ('*' [CONST|GccAttributeList])*
''    { [Identifier] | '(' Declarator ')' }
''    { '(' ParamList ')' | ('[' ArrayElements ']')* }
''    [ '=' Initializer ]
''    GccAttributeList
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
'' Example 1:
''
''         int (*f)(int a);
'' depth 1:     ^^
''    innerprocptrdtype = TYPE_PROC (unused)
''    procptrdtype      = typeAddrOf( TYPE_PROC )
''    innernode = NULL
''    node = NULL
''    t = VAR( f as int ptr )    (f as int ptr for now, in case it's just "int (*f);")
''
''         int (*f)(int a);
'' depth 0:    ^  ^^^^^^^^
''    innerprocptrdtype = typeAddrOf( TYPE_PROC )      (passed up from depth 1)
''    procptrdtype      = TYPE_PROC                    (unused)
''    innernode = VAR( f as int ptr )                  (passed up from depth 1)
''    node      = PROC( function( a as int ) as int )  (new function pointer subtype)
''    t = VAR( f as function( a as int ) as int )      (adjusted AST: f turned into function pointer)
''
'' Example 2:
''
''         int (*(*f)(int a))(int b);
'' depth 2:       ^^
''    innerprocptrdtype = TYPE_PROC                    (unused)
''    procptrdtype      = typeAddrOf( TYPE_PROC )
''    innernode = NULL
''    node      = NULL
''    t = VAR( f as int ptr ptr )    (f as int ptr ptr for now, in case it's just "int (*(*f));")
''
''         int (*(*f)(int a))(int b);
'' depth 1:     ^^  ^^^^^^^^
''    innerprocptrdtype = typeAddrOf( TYPE_PROC )      (passed up from depth 2)
''    procptrdtype      = typeAddrOf( TYPE_PROC )
''    innernode = VAR( f as int ptr ptr )              (passed up from depth 2)
''    node      = PROC( function( a as int ) as int ptr )  (new function pointer subtype,
''                                                     result = int ptr for now, in case it's
''                                                     just "int (*(*f)(int a));")
''    t = VAR( f as function( a as int ) as int ptr )  (adjusted AST: f turned into function pointer)
''
''         int (*(*f)(int a))(int b);
'' depth 0:    ^            ^^^^^^^^
''    innerprocptrdtype = typeAddrOf( TYPE_PROC )          (passed up from depth 1)
''    procptrdtype      = TYPE_PROC (unused)
''    innernode = PROC( function( a as int ) as int ptr )  (passed up from depth 1)
''    node      = PROC( function( b as int ) as int )      (new function pointer subtype)
''    t = VAR( f as function( a as int ) as function( b as int ) as int )
''                               (adjusted AST: f's subtype (innernode) turned into function pointer,
''                               i.e. the f function pointer now has a function pointer as its function
''                               result type, instead of "int ptr")
''
''
'' __attribute__((...)) parsing stuff:
''
'' These are all the same:
''    __attribute__((stdcall)) void (*p)(int a);
''    void (*p)(int a) __attribute__((stdcall));
''    void (__attribute__((stdcall)) *p)(int a);
''    void (* __attribute__((stdcall)) p)(int a);
''    extern p as sub stdcall( byval a as long )
'' i.e. the stdcall attribute goes to the procptr subtype, no matter whether
'' it appears in the toplevel declarator (the proc) or the nested declarator
'' (the pointer var).
''
'' Here the stdcall goes to the proc that's being declared, not to its result type:
''    __attribute__((stdcall)) void (*f(int a))(int b);
''    void (*f(int a))(int b) __attribute__((stdcall));
''    declare function f stdcall( byval a as long ) as sub cdecl( byval b as long )
''
'' Here the stdcall goes to the proc's result procptr subtype, not the proc itself:
''    void (__attribute__((stdcall)) *f(int a))(int b);
''    declare function f cdecl( byval a as long ) as sub stdcall( byval b as long )
''
'' This proc returns a pointer to the above one:
''    void (__attribute__((stdcall)) *(*f(int a))(int b))(int c);
''                                      ^^^^^^^^
''                                    ^^        ^^^^^^^^
''          ^^^^^^^^^^^^^^^^^^^^^^^^^^                  ^^^^^^^^
''    ^^^^^^                                                    ^
''    declare function f cdecl  ( byval a as long ) as _
''            function   cdecl  ( byval b as long ) as _
''            sub        stdcall( byval c as long )
''
'' Here the stdcall still goes to the proc that's being declared:
''    __attribute__((stdcall)) void (*(*f(int a))(int b))(int c);
''    declare function f stdcall( byval a as long ) as _
''            function   cdecl  ( byval b as long ) as _
''            sub        cdecl  ( byval c as long )
''
'' Here the stdcall still goes to the proc's result type:
''    void (*(__attribute__((stdcall)) *f(int a))(int b))(int c);
''    declare function f cdecl  ( byval a as long ) as _
''            function   stdcall( byval b as long ) as _
''            sub        cdecl  ( byval c as long )
''
'' I.e. attributes from the base type go to the inner most declarator (which
'' ends up defining the toplevel object, a proc or var), while attributes from
'' declarators go to the nodes for those declarators.
''
'' More about function pointers because they seem to be more complicated, these
'' are all ok:
''
''    __attribute__((stdcall)) void (*f(int a))(int b);         // proc
''    declare function f stdcall( byval a as long ) as sub cdecl( byval b as long )
''
''    extern __attribute__((stdcall)) void (*(*p)(int a))(int b) = f;  // ptr to it
''    extern void (*(__attribute__((stdcall)) *p)(int a))(int b) = f;  // same thing, apparently
''    extern p as function stdcall( byval a as long ) as sub cdecl( byval b as long )
''
'' I.e. we see again that for procptr vars, the attributes from toplevel and
'' inner-most declarators have the same effect - they go to the procptr subtype
'' in both cases.
''
''    void (__attribute__((stdcall)) *f(int a))(int b);         // different proc
''    declare function f cdecl( byval a as long ) as sub stdcall( byval b as long )
''
''    extern void (__attribute__((stdcall)) *(*p)(int a))(int b) = f;  // ptr to it
''    extern p as function cdecl( byval a as long ) as sub stdcall( byval b as long )
''
'' Here the stdcall is in the middle declarator and goes to the proc type
'' corresponding to it, the procptr's result type.
''
private function cDeclarator _
	( _
		byval nestlevel as integer, _
		byval decl as integer, _
		byval outerdtype as integer, _
		byval basesubtype as ASTNODE ptr, _
		byval basegccattribs as integer, _
		byref node as ASTNODE ptr, _
		byref procptrdtype as integer, _
		byref gccattribs as integer _
	) as ASTNODE ptr

	var dtype = outerdtype
	var innerprocptrdtype = TYPE_PROC
	var innergccattribs = 0
	procptrdtype = TYPE_PROC
	gccattribs = 0

	'' __ATTRIBUTE__((...))
	''
	'' Note: __attribute__'s behind the base type are handled by cBaseType()
	'' already because they apply to the whole declaration:
	''    int __attribute__((stdcall)) f1(void), f2(void);
	'' both should be stdcall.
	''
	'' But this is still here, to handle __attribute__'s appearing in
	'' nested declarators:
	''    int (__attribute__((stdcall)) f1)(void);
	'' or at the front of follow-up declarators in a declaration:
	''    int f1(void), __attribute__((stdcall)) f2(void);
	''
	cGccAttributeList( gccattribs )

	'' Pointers: ('*')*
	while( cMatch( TK_STAR ) and parseok )
		procptrdtype = typeAddrOf( procptrdtype )
		dtype = typeAddrOf( dtype )

		'' (CONST|RESTRICT|__ATTRIBUTE__((...)))*
		while( parseok )
			'' __ATTRIBUTE__((...))
			cGccAttributeList( gccattribs )

			select case( tkGet( x ) )
			case KW_CONST
				procptrdtype = typeSetIsConst( procptrdtype )
				dtype = typeSetIsConst( dtype )
				x += 1

			case KW_RESTRICT, KW___RESTRICT, KW___RESTRICT__
				'' The restrict keyword is not interesting for FB bindings, just ignore
				x += 1

			case else
				exit while
			end select
		wend
	wend

	''    '(' Declarator ')'    |    [Identifier]
	dim as ASTNODE ptr t, innernode

	'' '('?
	if( cMatch( TK_LPAREN ) ) then
		t = cDeclarator( nestlevel + 1, decl, dtype, basesubtype, 0, innernode, innerprocptrdtype, innergccattribs )

		'' ')'
		cExpectMatch( TK_RPAREN, "for '(...)' parenthesized declarator" )
	else
		'' [Identifier]
		'' An identifier must exist, except for parameters/types, and
		'' in fact for types there mustn't be an id.
		dim as string id
		select case( decl )
		case DECL_CASTTYPE, DECL_SIZEOFTYPE
		case else
			if( tkGet( x ) = TK_ID ) then
				id = *tkSpellId( x )
				x += 1
			else
				if( decl <> DECL_PARAM ) then
					cError( "expected identifier for the symbol declared in this declaration" + tkButFound( x ) )
					id = hMakeDummyId( )
				end if
			end if
		end select

		t = astNew( decl_to_astclass(decl), id )
		select case( decl )
		case DECL_EXTERNVAR
			t->attrib or= ASTATTRIB_EXTERN
		case DECL_GLOBALSTATICVAR
			t->attrib or= ASTATTRIB_STATIC
		case DECL_LOCALVAR
			t->attrib or= ASTATTRIB_LOCAL
		case DECL_LOCALSTATICVAR
			t->attrib or= ASTATTRIB_LOCAL or ASTATTRIB_STATIC
		case DECL_TYPEDEF
			hAddTypedef( id )
		end select
		astSetType( t, dtype, basesubtype )
	end if

	node = t

	select case( tkGet( x ) )
	'' ('[' [ArrayElements] ']')*
	case TK_LBRACKET
		'' Can't allow arrays on everything - currently, it's only
		'' handled for vars/fields/params/typedefs
		select case( decl )
		case DECL_CASTTYPE, DECL_SIZEOFTYPE
			cError( "TODO: arrays not supported here yet" )
		end select

		assert( node->array = NULL )
		node->array = astNew( ASTCLASS_ARRAY )

		'' For each array dimension...
		do
			'' '['
			x += 1

			var d = astNew( ASTCLASS_DIMENSION )

			'' Just '[]' (empty dimension) is allowed for parameters
			if( (tkGet( x ) <> TK_RBRACKET) or (decl <> DECL_PARAM) ) then
				d->expr = cExpression( 0, NULL )
			end if

			astAppend( node->array, d )

			'' ']'
			cExpectMatch( TK_RBRACKET, "to close this array dimension declaration" )

			'' '['? (next dimension)
		loop while( (tkGet( x ) = TK_LBRACKET) and parseok )

	'' ':' <bits>
	case TK_COLON
		if( decl <> DECL_FIELD ) then
			cError( "bitfields not supported here" )
		end if
		x += 1

		node->bits = cExpression( 0, NULL )

	'' '(' ParamList ')'
	case TK_LPAREN
		x += 1

		'' Parameters turn a vardecl/fielddecl into a procdecl,
		'' unless they're for a procptr type.
		if( innerprocptrdtype <> TYPE_PROC ) then
			'' There were '()'s above and the recursive
			'' cDeclarator() call found pointers/CONSTs,
			'' these parameters are for a function pointer.
			''
			'' Whichever object should become the function pointer,
			'' its dtype/subtype must be adjusted accordingly.
			'' For the subtype, a new PROC node is created, which
			'' will hold the parameters etc. found at this level.

			'' New PROC node for the function pointer's subtype
			node = astNew( ASTCLASS_PROC )
			astSetType( node, dtype, basesubtype )

			'' Turn the object into a function pointer
			astDelete( innernode->subtype )
			innernode->dtype = innerprocptrdtype
			innernode->subtype = node

			innerprocptrdtype = TYPE_PROC

		'' Typedefs with parameters aren't turned into procs, but must
		'' be given a PROC subtype, similar to procptrs.
		elseif( t->class = ASTCLASS_TYPEDEF ) then
			node = astNew( ASTCLASS_PROC )
			astSetType( node, dtype, basesubtype )

			astDelete( t->subtype )
			t->dtype = TYPE_PROC
			t->subtype = node
		else
			'' A plain symbol, not a pointer, becomes a function
			select case( t->class )
			case ASTCLASS_VAR, ASTCLASS_FIELD
				t->class = ASTCLASS_PROC
			end select
		end if

		'' Just '(void)'?
		if( (tkGet( x ) = KW_VOID) and (tkGet( x + 1 ) = TK_RPAREN) ) then
			'' VOID
			x += 1
		'' Not just '()'?
		elseif( tkGet( x ) <> TK_RPAREN ) then
			astAppend( node, cParamDeclList( ) )
		end if

		'' ')'
		cExpectMatch( TK_RPAREN, "to close parameter list in function declaration" )
	end select

	'' __ATTRIBUTE__((...))
	var endgccattribs = 0
	cGccAttributeList( endgccattribs )

	if( hCanHaveInitializer( t ) ) then
		'' ['=' Initializer]
		if( cMatch( TK_EQ ) ) then
			assert( t->expr = NULL )
			t->expr = cExpression( 0, NULL )
		end if
	end if

	if( nestlevel > 0 ) then
		'' __attribute__'s from this level should always be passed up
		gccattribs or= endgccattribs

		'' Pass innerprocptrdtype and innergccattribs up again if they
		'' weren't used up on this level
		if( innerprocptrdtype <> TYPE_PROC ) then
			gccattribs or= innergccattribs
			procptrdtype = typeMultAddrOf( procptrdtype, typeGetPtrCount( innerprocptrdtype ) ) or _
								typeGetConst( innerprocptrdtype )
		else
			node->attrib or= innergccattribs
		end if
	else
		'' At toplevel nothing can be passed up, everything must be assigned.
		'' __attribute__'s from the base type go to the toplevel symbol
		'' that's being declared, for example a function, except if it's
		'' a function pointer variable, then the __attribute__'s go to
		'' the procptr subtype, not the variable.

		basegccattribs or= gccattribs or endgccattribs

		if( (typeGetDt( t->dtype ) = TYPE_PROC) and (t->class <> ASTCLASS_PROC) ) then
			assert( t->subtype->class = ASTCLASS_PROC )
			t->subtype->attrib or= basegccattribs
		else
			t->attrib or= basegccattribs
		end if

		node->attrib or= innergccattribs
	end if

	function = t
end function

''
'' Generic 'type *a, **b;' parsing, used for vars/fields/protos/params/typedefs
'' ("multiple declaration" syntax)
''    int i;
''    int a, b, c;
''    int *a, ***b, c;
''    int f(void);
''    int (*procptr)(void);
''    struct UDT { int a; };  (special case for BaseType only)
''
'' Declaration = GccAttributeList BaseType Declarator (',' Declarator)* [';']
''
private function cDeclaration _
	( _
		byval decl as integer, _
		byval gccattribs as integer, _
		byref comment as string _
	) as ASTNODE ptr

	var begin = x

	'' BaseType
	dim as integer dtype
	dim as ASTNODE ptr subtype
	cBaseType( dtype, subtype, gccattribs, decl )
	comment += tkCollectComments( begin, x - 1 )

	'' Special case for standalone struct/union/enum declarations (no CONST bits):
	if( dtype = TYPE_UDT ) then
		'' Tag declaration with body?
		''    STRUCT|UNION|ENUM Identifier '{' ... '}' ';'
		select case( subtype->class )
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			'' ';'?
			if( tkGet( x ) = TK_SEMI ) then
				x += 1
				return subtype
			end if

		'' Useless tag declaration?
		''    STRUCT|UNION|ENUM Identifier ';'
		case ASTCLASS_TAGID
			'' ';'?
			if( tkGet( x ) = TK_SEMI ) then
				x += 1
				'' Ignore & treat as no-op
				astDelete( subtype )
				return astNewGROUP( )
			end if
		end select
	end if

	var result = astNewGROUP( )

	'' Special case for struct/union/enum bodies used as basedtype:
	'' Make the struct/union/enum body a separate declaration (as needed
	'' by FB) and make the basedtype reference it by name.
	if( typeGetDtAndPtr( dtype ) = TYPE_UDT ) then
		select case( subtype->class )
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			'' Make up an id for anonymous structs, for use as the base type
			'' of following declarators. If it turns out to be unnecessary,
			'' we can still solve it out later.
			if( subtype->text = NULL ) then
				astSetText( subtype, hMakeDummyId( ) )
			end if

			astAppend( result, subtype )
			subtype = astNew( ASTCLASS_TAGID, subtype->text )
		end select
	end if

	var require_semi = TRUE

	'' ... (',' ...)*
	var declarator_count = 0
	do
		declarator_count += 1
		begin = x

		astAppend( result, cDeclarator( 0, decl, dtype, subtype, gccattribs, NULL, 0, 0 ) )
		var t = result->tail

		'' The first declaration takes the comments from the base type
		if( len( comment ) > 0 ) then
			astAddComment( t, comment )
			comment = ""
		end if

		'' Every declaration takes the comments on its declarator tokens
		astAddComment( t, tkCollectComments( begin, x - 1 ) )

		'' Parameters/types can't have commas and more identifiers,
		'' and don't need with ';' either.
		select case( decl )
		case DECL_PARAM, DECL_CASTTYPE, DECL_SIZEOFTYPE
			require_semi = FALSE
			exit do
		end select

		'' '{', procedure body?
		if( (t->class = ASTCLASS_PROC) and (tkGet( x ) = TK_LBRACE) ) then
			'' A procedure with body must be the first and only
			'' declarator in the declaration.
			if( declarator_count = 1 ) then
				assert( t->expr = NULL )
				t->expr = cScope( )
				require_semi = FALSE
				exit do
			end if
		end if

		'' ','?
	loop while( cMatch( TK_COMMA ) and parseok )

	if( require_semi ) then
		'' ';'
		cExpectMatch( TK_SEMI, "to finish this declaration" )
	end if

	astDelete( subtype )
	function = result
end function

'' Variable/procedure declarations
''    GccAttributeList [EXTERN|STATIC] Declaration
private function cVarOrProcDecl( byval is_local as integer ) as ASTNODE ptr
	var begin = x

	'' __ATTRIBUTE__((...))
	var gccattribs = 0
	cGccAttributeList( gccattribs )

	'' [EXTERN|STATIC]
	var decl = iif( is_local, DECL_LOCALVAR, DECL_GLOBALVAR )
	select case( tkGet( x ) )
	case KW_EXTERN
		decl = DECL_EXTERNVAR
		x += 1
	case KW_STATIC
		decl = iif( is_local, DECL_LOCALSTATICVAR, DECL_GLOBALSTATICVAR )
		x += 1
	end select

	var comment = tkCollectComments( begin, x - 1 )

	'' Declaration
	function = cDeclaration( decl, gccattribs, comment )
end function

'' Expression statement: Assignments, function calls, i++, etc.
private function cExpressionStatement( ) as ASTNODE ptr
	function = cExpression( 0, NULL )

	'' ';'?
	cExpectMatch( TK_SEMI, "(end of expression statement)" )
end function

'' '{ ... }' statement block
private function cScope( ) as ASTNODE ptr
	'' '{'
	assert( tkGet( x ) = TK_LBRACE )
	x += 1

	function = cBody( BODY_SCOPE )

	'' '}'
	cExpectMatch( TK_RBRACE, "to close compound statement" )
end function

private function cConstruct( byval body as integer ) as ASTNODE ptr
	'' '#'?
	if( (tkGet( x ) = TK_HASH) and (tkGetExpansionLevel( x ) = 0) ) then
		var begin = x
		x += 1

		dim directive as ASTNODE ptr
		if( tkGet( x ) = TK_ID ) then
			select case( *tkSpellId( x ) )
			case "define"
				directive = cDefine( )
			case "include"
				directive = cInclude( )
			end select
		end if

		if( directive = NULL ) then
			cError( "unknown CPP directive" )
			directive = astNew( ASTCLASS_PPDEFINE )
		end if

		astAddComment( directive, tkCollectComments( begin, x - 1 ) )
		return directive
	end if

	if( tkGet( x ) = TK_DIVIDER ) then
		var t = astNew( ASTCLASS_DIVIDER, tkGetText( x ) )
		astAddComment( t, tkCollectComments( x, x ) )
		x += 1
		return t
	end if

	if( body = BODY_ENUM ) then
		return cEnumConst( )
	end if

	select case( tkGet( x ) )
	case KW_TYPEDEF
		return cTypedef( )
	case TK_SEMI
		'' Ignore standalone ';'
		x += 1
		return astNewGROUP( )
	end select

	select case( body )
	case BODY_STRUCT
		'' Field declaration
		function = cDeclaration( DECL_FIELD, 0, "" )

	case BODY_SCOPE
		'' Disambiguate: local declaration vs. expression
		'' If it starts with a data type, __attribute__, or 'static',
		'' then it must be a declaration.
		if( hIsDataType( x, NULL ) or (tkGet( x ) = KW_STATIC) ) then
			function = cVarOrProcDecl( TRUE )
		else
			function = cExpressionStatement( )
		end if

	case else
		function = cVarOrProcDecl( FALSE )
	end select
end function

private function cBody( byval body as integer ) as ASTNODE ptr
	var group = astNewGROUP( )

	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do
		'' '}'
		case TK_RBRACE
			if( body <> BODY_TOPLEVEL ) then
				exit do
			end if
		end select

		var begin = x
		var t = cConstruct( body )

		if( parseok = FALSE ) then
			astDelete( t )

			'' Skip current construct and preserve its tokens in
			'' an UNKNOWN node
			x = hSkipConstruct( begin )
			t = astNew( ASTCLASS_UNKNOWN, tkSpell( begin, x - 1 ) )
			astSetComment( t, errmsg )

			errmsg = ""
			parseok = TRUE
		end if

		astAppend( group, t )
	loop

	function = group
end function

function cFile( ) as ASTNODE ptr
	hashInit( @typedefs, 4, TRUE )
	x = 0
	parseok = TRUE
	function = cBody( BODY_TOPLEVEL )
	hashEnd( @typedefs )
end function

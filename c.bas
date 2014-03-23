''
'' C parsing
''
'' cFile() parses the content of the tk buffer without modifying it,
'' and returns the resulting AST.
''

#include once "fbfrog.bi"

enum
	DECL_VAR = 0
	DECL_EXTERNVAR
	DECL_STATICVAR
	DECL_FIELD
	DECL_PARAM
	DECL_TYPEDEF
	DECL_CASTTYPE
	DECL_SIZEOFTYPE
	DECL__COUNT
end enum

#if 0
dim shared as zstring ptr decl2str(0 to DECL__COUNT-1) => _
{ _
	@"variable declaration", _
	@"extern variable declaration", _
	@"static variable declaration", _
	@"field declaration", _
	@"parameter declaration", _
	@"typedef declaration", _
	@"type cast expression", _
	@"sizeof() type argument" _
}
#endif

declare function cExpression _
	( _
		byref x as integer, _
		byval level as integer, _
		byval macro as ASTNODE ptr _
	) as ASTNODE ptr
declare function cIdList _
	( _
		byref x as integer, _
		byval decl as integer, _
		byval basedtype as integer, _
		byval basesubtype as ASTNODE ptr, _
		byval gccattribs as integer, _
		byref comment as string _
	) as ASTNODE ptr
declare function cMultDecl _
	( _
		byref x as integer, _
		byval decl as integer, _
		byval gccattribs as integer, _
		byref comment as string _
	) as ASTNODE ptr
declare function cToplevel _
	( _
		byref x as integer, _
		byval body as integer _
	) as ASTNODE ptr

enum
	BODY_TOPLEVEL = 0
	BODY_STRUCT
	BODY_ENUM
end enum

namespace file
	dim shared typedefs as THASH
end namespace

private sub hAddTypedef( byval id as zstring ptr )
	if( frog.verbose ) then
		print "registering typedef '" + *id + "'"
	end if
	hashAddOverwrite( @file.typedefs, id, NULL )
end sub

private function hIsTypedef( byval id as zstring ptr ) as integer
	function = (hashLookup( @file.typedefs, id, hashHash( id ) )->s <> NULL)
end function

private function hMatch( byref x as integer, byval tk as integer ) as integer
	if( tkGet( x ) = tk ) then
		x += 1
		function = TRUE
	end if
end function

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
private function hStringLiteralSequence( byref x as integer ) as ASTNODE ptr
	dim as ASTNODE ptr a

	do
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
				exit do
			end if
			x += 1

			s = astNewUOP( ASTCLASS_STRINGIFY, astNewID( tkGetText( x ) ) )

		case else
			exit do
		end select

		if( a = NULL ) then
			a = s
		else
			a = astNewBOP( ASTCLASS_STRCAT, a, s )
		end if

		x += 1
	loop

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
		byval x as integer, _
		byval macro as ASTNODE ptr _
	) as integer

	var is_type = FALSE

	select case( tkGet( x ) )
	case KW___CDECL, KW___STDCALL, KW___ATTRIBUTE__, _
	     KW_SIGNED, KW_UNSIGNED, KW_CONST, _
	     KW_SHORT, KW_LONG, _
	     KW_ENUM, KW_STRUCT, KW_UNION, _
	     KW_VOID, KW_CHAR, KW_FLOAT, KW_DOUBLE, KW_INT
		is_type = not hIdentifierIsMacroParam( macro, tkGetIdOrKw( x ) )
	case TK_ID
		var id = tkGetText( x )
		if( (hIdentifyCommonTypedef( id ) <> TYPE_NONE) or _
		    hIsTypedef( id ) ) then
			is_type = not hIdentifierIsMacroParam( macro, id )
		end if
	end select

	function = is_type
end function

'' Parse cast expression for '(DataType) foo', or sizeof operand for
'' 'sizeof (DataType)'.
private function hDataTypeInParens _
	( _
		byref x as integer, _
		byval decl as integer _
	) as ASTNODE ptr

	''
	'' Using cMultDecl() to parse:
	''
	''    BaseType Declarator
	''
	'' Parsing just the base data type isn't enough, because it could be a
	'' function pointer cast with parameter list etc. We need to do full
	'' declarator parsing to handle that.
	''
	'' cMultDecl()/cIdList() will have built up a GROUP, for DECL_CASTTYPE
	'' there should be 1 child only though, extract it.
	''
	function = astUngroupOne( cMultDecl( x, DECL_CASTTYPE, 0, "" ) )

	'' ')'
	tkExpect( x, TK_RPAREN, iif( decl = DECL_CASTTYPE, _
			@"to close '(...)' type cast", _
			@"to close 'sizeof (...)'" ) )
	x += 1
end function

'' Scope block: '{' (Expression ';')* '}'
'' Initializer: '{' Expression (',' Expression)* '}'
private function hScopeBlockOrInitializer _
	( _
		byref x as integer, _
		byval macro as ASTNODE ptr _
	) as ASTNODE ptr

	var a = astNewGROUP( )
	x += 1

	'' '}'?
	while( tkGet( x ) <> TK_RBRACE )
		astAppend( a, cExpression( x, 0, macro ) )

		select case( a->class )
		case ASTCLASS_STRUCTINIT
			'' '}'?
			if( tkGet( x ) = TK_RBRACE ) then exit while

			'' ','?
			tkExpect( x, TK_COMMA, "(expression separator in struct initializer)" )
			x += 1

		case ASTCLASS_SCOPEBLOCK
			'' ';'?
			tkExpect( x, TK_SEMI, "(end of statement in scope block)" )
			x += 1

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
				tkOopsExpected( x, "',' (expression separator in struct initializer), or ';' (end of statement in scope block), or '}' (end of block)" )
			end select
		end select
	wend

	'' If no ',' or ';' was found and we don't know whether it's a struct
	'' initializer or a scope block, make an assumption...
	if( a->class = ASTCLASS_GROUP ) then
		a->class = ASTCLASS_STRUCTINIT
	end if

	tkExpect( x, TK_RBRACE, iif( a->class = ASTCLASS_STRUCTINIT, _
		@"to close struct initializer", _
		@"to close scope block" ) )
	x += 1

	function = a
end function

'' C expression parser based on precedence climbing
private function cExpression _
	( _
		byref x as integer, _
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
		a = astNewUOP( op, cExpression( x, cprecedence(op), macro ) )
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
			var y = hFindClosingParen( x - 1 ) + 1
			select case( tkGet( y ) )
			case TK_RPAREN, TK_EOF, TK_END
				is_cast = FALSE
			end select

			if( is_cast ) then
				'' DataType ')'
				var t = hDataTypeInParens( x, DECL_CASTTYPE )

				'' Expression
				a = astNewUOP( ASTCLASS_CAST, cExpression( x, 0, macro ) )

				assert( t->class = ASTCLASS_TYPE )
				astSetType( a, t->dtype, astClone( t->subtype ) )
				astDelete( t )
			else
				'' Expression
				a = cExpression( x, 0, macro )

				'' ')'
				tkExpect( x, TK_RPAREN, "to close '(...)' parenthesized expression" )
				x += 1
			end if

		case TK_OCTNUM, TK_DECNUM, TK_HEXNUM, TK_DECFLOAT
			a = hNumberLiteral( x )
			x += 1

		case TK_STRING, TK_WSTRING, TK_HASH
			a = hStringLiteralSequence( x )

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
			a = astNewID( tkGetText( x ) )
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
						astAppend( a, cExpression( x, 0, macro ) )

						'' ','?
					loop while( hMatch( x, TK_COMMA ) )
				end if

				'' ')'?
				tkExpect( x, TK_RPAREN, "to close call argument list" )
				x += 1

			'' '##'?
			case TK_HASHHASH
				var t = astNew( ASTCLASS_PPMERGE )
				astAppend( t, a )
				a = t
				x += 1

				'' Identifier ('##' Identifier)*
				do
					'' Identifier?
					tkExpect( x, TK_ID, "as operand of '##' PP merge operator" )
					astAppend( a, astNewID( tkGetText( x ) ) )
					x += 1

					'' '##'?
				loop while( hMatch( x, TK_HASHHASH ) )

			end select

		'' Scope block: '{' (Expression ';')* '}'
		'' Initializer: '{' Expression (',' Expression)* '}'
		case TK_LBRACE
			a = hScopeBlockOrInitializer( x, macro )

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
				var t = hDataTypeInParens( x, DECL_SIZEOFTYPE )

				a = astNew( ASTCLASS_SIZEOFTYPE )

				assert( t->class = ASTCLASS_TYPE )
				astSetType( a, t->dtype, astClone( t->subtype ) )
				astDelete( t )
			else
				a = astNewUOP( ASTCLASS_SIZEOF, cExpression( x, cprecedence(ASTCLASS_SIZEOF), macro ) )
			end if

		case else
			tkOopsExpected( x, "an atomic expression" )
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
		case TK_LBRACKET : op = ASTCLASS_INDEX   '' [ (a[b])
		case TK_DOT      : op = ASTCLASS_MEMBER  '' .
		case TK_ARROW    : op = ASTCLASS_MEMBERDEREF '' ->
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
		x += 1

		'' rhs
		var b = cExpression( x, oplevel, macro )

		'' Handle ?: special case
		if( op = ASTCLASS_IIF ) then
			'' ':'
			tkExpect( x, TK_COLON, "for a?b:c iif operator" )
			x += 1

			var c = cExpression( x, oplevel, macro )

			a = astNewIIF( a, b, c )
		else
			'' Handle [] special case
			if( op = ASTCLASS_INDEX ) then
				'' ']'
				tkExpect( x, TK_RBRACKET, "for [] indexing operator" )
				x += 1
			end if

			a = astNewBOP( op, a, b )
		end if
	loop

	function = a
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private sub hCdeclAttribute( byref x as integer, byref gccattribs as integer )
	if( gccattribs and ASTATTRIB_STDCALL ) then
		tkOops( x, "cdecl attribute specified together with stdcall" )
	end if
	gccattribs or= ASTATTRIB_CDECL
	x += 1
end sub

private sub hStdcallAttribute( byref x as integer, byref gccattribs as integer )
	if( gccattribs and ASTATTRIB_CDECL ) then
		tkOops( x, "stdcall attribute specified together with cdecl" )
	end if
	gccattribs or= ASTATTRIB_STDCALL
	x += 1
end sub

private sub cGccAttribute( byref x as integer, byref gccattribs as integer )
	if( tkGet( x ) < TK_ID ) then
		tkOopsExpected( x, "expected attribute identifier inside __attribute__((...))" )
	end if

	select case( *tkGetText( x ) )
	case "warn_unused_result", "__warn_unused_result__", _
	     "noreturn", "__noreturn__", _
	     "malloc", "__malloc__", _
	     "deprecated", "__deprecated__"
		'' Ignore, not interesting for FB bindings
		x += 1

	case "cdecl", "__cdecl__"
		hCdeclAttribute( x, gccattribs )

	case "stdcall", "__stdcall__"
		hStdcallAttribute( x, gccattribs )

	case else
		tkOops( x, "unknown attribute" )
	end select
end sub

private sub cGccAttributeList( byref x as integer, byref gccattribs as integer )
	do
		select case( tkGet( x ) )
		'' __cdecl
		case KW___CDECL
			hCdeclAttribute( x, gccattribs )

		'' __stdcall
		case KW___STDCALL
			hStdcallAttribute( x, gccattribs )

		'' __attribute__((...)):
		'' __ATTRIBUTE__ '((' Attribute (',' Attribute)* '))'
		case KW___ATTRIBUTE__
			x += 1

			'' '('?
			tkExpect( x, TK_LPAREN, "as 1st '(' in '__attribute__((...))'" )
			x += 1

			'' '('?
			tkExpect( x, TK_LPAREN, "as 2nd '(' in '__attribute__((...))'" )
			x += 1

			'' Attribute (',' Attribute)*
			do
				'' ')'?
				if( tkGet( x ) = TK_RPAREN ) then
					exit do
				end if

				'' Attribute
				cGccAttribute( x, gccattribs )

				'' ','?
			loop while( hMatch( x, TK_COMMA ) )

			'' ')'?
			tkExpect( x, TK_RPAREN, "as 1st ')' in '__attribute__((...))'" )
			x += 1

			'' ')'?
			tkExpect( x, TK_RPAREN, "as 2nd ')' in '__attribute__((...))'" )
			x += 1

		case else
			exit do
		end select
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Enum constant: Identifier ['=' Expression] (',' | '}')
private function cEnumConst( byref x as integer ) as ASTNODE ptr
	'' Identifier
	tkExpect( x, TK_ID, "for an enum constant" )
	var t = astNew( ASTCLASS_ENUMCONST, tkGetText( x ) )
	x += 1

	'' '='?
	if( hMatch( x, TK_EQ ) ) then
		'' Expression
		t->expr = cExpression( x, 0, NULL )
	end if

	'' (',' | '}')
	select case( tkGet( x ) )
	case TK_COMMA
		x += 1

	case TK_RBRACE

	case else
		tkOopsExpected( x, "',' or '}' behind enum constant" )
	end select

	function = t
end function

private function cFieldDecl( byref x as integer ) as ASTNODE ptr
	function = cMultDecl( x, DECL_FIELD, 0, "" )
end function

'' {STRUCT|UNION|ENUM} [Identifier] '{' StructBody|EnumBody '}'
'' {STRUCT|UNION|ENUM} Identifier
private function cStruct( byref x as integer ) as ASTNODE ptr
	var head = x

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

	'' [Identifier]
	if( tkGet( x ) = TK_ID ) then
		astSetText( struct, TAG_PREFIX + *tkGetText( x ) )
		x += 1
	end if

	'' '{'?
	if( tkGet( x ) = TK_LBRACE ) then
		x += 1

		astAppend( struct, _
			cToplevel( x, iif( astclass = ASTCLASS_ENUM, _
				BODY_ENUM, BODY_STRUCT ) ) )

		'' '}'
		tkExpect( x, TK_RBRACE, "to close " + astDumpPrettyDecl( struct ) + " block" )
		x += 1
	else
		if( struct->text = NULL ) then
			tkOopsExpected( x, "'{' or tag name behind " + astDumpPrettyDecl( struct ) )
		end if

		'' It's just a tag name, not a body
		struct->class = ASTCLASS_ID
	end if

	function = struct
end function

private function cTypedef( byref x as integer ) as ASTNODE ptr
	'' TYPEDEF
	var comment = tkCollectComments( x, x )
	x += 1

	function = cMultDecl( x, DECL_TYPEDEF, 0, comment )
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function cDefine( byref x as integer ) as ASTNODE ptr
	var t = tkGetAst( x )

	t = astClone( t )
	astAddComment( t, tkCollectComments( x, x ) )
	x += 1

	'' Temporarily insert the macro body tokens from AST back into the
	'' tk buffer, allowing them to be parsed by cExpression() here.
	var bodybegin = x
	hInsertMacroBody( bodybegin, t )

	assert( tkGet( x ) = TK_BEGIN )
	x += 1

	dim as ASTNODE ptr expr

	'' Macro body empty?
	if( tkGet( x ) = TK_END ) then
		expr = NULL
	else
		expr = cExpression( x, 0, t )

		'' Must have reached the TK_END
		if( tkGet( x ) <> TK_END ) then
			tkOops( x, "couldn't parse all of the #define's body as an expression, only the beginning" )
		end if
	end if

	assert( tkGet( x ) = TK_END )

	'' Remove the temporarily inserted macro body
	tkRemove( bodybegin, x )
	x = bodybegin

	assert( t->expr->class = ASTCLASS_MACROBODY )
	astDelete( t->expr )
	t->expr = expr
	function = t
end function

private function cOtherPPToken( byref x as integer ) as ASTNODE ptr
	var astclass = ASTCLASS_DIVIDER
	select case( tkGet( x ) )
	case TK_PPINCLUDE : astclass = ASTCLASS_PPINCLUDE
	case TK_PPUNDEF   : astclass = ASTCLASS_PPUNDEF
	case else         : assert( tkGet( x ) = TK_DIVIDER )
	end select

	var t = astNew( astclass, tkGetText( x ) )
	astAddComment( t, tkCollectComments( x, x ) )
	x += 1

	function = t
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
		byref x as integer, _
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

	do
		'' __ATTRIBUTE__((...))
		cGccAttributeList( x, gccattribs )

		select case( tkGet( x ) )
		case KW_SIGNED
			if( unsignedmods > 0 ) then
				tkOops( x, "mixed SIGNED with previous UNSIGNED modifier" )
			end if
			signedmods += 1

		case KW_UNSIGNED
			if( signedmods > 0 ) then
				tkOops( x, "mixed UNSIGNED with previous SIGNED modifier" )
			end if
			unsignedmods += 1

		case KW_CONST
			constmods += 1

		case KW_SHORT
			if( longmods > 0 ) then
				tkOops( x, "mixed SHORT with previous LONG modifier" )
			end if
			shortmods += 1
			if( shortmods > 1 ) then
				tkOops( x, "more than 1 SHORT modifier" )
			end if

		case KW_LONG
			if( shortmods > 0 ) then
				tkOops( x, "mixed LONG with previous SHORT modifier" )
			end if
			longmods += 1
			if( longmods > 2 ) then
				tkOops( x, "more than 2 LONG modifiers" )
			end if

		case else
			'' Only one base type is allowed
			if( dtype <> TYPE_NONE ) then
				exit do
			end if

			select case( tkGet( x ) )
			case KW_ENUM, KW_STRUCT, KW_UNION
				dtype = TYPE_UDT
				subtype = cStruct( x )
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
					exit do
				end if

				'' Treat the id as the type
				var id = tkGetText( x )
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
				exit do
			end select
		end select

		x += 1
	loop

	'' Some details can only be decided after parsing the whole thing,
	'' because for example "unsigned int" and "int unsigned" both are allowed.
	select case( dtype )
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
			select case( decl )
			case DECL_CASTTYPE
				tkOopsExpected( x, "a data type in this '(...)' type cast" )
			case DECL_SIZEOFTYPE
				tkOopsExpected( x, "a data type as operand in this 'sizeof(...)'" )
			case else
				tkOopsExpected( x, "a data type at the beginning of this declaration" )
			end select
		end if
	end select

	select case( dtype )
	case TYPE_ANY, TYPE_SINGLE, TYPE_DOUBLE, TYPE_UDT
		if( signedmods or unsignedmods or shortmods or longmods ) then
			tkOops( x, "SIGNED|UNSIGNED|SHORT|LONG modifiers used with void/float/double/typedef/UDT" )
		end if
	case TYPE_ZSTRING, TYPE_BYTE, TYPE_UBYTE
		if( shortmods or longmods ) then
			tkOops( x, "SHORT|LONG modifiers used with CHAR type" )
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
	cGccAttributeList( x, gccattribs )
end sub

'' ParamDeclList = ParamDecl (',' ParamDecl)*
'' ParamDecl = '...' | MultDecl{Param}
private function cParamDeclList( byref x as integer ) as ASTNODE ptr
	var group = astNewGROUP( )

	do
		dim as ASTNODE ptr t

		'' '...'?
		if( tkGet( x ) = TK_ELLIPSIS ) then
			t = astNew( ASTCLASS_PARAM )
			astAddComment( t, tkCollectComments( x, x ) )
			x += 1
		else
			t = cMultDecl( x, DECL_PARAM, 0, "" )
		end if

		astAppend( group, t )

		'' ','?
	loop while( hMatch( x, TK_COMMA ) )

	function = group
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
		byref x as integer, _
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
	'' already because they apply to the whole multdecl:
	''    int __attribute__((stdcall)) f1(void), f2(void);
	'' both should be stdcall.
	''
	'' But this is still here, to handle __attribute__'s appearing in
	'' nested declarators:
	''    int (__attribute__((stdcall)) f1)(void);
	'' or at the front of follow-up declarators in a multdecl:
	''    int f1(void), __attribute__((stdcall)) f2(void);
	''
	cGccAttributeList( x, gccattribs )

	'' Pointers: ('*')*
	while( hMatch( x, TK_STAR ) )
		procptrdtype = typeAddrOf( procptrdtype )
		dtype = typeAddrOf( dtype )

		'' (CONST|RESTRICT|__ATTRIBUTE__((...)))*
		do
			'' __ATTRIBUTE__((...))
			cGccAttributeList( x, gccattribs )

			select case( tkGet( x ) )
			case KW_CONST
				procptrdtype = typeSetIsConst( procptrdtype )
				dtype = typeSetIsConst( dtype )
				x += 1

			case KW_RESTRICT, KW___RESTRICT, KW___RESTRICT__
				'' The restrict keyword is not interesting for FB bindings, just ignore
				x += 1

			case else
				exit do
			end select
		loop
	wend

	''    '(' Declarator ')'    |    [Identifier]
	dim as ASTNODE ptr t, innernode

	'' '('?
	if( hMatch( x, TK_LPAREN ) ) then
		t = cDeclarator( x, nestlevel + 1, decl, dtype, basesubtype, 0, innernode, innerprocptrdtype, innergccattribs )

		'' ')'
		tkExpect( x, TK_RPAREN, "for '(...)' parenthesized declarator" )
		x += 1
	else
		'' [Identifier]
		'' An identifier must exist, except for parameters/types, and
		'' in fact for types there mustn't be an id.
		dim as string id
		select case( decl )
		case DECL_CASTTYPE, DECL_SIZEOFTYPE
		case else
			if( tkGet( x ) = TK_ID ) then
				id = *tkGetText( x )
				x += 1
			else
				if( decl <> DECL_PARAM ) then
					tkOopsExpected( x, "identifier for the symbol declared in this declaration" )
				end if
			end if
		end select

		static as integer decl_to_astclass(0 to DECL__COUNT-1) = _
		{ _
			ASTCLASS_VAR      , _ '' DECL_VAR
			ASTCLASS_EXTERNVAR, _ '' DECL_EXTERNVAR
			ASTCLASS_STATICVAR, _ '' DECL_STATICVAR
			ASTCLASS_FIELD    , _ '' DECL_FIELD
			ASTCLASS_PARAM    , _ '' DECL_PARAM
			ASTCLASS_TYPEDEF  , _ '' DECL_TYPEDEF
			ASTCLASS_TYPE     , _ '' DECL_CASTTYPE
			ASTCLASS_TYPE       _ '' DECL_SIZEOFTYPE
		}

		if( decl = DECL_TYPEDEF ) then
			hAddTypedef( id )
		end if

		t = astNew( decl_to_astclass(decl), id )
		astSetType( t, dtype, basesubtype )
	end if

	node = t

	select case( tkGet( x ) )
	'' ('[' [ArrayElements] ']')*
	case TK_LBRACKET
		'' Can't allow arrays on everything - currently, it's only
		'' handled for vars/fields/params/typedefs
		select case( decl )
		case DECL_VAR, DECL_EXTERNVAR, DECL_STATICVAR, _
		     DECL_FIELD, DECL_PARAM, DECL_TYPEDEF

		case else
			tkOops( x, "TODO: arrays not supported here yet" )
		end select

		assert( node->array = NULL )
		node->array = astNew( ASTCLASS_ARRAY )

		'' For each array dimension...
		do
			'' '['
			x += 1

			dim as ASTNODE ptr d

			'' Just '[]'?
			if( tkGet( x ) = TK_RBRACKET ) then
				'' Only allowed on parameters, not variables etc.
				if( decl <> DECL_PARAM ) then
					tkOops( x, "array dimension must have an explicit size here" )
				end if
				d = astNewDIMENSION( NULL, NULL )
			else
				d = cExpression( x, 0, NULL )

				'' Add new DIMENSION to the ARRAY:
				'' lbound = 0, ubound = elements - 1
				d = astNewDIMENSION( _
						astNewCONSTI( 0, TYPE_LONG ), _
						astNewBOP( ASTCLASS_SUB, _
							d, _
							astNewCONSTI( 1, TYPE_LONG ) ) )
			end if
			astAppend( node->array, d )

			'' ']'
			tkExpect( x, TK_RBRACKET, "to close this array dimension declaration" )
			x += 1

			'' '['? (next dimension)
		loop while( tkGet( x ) = TK_LBRACKET )

	'' ':' <bits>
	case TK_COLON
		if( decl <> DECL_FIELD ) then
			tkOops( x, "bitfields not supported here" )
		end if
		x += 1

		node->bits = cExpression( x, 0, NULL )

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
			case ASTCLASS_VAR, ASTCLASS_EXTERNVAR, ASTCLASS_STATICVAR, ASTCLASS_FIELD
				t->class = ASTCLASS_PROC
			end select
		end if

		'' Just '(void)'?
		if( (tkGet( x ) = KW_VOID) and (tkGet( x + 1 ) = TK_RPAREN) ) then
			'' VOID
			x += 1
		'' Not just '()'?
		elseif( tkGet( x ) <> TK_RPAREN ) then
			astAppend( node, cParamDeclList( x ) )
		end if

		'' ')'
		tkExpect( x, TK_RPAREN, "to close parameter list in function declaration" )
		x += 1
	end select

	'' __ATTRIBUTE__((...))
	var endgccattribs = 0
	cGccAttributeList( x, endgccattribs )

	select case( t->class )
	case ASTCLASS_PARAM, ASTCLASS_VAR, ASTCLASS_STATICVAR
		'' ['=' Initializer]
		if( hMatch( x, TK_EQ ) ) then
			assert( t->expr = NULL )
			t->expr = cExpression( x, 0, NULL )
		end if
	end select

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

'' IdList = Declarator (',' Declarator)* [';']
private function cIdList _
	( _
		byref x as integer, _
		byval decl as integer, _
		byval basedtype as integer, _
		byval basesubtype as ASTNODE ptr, _
		byval gccattribs as integer, _
		byref comment as string _
	) as ASTNODE ptr

	var group = astNewGROUP( )

	'' ... (',' ...)*
	do
		var begin = x

		astAppend( group, cDeclarator( x, 0, decl, basedtype, basesubtype, gccattribs, NULL, 0, 0 ) )

		'' The first declaration takes the comments from the base type
		if( len( comment ) > 0 ) then
			astAddComment( group->tail, comment )
			comment = ""
		end if

		'' Every declaration takes the comments on its declarator tokens
		astAddComment( group->tail, tkCollectComments( begin, x - 1 ) )

		'' Everything can have a comma and more identifiers,
		'' except for parameters/types.
		select case( decl )
		case DECL_PARAM, DECL_CASTTYPE, DECL_SIZEOFTYPE
			exit do
		end select

		'' ','?
	loop while( hMatch( x, TK_COMMA ) )

	'' Everything except parameters/types must end with a ';'
	select case( decl )
	case DECL_PARAM, DECL_CASTTYPE, DECL_SIZEOFTYPE
	case else
		'' ';'
		tkExpect( x, TK_SEMI, "to finish this declaration" )
		x += 1
	end select

	function = group
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
'' MultDecl = GccAttributeList BaseType IdList
''
private function cMultDecl _
	( _
		byref x as integer, _
		byval decl as integer, _
		byval gccattribs as integer, _
		byref comment as string _
	) as ASTNODE ptr

	var begin = x

	'' BaseType
	dim as integer dtype
	dim as ASTNODE ptr subtype
	cBaseType( x, dtype, subtype, gccattribs, decl )

	'' Special case for standalone struct/union/enum declarations (no CONST bits):
	if( dtype = TYPE_UDT ) then
		select case( subtype->class )
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			'' ';'?
			if( tkGet( x ) = TK_SEMI ) then
				x += 1
				return subtype
			end if
		end select
	end if

	var result = astNewGROUP( )

	'' Special case for struct/union/enum bodies used as basetype:
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
			subtype = astNewID( subtype->text )
		end select
	end if

	comment += tkCollectComments( begin, x - 1 )

	'' IdList
	astAppend( result, cIdList( x, decl, dtype, subtype, gccattribs, comment ) )

	astDelete( subtype )
	function = result
end function

'' Global variable/procedure declarations
''    GccAttributeList [EXTERN|STATIC] MultDecl
private function cGlobalDecl( byref x as integer ) as ASTNODE ptr
	var begin = x

	'' __ATTRIBUTE__((...))
	var gccattribs = 0
	cGccAttributeList( x, gccattribs )

	'' [EXTERN|STATIC]
	var decl = DECL_VAR
	select case( tkGet( x ) )
	case KW_EXTERN
		decl = DECL_EXTERNVAR
		x += 1
	case KW_STATIC
		decl = DECL_STATICVAR
		x += 1
	end select

	var comment = tkCollectComments( begin, x - 1 )

	'' MultDecl
	function = cMultDecl( x, decl, gccattribs, comment )
end function

private function cSemiColon( byref x as integer ) as ASTNODE ptr
	'' Just ignore single semi-colons
	x += 1
	function = astNewGROUP( )
end function

'' Parse a construct. The body parameter is used to determine the context,
'' i.e. toplevel vs. struct body vs. enum body. All constructs are
'' identified/disambiguated here, then parsing is handed of to helper functions
'' for each construct.
private function cConstruct _
	( _
		byref x as integer, _
		byval body as integer _
	) as ASTNODE ptr

	select case( tkGet( x ) )
	case TK_PPDEFINE
		return cDefine( x )
	case TK_PPINCLUDE, TK_PPUNDEF, TK_DIVIDER
		return cOtherPPToken( x )
	end select

	if( body = BODY_ENUM ) then
		return cEnumConst( x )
	end if

	select case( tkGet( x ) )
	case KW_TYPEDEF
		return cTypedef( x )
	case TK_SEMI
		return cSemiColon( x )
	end select

	if( body = BODY_STRUCT ) then
		return cFieldDecl( x )
	end if

	function = cGlobalDecl( x )
end function

'' Parse constructs in a file (at toplevel) or in a nested block (inside
'' struct/union/enum bodies).
private function cToplevel _
	( _
		byref x as integer, _
		byval body as integer _
	) as ASTNODE ptr

	var group = astNewGROUP( )

	while( tkGet( x ) <> TK_EOF )
		dim as ASTNODE ptr t

		if( body <> BODY_TOPLEVEL ) then
			'' '}'?
			if( tkGet( x ) = TK_RBRACE ) then
				exit while
			end if
		end if

		astAppend( group, cConstruct( x, body ) )
	wend

	function = group
end function

function cFile( ) as ASTNODE ptr
	hashInit( @file.typedefs, 4, TRUE )
	function = cToplevel( 0, BODY_TOPLEVEL )
	hashEnd( @file.typedefs )
end function

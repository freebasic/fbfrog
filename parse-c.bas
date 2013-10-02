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
	DECL_CAST
	DECL__COUNT
end enum

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
	BODY_PPIF
end enum

private function hMatch( byref x as integer, byval tk as integer ) as integer
	if( tkGet( x ) = tk ) then
		x += 1
		function = TRUE
	end if
end function

private function hMakeTempId( ) as string
	static n as integer
	function = FROG_ANON_PREFIX & n
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

private function hAppendStrLit _
	( _
		byval a as ASTNODE ptr, _
		byval s as ASTNODE ptr _
	) as ASTNODE ptr

	if( a = NULL ) then
		function = s
	else
		function = astNewBOP( ASTOP_STRCAT, a, s )
	end if

end function

'' ("..." | #id)*
private function hStringLiteralSequence( byref x as integer ) as ASTNODE ptr
	dim as ASTNODE ptr a

	do
		select case( tkGet( x ) )
		case TK_STRING
			var s = astNew( ASTCLASS_STRING, tkGetText( x ) )
			astSetType( s, TYPE_ZSTRING, NULL )
			a = hAppendStrLit( a, s )
			x += 1

		case TK_WSTRING
			var s = astNew( ASTCLASS_STRING, tkGetText( x ) )
			astSetType( s, TYPE_WSTRING, NULL )
			a = hAppendStrLit( a, s )
			x += 1

		'' '#' stringify operator
		case TK_HASH
			x += 1

			'' #id?
			tkExpect( x, TK_ID, "as operand of '#' PP stringify operator" )
			var s = astNewUOP( ASTOP_STRINGIFY, astNewID( tkGetText( x ) ) )
			a = hAppendStrLit( a, s )
			x += 1

		case else
			exit do
		end select
	loop

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
	case TK_EXCL   : op = ASTOP_CLOGNOT   '' !
	case TK_TILDE  : op = ASTOP_NOT       '' ~
	case TK_MINUS  : op = ASTOP_NEGATE    '' -
	case TK_PLUS   : op = ASTOP_UNARYPLUS '' +
	case TK_AMP    : op = ASTOP_ADDROF    '' &
	case TK_STAR   : op = ASTOP_DEREF     '' *
	case KW_SIZEOF : op = ASTOP_SIZEOF    '' sizeof
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

			''
			'' Try to disambiguate between DataType and Expression,
			'' even without being a full C compiler, good guesses can be made.
			''
			'' If it starts with a data type keyword, and isn't inside
			'' a macro where that's a macro parameter, then it must be
			'' a data type, because it couldn't appear in an expression.
			''
			'' Of course that's an unsafe assumption because any identifier
			'' could have been re-#defined to something different than what
			'' fbfrog assumes, in #include files that fbfrog doesn't even parse,
			'' etc... but for common typedefs such as size_t that probably won't
			'' be a problem in practice.
			''
			'' If there's just an identifier then it could be a typedef
			'' but we can't be sure. Finding out whether it is a typedef
			'' would require checking all previous declarations in this
			'' file and in #includes, that's not possible currently
			'' because #includes aren't always merged in.
			''
			var is_cast = FALSE

			select case( tkGet( x ) )
			case KW___CDECL, KW___STDCALL, KW___ATTRIBUTE__, _
			     KW_SIGNED, KW_UNSIGNED, KW_CONST, _
			     KW_SHORT, KW_LONG, _
			     KW_ENUM, KW_STRUCT, KW_UNION, _
			     KW_VOID, KW_CHAR, KW_FLOAT, KW_DOUBLE, KW_INT
				is_cast = not hIdentifierIsMacroParam( macro, tkGetIdOrKw( x ) )
			case TK_ID
				var id = tkGetText( x )
				if( hIdentifyCommonTypedef( id ) <> TYPE_NONE ) then
					is_cast = not hIdentifierIsMacroParam( macro, id )
					if( is_cast and verbose ) then
						tkReport( x, "treating id as typedef" )
					end if
				end if
			end select

			if( is_cast ) then
				'' BaseType Declarator
				'' (parsing just the base data type isn't enough, because it
				'' could be a function pointer cast with parameter list etc.)
				a = cMultDecl( x, DECL_CAST, 0, "" )

				'' cMultDecl()/cIdList() will have built up a GROUP,
				'' for DECL_CAST there should be 1 child only, extract it.
				assert( a->class = ASTCLASS_GROUP )
				assert( a->head )
				assert( a->head = a->tail )
				var tmp = astClone( a->head )
				astDelete( a )
				a = tmp

				'' For DECL_CAST, cDeclarator() should build up the CAST UOP
				assert( a->class = ASTCLASS_UOP )
				assert( a->op = ASTOP_CAST )

				'' ')'
				tkExpect( x, TK_RPAREN, "to close '(...)' type cast" )
				x += 1

				'' Expression
				a->l = cExpression( x, 0, macro )
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

		case TK_STRING
			a = hStringLiteralSequence( x )

		case TK_WSTRING
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

		'' '#' stringify operator
		case TK_HASH
			a = hStringLiteralSequence( x )

		'' '{' Statements '}'
		case TK_LBRACE
			a = astNew( ASTCLASS_SCOPEBLOCK )
			x += 1

			'' Statements = Statement*
			while( tkGet( x ) <> TK_RBRACE )
				'' Statement = Expression ';'
				astAppend( a, cExpression( x, 0, macro ) )
				tkExpect( x, TK_SEMI, "to terminate statement" )
				x += 1
			wend

			tkExpect( x, TK_RBRACE, "to close scope block" )
			x += 1

		case else
			tkOops( x, "not an atomic expression (identifier, literal, ...), or not yet implemented" )
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
		case TK_LBRACKET : op = ASTOP_INDEX   '' [ (a[b])
		case TK_DOT      : op = ASTOP_MEMBER  '' .
		case TK_ARROW    : op = ASTOP_MEMBERDEREF '' ->
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
		x += 1

		'' rhs
		var b = cExpression( x, oplevel, macro )

		'' Handle ?: special case
		if( op = ASTOP_IIF ) then
			'' ':'
			tkExpect( x, TK_COLON, "for a?b:c iif operator" )
			x += 1

			var c = cExpression( x, oplevel, macro )

			a = astNewIIF( a, b, c )
		else
			'' Handle [] special case
			if( op = ASTOP_INDEX ) then
				'' ']'
				tkExpect( x, TK_RBRACKET, "for [] indexing operator" )
				x += 1
			end if

			a = astNewBOP( op, a, b )
		end if
	loop

	function = a
end function

private function hExpr _
	( _
		byref x as integer, _
		byval is_bool_context as integer, _
		byval macro as ASTNODE ptr _
	) as ASTNODE ptr

	var expr = cExpression( x, 0, macro )

	function = astFold( astOpsC2FB( expr ), NULL, FALSE, is_bool_context )
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

'' Identifier ['=' Expression] (',' | '}')
private function cEnumConst( byref x as integer ) as ASTNODE ptr
	'' Identifier
	tkExpect( x, TK_ID, "for an enum constant" )
	var n = astNew( ASTCLASS_ENUMCONST, tkGetText( x ) )
	x += 1

	'' '='?
	if( hMatch( x, TK_EQ ) ) then
		'' Expression
		n->expr = hExpr( x, FALSE, NULL )
	end if

	'' (',' | '}')
	select case( tkGet( x ) )
	case TK_COMMA
		x += 1

	case TK_RBRACE

	case else
		tkOopsExpected( x, "',' or '}' behind enum constant" )
	end select

	function = n
end function

'' Typedefs
''    TYPEDEF MultDecl
private function cTypedef( byref x as integer ) as ASTNODE ptr
	'' TYPEDEF?
	var comment = tkCollectComments( x, x )
	tkExpect( x, KW_TYPEDEF, "for typedef declaration" )
	x += 1

	function = cMultDecl( x, DECL_TYPEDEF, 0, comment )
end function

'' Structs/Unions, Enums
'' [TYPEDEF] {STRUCT|UNION|ENUM} [Identifier] '{'
''     {StructBody|EnumBody}
'' '}' [MultDecl] ';'
private function cStructCompound( byref x as integer ) as ASTNODE ptr
	var head = x

	'' TYPEDEF?
	var is_typedef = hMatch( x, KW_TYPEDEF )

	'' {STRUCT|UNION|ENUM}
	dim as integer astclass
	select case( tkGet( x ) )
	case KW_UNION
		astclass = ASTCLASS_UNION
	case KW_ENUM
		astclass = ASTCLASS_ENUM
	case KW_STRUCT
		astclass = ASTCLASS_STRUCT
	case else
		tkOopsExpected( x, "STRUCT|UNION|ENUM at beginning of struct/union/enum block" )
	end select
	x += 1

	'' [Identifier]
	dim as string id
	if( tkGet( x ) = TK_ID ) then
		id = *tkGetText( x )
		x += 1
	elseif( is_typedef ) then
		'' If it's a typedef with anonymous struct block, we need to
		'' make up an id for it, for use in the base type of the
		'' typedef MultDecl. If it turns out to be just a single
		'' typedef, we can still solve it out later.
		id = hMakeTempId( )
	end if

	'' '{'
	tkExpect( x, TK_LBRACE, iif( astclass = ASTCLASS_ENUM, _
			@"to open enum block", _
			@"to open struct block" ) )
	x += 1

	var struct = astNew( astclass, id )
	astAddComment( struct, tkCollectComments( head, x - 1 ) )

	astAppend( struct, _
		cToplevel( x, iif( astclass = ASTCLASS_ENUM, _
			BODY_ENUM, BODY_STRUCT ) ) )

	'' '}'
	tkExpect( x, TK_RBRACE, iif( astclass = ASTCLASS_ENUM, _
			@"to close enum block", _
			@"to close struct block" ) )
	x += 1

	if( is_typedef ) then
		'' IdList
		var subtype = astNewID( id )
		var t = cIdList( x, DECL_TYPEDEF, TYPE_UDT, subtype, 0, "" )
		astDelete( subtype )
		var group = astNewGROUP( )
		astAppend( group, struct )
		astAppend( group, t )
		function = group
	else
		'' ';'
		tkExpect( x, TK_SEMI, iif( astclass = ASTCLASS_ENUM, _
				@"to finish enum block declaration", _
				@"to finish struct block declaration" ) )
		x += 1
		function = struct
	end if
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
	var basetypex = -1

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
			if( basetypex >= 0 ) then
				exit do
			end if

			select case( tkGet( x ) )
			case KW_ENUM, KW_STRUCT, KW_UNION
				'' {ENUM|STRUCT|UNION}
				x += 1

				'' Identifier
				tkExpect( x, TK_ID, ", the tag name as in 'struct|union|enum TAGNAME'" )
				basetypex = x

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
				basetypex = x

			case KW_VOID, KW_CHAR, KW_FLOAT, KW_DOUBLE, KW_INT
				basetypex = x

			case else
				exit do
			end select
		end select

		x += 1
	loop

	''
	'' 2. Refuse invalid modifier combinations etc.
	''

	select case( tkGet( basetypex ) )
	case TK_ID, KW_VOID, KW_FLOAT, KW_DOUBLE
		if( signedmods or unsignedmods or shortmods or longmods ) then
			tkOops( x, "SIGNED|UNSIGNED|SHORT|LONG modifiers used with typedef/UDT/float/double/void" )
		end if

		select case( tkGet( basetypex ) )
		case TK_ID
			var id = tkGetText( basetypex )
			dtype = hIdentifyCommonTypedef( id )
			if( dtype = TYPE_NONE ) then
				dtype = TYPE_UDT
				subtype = astNewID( id )
			end if
		case KW_VOID
			dtype = TYPE_ANY
		case KW_FLOAT
			dtype = TYPE_SINGLE
		case KW_DOUBLE
			dtype = TYPE_DOUBLE
		case else
			assert( FALSE )
		end select

	case KW_CHAR
		if( shortmods or longmods ) then
			tkOops( x, "SHORT|LONG modifiers used with CHAR type" )
		end if

		'' SIGNED|UNSIGNED CHAR becomes BYTE|UBYTE,
		'' but plain CHAR probably means ZSTRING
		if( signedmods > 0 ) then
			dtype = TYPE_BYTE
		elseif( unsignedmods > 0 ) then
			dtype = TYPE_UBYTE
		else
			dtype = TYPE_ZSTRING
		end if

	case else
		'' Base type is "int" (either explicitly given, or implied
		'' because no other base type was given). Any modifiers are
		'' just added on top of that.
		if( shortmods = 1 ) then
			dtype = iif( unsignedmods > 0, TYPE_USHORT, TYPE_SHORT )
		elseif( longmods = 1 ) then
			dtype = iif( unsignedmods > 0, TYPE_CULONG, TYPE_CLONG )
		elseif( longmods = 2 ) then
			dtype = iif( unsignedmods > 0, TYPE_ULONGINT, TYPE_LONGINT )
		elseif( tkGet( basetypex ) = KW_INT ) then
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
			tkOopsExpected( x, iif( decl = DECL_CAST, _
					@"a data type in this '(...)' type cast", _
					@"a data type at the beginning of this declaration" ) )
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
		'' An identifier must exist, except for parameters/casts, and
		'' in fact for casts there mustn't be an id.
		dim as string id
		if( decl <> DECL_CAST ) then
			if( tkGet( x ) = TK_ID ) then
				id = *tkGetText( x )
				x += 1
			else
				if( decl <> DECL_PARAM ) then
					tkOopsExpected( x, "identifier for the symbol declared in this declaration" )
				end if
			end if
		end if

		static as integer decl_to_astclass(0 to DECL__COUNT-1) = _
		{ _
			ASTCLASS_VAR    , _ '' DECL_VAR
			ASTCLASS_VAR    , _ '' DECL_EXTERNVAR
			ASTCLASS_VAR    , _ '' DECL_STATICVAR
			ASTCLASS_FIELD  , _ '' DECL_FIELD
			ASTCLASS_PARAM  , _ '' DECL_PARAM
			ASTCLASS_TYPEDEF, _ '' DECL_TYPEDEF
			ASTCLASS_UOP      _ '' DECL_CAST
		}

		t = astNew( decl_to_astclass(decl), id )

		select case( decl )
		case DECL_EXTERNVAR : t->attrib or= ASTATTRIB_EXTERN
		case DECL_STATICVAR : t->attrib or= ASTATTRIB_PRIVATE
		case DECL_CAST      : t->op = ASTOP_CAST : assert( t->class = ASTCLASS_UOP )
		end select

		astSetType( t, dtype, basesubtype )
	end if

	node = t

	select case( tkGet( x ) )
	'' ('[' ArrayElements ']')*
	case TK_LBRACKET
		'' Can't allow arrays on everything - currently, it's only
		'' handled for vars/fields/params
		select case( decl )
		case DECL_VAR, DECL_EXTERNVAR, DECL_STATICVAR, _
		     DECL_FIELD, DECL_PARAM

		case else
			tkOops( x, "TODO: arrays not supported here yet" )
		end select

		assert( node->array = NULL )
		node->array = astNew( ASTCLASS_ARRAY )

		'' For each array dimension...
		do
			'' '['
			x += 1

			var elements = hExpr( x, FALSE, NULL )

			'' Add new DIMENSION to the ARRAY:
			'' lbound = 0, ubound = elements - 1
			astAppend( node->array, _
				astNewDIMENSION( astNewCONST( 0, 0, TYPE_LONG ), _
					astNewBOP( ASTOP_SUB, _
						elements, _
						astNewCONST( 1, 0, TYPE_LONG ) ) ) )

			'' ']'
			tkExpect( x, TK_RBRACKET, "to close this array dimension declaration" )
			x += 1

			'' '['? (next dimension)
		loop while( tkGet( x ) = TK_LBRACKET )

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
			astAppend( node, cParamDeclList( x ) )
		end if

		'' ')'
		tkExpect( x, TK_RPAREN, "to close parameter list in function declaration" )
		x += 1
	end select

	'' __ATTRIBUTE__((...))
	var endgccattribs = 0
	cGccAttributeList( x, endgccattribs )

	if( decl = DECL_PARAM ) then
		'' ['=' Initializer]
		if( hMatch( x, TK_EQ ) ) then
			assert( node->expr = NULL )
			node->expr = hExpr( x, FALSE, NULL )
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
		'' except for parameters/casts.
		select case( decl )
		case DECL_PARAM, DECL_CAST
			exit do
		end select

		'' ','?
	loop while( hMatch( x, TK_COMMA ) )

	'' Everything except parameters/casts must end with a ';'
	select case( decl )
	case DECL_PARAM, DECL_CAST
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

	comment += tkCollectComments( begin, x - 1 )

	'' IdList
	function = cIdList( x, decl, dtype, subtype, gccattribs, comment )
	astDelete( subtype )
end function

private function cToplevel _
	( _
		byref x as integer, _
		byval body as integer _
	) as ASTNODE ptr

	var group = astNewGROUP( )

	do
		dim as ASTNODE ptr t

		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_PPINCLUDE
			t = astNew( ASTCLASS_PPINCLUDE, tkGetText( x ) )
			astAddComment( t, tkCollectComments( x, x ) )
			x += 1

		case TK_PPDEFINE
			t = tkGetAst( x )

			'' Did the PP mark this #define for removal? Then just
			'' skip it (and its TK_BEGIN/END body) instead of
			'' parsing it into an AST...
			if( t->attrib and ASTATTRIB_REMOVE ) then
				x += 1

				assert( tkGet( x ) = TK_BEGIN )
				x += 1

				while( tkGet( x ) <> TK_END )
					x += 1
				wend

				assert( tkGet( x ) = TK_END )
				x += 1

				t = NULL
				exit select
			end if

			t = astClone( t )
			astAddComment( t, tkCollectComments( x, x ) )
			x += 1

			'' Macro body should still be enclosed in TK_BEGIN/END
			assert( t->expr = NULL )

			assert( tkGet( x ) = TK_BEGIN )
			x += 1

			dim as ASTNODE ptr expr

			'' Macro body empty?
			if( tkGet( x ) = TK_END ) then
				expr = NULL
			else
				expr = hExpr( x, TRUE, t )

				'' Must have reached the TK_END
				if( tkGet( x ) <> TK_END ) then
					tkOops( x, "couldn't parse #define body as expression" )
				end if
			end if

			assert( tkGet( x ) = TK_END )
			x += 1

			astDelete( t->expr )
			t->expr = expr

		case TK_PPUNDEF
			t = astNew( ASTCLASS_PPUNDEF, tkGetText( x ) )
			astAddComment( t, tkCollectComments( x, x ) )
			x += 1

		case TK_PPIF, TK_PPELSEIF
			t = astNew( iif( tkGet( x ) = TK_PPIF, _
					ASTCLASS_PPIF, ASTCLASS_PPELSEIF ) )
			t->expr = astClone( tkGetAst( x ) )
			astAddComment( t, tkCollectComments( x, x ) )
			x += 1

		case TK_PPELSE, TK_PPENDIF
			t = astNew( iif( tkGet( x ) = TK_PPELSE, _
					ASTCLASS_PPELSE, ASTCLASS_PPENDIF ) )
			astAddComment( t, tkCollectComments( x, x ) )
			x += 1

		case TK_DIVIDER
			t = astNew( ASTCLASS_DIVIDER, tkGetText( x ) )
			astAddComment( t, tkCollectComments( x, x ) )
			x += 1

		'' TYPEDEF BaseType IdList ';'
		'' TYPEDEF STRUCT|UNION|ENUM [Identifier] '{' StructBody '}' IdList ';'
		case KW_TYPEDEF
			if( body = BODY_ENUM ) then
				t = cEnumConst( x )
				exit select
			end if

			var y = x + 1

			'' STRUCT|UNION|ENUM
			select case( tkGet( y ) )
			case KW_STRUCT, KW_UNION, KW_ENUM
				y += 1

				'' [Identifier]
				if( tkGet( y ) = TK_ID ) then
					y += 1
				end if

				'' '{'?
				if( tkGet( y ) = TK_LBRACE ) then
					t = cStructCompound( x )
				else
					t = cTypedef( x )
				end if
			case else
				t = cTypedef( x )
			end select

		'' STRUCT|UNION|ENUM [Identifier] ';'
		'' STRUCT|UNION|ENUM [Identifier] '{' StructBody '}' ';'
		'' STRUCT|UNION|ENUM Identifier MultDecl
		case KW_STRUCT, KW_UNION, KW_ENUM
			if( body = BODY_ENUM ) then
				t = cEnumConst( x )
				exit select
			end if

			'' (Identifier ';')?
			if( (tkGet( x + 1 ) = TK_ID) and _
			    (tkGet( x + 2 ) = TK_SEMI) ) then
				var astclass = ASTCLASS_STRUCTFWD
				select case( tkGet( x ) )
				case KW_UNION
					astclass = ASTCLASS_UNIONFWD
				case KW_ENUM
					astclass = ASTCLASS_ENUMFWD
				case else
					assert( tkGet( x ) = KW_STRUCT )
				end select
				t = astNew( astclass, tkGetText( x + 1 ) )
				x += 3
			elseif( (tkGet( x + 1 ) = TK_LBRACE) or _
			        ((tkGet( x + 1 ) = TK_ID) and _
			         (tkGet( x + 2 ) = TK_LBRACE)) ) then
				t = cStructCompound( x )
			else
				t = cMultDecl( x, iif( body = BODY_STRUCT, DECL_FIELD, DECL_VAR ), 0, "" )
			end if

		'' ';'
		case TK_SEMI
			if( body = BODY_ENUM ) then
				t = cEnumConst( x )
				exit select
			end if

			'' Just ignore single semi-colons
			x += 1

		'' '}'
		case TK_RBRACE
			select case( body )
			case BODY_STRUCT, BODY_ENUM
				exit do
			end select

			tkOopsExpected( x, "a toplevel declaration, not the end of a block" )

		case else
			select case( body )
			case BODY_STRUCT
				t = cMultDecl( x, DECL_FIELD, 0, "" )
			case BODY_ENUM
				t = cEnumConst( x )
			case else
				var begin = x

				'' Global variable/procedure declarations
				''    GccAttributeList [EXTERN|STATIC] MultDecl
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

				t = cMultDecl( x, decl, gccattribs, comment )
			end select
		end select

		astAppend( group, t )
	loop

	function = group
end function

function cFile( ) as ASTNODE ptr
	function = cToplevel( 0, BODY_TOPLEVEL )
end function

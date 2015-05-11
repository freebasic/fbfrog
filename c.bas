''
'' C parsing
''
'' * parses the content of the tk buffer and builds an AST
'' * parsing one "construct" (declaration/statement) at a time
'' * able to recover from parsing errors, by skipping the current construct and
''   continuing to the next one. Bad constructs are stored in form
''   ASTCLASS_UNKNOWN ASTNODEs. Even if all C constructs and gcc extensions
''   would be supported, this would still be needed for complex #define bodies.
'' * Recursive declaration parser: can handle multiple declarations in the same
''   statement and nested declarations such as function pointers returning
''   function pointers etc.
'' * Expression parser: used for variable/parameter initializers, enum
''   constants, #define bodies.
'' * Data type parser: parses base types in declarations including
''   struct/union/enum tags with or without body, type casts, and sizeof()
''
'' Declarations of the form "type a, b, c;" are split up into separate
'' declarations, instead of being preserved as-is. This way they're easier
'' to store in the AST. Technically this isn't always needed, since FB also
'' supports declaring multiple symbols in one statement, but FB is less
'' flexible than C. In C, declarations can declare multiple kinds of symbols
'' (e.g. function and variable) and/or symbols with different data types.
'' For example:
''         extern int a, b, *c, d(void);
''     =>
''         extern as long a, b
''         extern as long ptr c
''         declare function d() as long
'' a & b are same kind and data type, so they can be combined in FB, but
'' c & d can only be declared separately. Thus it makes sense to represent
'' them as separate ASTNODEs.
''
'' Inline struct/union/enum bodies are separated from the declaration, and
'' the declaration is changed to reference the UDT by name instead, because
'' FB does not support inline UDTs. For example:
''         typedef struct { ... } T;
''     =>
''         struct temp { ... };
''         typedef struct temp T;
''     =>
''         type temp : ... : end type
''         type T as temp
'' Another reason for doing this: inline UDTs in declarations declaring
'' multiple symbols. For example:
''         typedef struct { ... } A, *PA;
''     =>
''         struct temp { ... };
''         typedef struct temp A;
''         typedef struct temp *PA;
'' The declaration with multiple symbols will be split up, and the inlined
'' UDT must be extracted, otherwise it would have to be duplicated too.
''

#include once "fbfrog.bi"

declare function cExpression(byval allow_toplevel_comma as integer) as ASTNODE ptr
declare function cExpressionOrInitializer() as ASTNODE ptr
declare function cDataType() as ASTNODE ptr
declare function cDeclaration(byval astclass as integer, byval gccattribs as integer) as ASTNODE ptr
declare function cScope() as ASTNODE ptr
declare function cConstruct(byval bodyastclass as integer) as ASTNODE ptr
declare function cBody(byval bodyastclass as integer) as ASTNODE ptr

namespace c
	dim shared api as ApiInfo ptr
	dim shared as integer x, parseok, tempids
	dim shared parentdefine as ASTNODE ptr

	dim shared typedefs as THASH

	'' #pragma pack stack
	namespace pragmapack
		const MAXLEVEL = 128
		dim shared stack(0 to MAXLEVEL-1) as integer
		dim shared level as integer
	end namespace

	type DEFBODYNODE
		xdefbegin	as integer  '' Begin of the #define
		xbodybegin	as integer  '' Begin of the #define's body
		n		as ASTNODE ptr  '' #define node
	end type
	dim shared defbodies as DEFBODYNODE ptr
	dim shared as integer defbodycount, defbodyroom

	'' Used by hUpdateTypeNameReferences
	dim shared as string oldid, newid
end namespace

private function cMatch(byval tk as integer) as integer
	if tkGet(c.x) = tk then
		c.x += 1
		function = TRUE
	end if
end function

private sub cError(byval message as zstring ptr)
	if c.parseok then
		c.parseok = FALSE
		if frog.verbose then
			print tkReport(c.x, message)
		end if
	end if
end sub

private sub cExpectMatch(byval tk as integer, byval message as zstring ptr)
	if tkGet(c.x) = tk then
		c.x += 1
	elseif c.parseok then
		c.parseok = FALSE
		if frog.verbose then
			print tkReport(c.x, tkMakeExpectedMessage(c.x, tkInfoPretty(tk) + " " + *message))
		end if
	end if
end sub

#define cIsInsideDefineBody() (c.parentdefine <> NULL)

private sub cResetPragmaPack()
	c.pragmapack.stack(c.pragmapack.level) = 0
end sub

private function cIsTypedef(byval id as zstring ptr) as integer
	'' 1. Check typedefs seen by C parser
	if hashContains(@c.typedefs, id, hashHash(id)) then
		return TRUE
	end if

	'' 2. Check -typedefhint options
	function = hashContains(@c.api->idopt(OPT_TYPEDEFHINT), id, hashHash(id))
end function

private function cIdentifierIsMacroParam(byval id as zstring ptr) as integer
	if c.parentdefine then
		function = (astLookupMacroParam(c.parentdefine, id) >= 0)
	else
		function = FALSE
	end if
end function

sub cInit(byref api as ApiInfo)
	c.api = @api
	c.x = 0
	c.parseok = TRUE
	c.parentdefine = NULL
	c.tempids = 0

	hashInit(@c.typedefs, 8, FALSE)

	'' Initially no packing
	c.pragmapack.level = 0
	cResetPragmaPack()

	c.defbodies = NULL
	c.defbodycount = 0
	c.defbodyroom = 0
end sub

sub cEnd()
	deallocate(c.defbodies)
	hashEnd(@c.typedefs)
end sub

#define cAddTypedef(id) hashAddOverwrite(@c.typedefs, id, NULL)

private sub cAddDefBody(byval xdefbegin as integer, byval xbodybegin as integer, byval n as ASTNODE ptr)
	if c.defbodyroom = c.defbodycount then
		if c.defbodyroom = 0 then
			c.defbodyroom = 512
		else
			c.defbodyroom *= 2
		end if
		c.defbodies = reallocate(c.defbodies, c.defbodyroom * sizeof(*c.defbodies))
	end if
	with c.defbodies[c.defbodycount]
		.xdefbegin = xdefbegin
		.xbodybegin = xbodybegin
		.n = n
	end with
	c.defbodycount += 1
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function cLiteral(byval astclass as integer, byval eval_escapes as integer) as ASTNODE ptr
	dim errmsg as string
	dim n as ASTNODE ptr

	if astclass = ASTCLASS_CONSTI then
		n = hNumberLiteral(c.x, FALSE, errmsg)
	else
		n = hStringLiteral(c.x, eval_escapes, errmsg)
	end if

	if n = NULL then
		cError(errmsg)

		select case astclass
		case ASTCLASS_CONSTI
			n = astNew(ASTCLASS_CONSTI, "0")
			n->dtype = TYPE_LONG
		case ASTCLASS_STRING
			n = astNew(ASTCLASS_STRING, "abc")
			n->dtype = TYPE_ZSTRING
		case ASTCLASS_CHAR
			n = astNew(ASTCLASS_CHAR, "0")
			n->dtype = TYPE_BYTE
		end select
	end if

	c.x += 1
	function = n
end function

'' ("..." | [#]id)*
private function cStringLiteralSequence() as ASTNODE ptr
	var strcat = astNew(ASTCLASS_STRCAT)

	while c.parseok
		select case tkGet(c.x)
		case TK_ID
			astAppend(strcat, astNewTEXT(tkSpellId(c.x)))
			c.x += 1

		case TK_STRING, TK_WSTRING
			astAppend(strcat, cLiteral(ASTCLASS_STRING, TRUE))

		'' '#' stringify operator
		case TK_HASH
			'' #id?
			if tkGet(c.x + 1) <> TK_ID then
				exit while
			end if
			c.x += 1

			astAppend(strcat, astNew(ASTCLASS_STRINGIFY, astNewTEXT(tkGetText(c.x))))
			c.x += 1

		case else
			exit while
		end select
	wend

	function = strcat
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
private function hIsDataType(byval y as integer) as integer
	var is_type = FALSE

	select case tkGet(y)
	case KW_SIGNED, KW_UNSIGNED, KW_CONST, KW_SHORT, KW_LONG, _
	     KW_ENUM, KW_STRUCT, KW_UNION, _
	     KW_VOID, KW_CHAR, KW_FLOAT, KW_DOUBLE, KW_INT
		is_type = not cIdentifierIsMacroParam(tkSpellId(y))
	case TK_ID
		var id = tkSpellId(y)
		if (extradatatypesLookup(id) <> TYPE_NONE) or cIsTypedef(id) then
			is_type = not cIdentifierIsMacroParam(id)
		end if
	end select

	function = is_type
end function

private function hIsDataTypeOrAttribute(byval y as integer) as integer
	select case tkGet(y)
	case KW___ATTRIBUTE, KW___ATTRIBUTE__
		function = not cIdentifierIsMacroParam(tkSpellId(y))
	case else
		function = hIsDataType(y)
	end select
end function

'' C expression parser based on precedence climbing
private function hExpression _
	( _
		byval level as integer, _
		byval parentheses as integer, _
		byval allow_toplevel_comma as integer _
	) as ASTNODE ptr

	'' Unary prefix operators
	var op = -1
	select case tkGet(c.x)
	case TK_EXCL   : op = ASTCLASS_CLOGNOT   '' !
	case TK_TILDE  : op = ASTCLASS_NOT       '' ~
	case TK_MINUS  : op = ASTCLASS_NEGATE    '' -
	case TK_PLUS   : op = ASTCLASS_UNARYPLUS '' +
	case TK_AMP    : op = ASTCLASS_ADDROF    '' &
	case TK_STAR   : op = ASTCLASS_DEREF     '' *
	end select

	dim as ASTNODE ptr a
	if op >= 0 then
		c.x += 1
		a = astNew(op, hExpression(cprecedence(op), parentheses, allow_toplevel_comma))
	else
		'' Atoms
		select case tkGet(c.x)

		''     '(' Expression ')'
		'' or: '(' DataType ')' Expression
		case TK_LPAREN
			'' '('
			c.x += 1

			var is_cast = hIsDataTypeOrAttribute(c.x)

			'' Find the ')' and check the token behind it, in some cases
			'' we can tell that it probably isn't a cast.
			var closingparen = hFindClosingParen(c.x - 1, cIsInsideDefineBody(), FALSE)
			select case tkGet(closingparen + 1)
			case TK_RPAREN, TK_EOF, TK_EOL
				is_cast = FALSE
			end select

			'' Something of the form '(id*)' or just in general a
			'' '*' in front of the closing ')'? It most likely is a pointer cast.
			is_cast or= (tkGet(closingparen - 1) = TK_STAR)

			if is_cast then
				'' DataType
				var t = cDataType()

				'' ')'
				cExpectMatch(TK_RPAREN, "behind the data type")

				'' Expression
				a = hExpression(cprecedence(ASTCLASS_CAST), parentheses, allow_toplevel_comma)

				assert(t->class = ASTCLASS_DATATYPE)
				a = astNew(ASTCLASS_CAST, a)
				astSetType(a, t->dtype, t->subtype)
				astDelete(t)
			else
				'' Expression
				a = hExpression(0, parentheses + 1, allow_toplevel_comma)

				'' ')'
				cExpectMatch(TK_RPAREN, "to close '(...)' parenthesized expression")

				if astIsTEXT(a) then
					if cIdentifierIsMacroParam(a->text) then
						a->attrib or= ASTATTRIB_PARENTHESIZEDMACROPARAM
					end if
				end if
			end if

		case TK_NUMBER
			a = cLiteral(ASTCLASS_CONSTI, TRUE)

		case TK_STRING, TK_WSTRING, TK_HASH
			a = cStringLiteralSequence()

		case TK_CHAR, TK_WCHAR
			a = cLiteral(ASTCLASS_CHAR, TRUE)

		'' Id
		'' Id ()
		'' Id (CallArgs)
		'' Id ## Id ## ...
		'' Id ("String"|Id)*
		case TK_ID
			select case tkGet(c.x + 1)
			'' '('?
			case TK_LPAREN
				a = astNew(ASTCLASS_CALL, tkSpellId(c.x))
				c.x += 2

				'' [CallArguments]
				if tkGet(c.x) <> TK_RPAREN then
					'' Expression (',' Expression)*
					do
						astAppend(a, cExpression(FALSE))

						'' ','?
					loop while cMatch(TK_COMMA) and c.parseok
				end if

				'' ')'?
				cExpectMatch(TK_RPAREN, "to close call argument list")

			'' '##'?
			case TK_HASHHASH
				a = astNew(ASTCLASS_PPMERGE)
				astAppend(a, astNewTEXT(tkSpellId(c.x)))
				c.x += 2

				'' Identifier ('##' Identifier)*
				do
					'' Identifier?
					if tkGet(c.x) = TK_ID then
						astAppend(a, astNewTEXT(tkSpellId(c.x)))
						c.x += 1
					else
						cError("expected identifier as operand of '##' PP merge operator" + tkButFound(c.x))
					end if

					'' '##'?
				loop while cMatch(TK_HASHHASH) and c.parseok

			case TK_ID, TK_STRING, TK_WSTRING, TK_HASH
				a = cStringLiteralSequence()

			case else
				a = astNewTEXT(tkSpellId(c.x))
				c.x += 1
			end select

		'' SIZEOF Expression
		'' SIZEOF '(' DataType ')'
		case KW_SIZEOF
			c.x += 1

			'' ('(' DataType)?
			if (tkGet(c.x) = TK_LPAREN) andalso hIsDataTypeOrAttribute(c.x + 1) then
				'' '('
				c.x += 1

				'' DataType
				a = cDataType()

				'' ')'
				cExpectMatch(TK_RPAREN, "behind the data type")
			else
				a = hExpression(cprecedence(ASTCLASS_SIZEOF), parentheses + 1, allow_toplevel_comma)
			end if
			a = astNew(ASTCLASS_SIZEOF, a)

		'' DEFINED ['('] Identifier [')']
		case KW_DEFINED
			c.x += 1

			'' '('
			var have_parens = cMatch(TK_LPAREN)

			'' Identifier
			dim as string id
			if tkGet(c.x) = TK_ID then
				id = *tkSpellId(c.x)
			else
				cError("expected identifier" + tkButFound(c.x))
				id = "<error-recovery>"
			end if
			a = astNew(ASTCLASS_CDEFINED, id)
			c.x += 1

			if have_parens then
				'' ')'
				cExpectMatch(TK_RPAREN, "to finish defined(...) expression")
			end if

		case else
			cError("expected expression" + tkButFound(c.x))
			a = astNew(ASTCLASS_CONSTI, "0")
			a->dtype = TYPE_INTEGER
		end select
	end if

	'' Infix operators
	while c.parseok
		select case as const tkGet(c.x)
		case TK_QUEST    : op = ASTCLASS_IIF     '' ? (a ? b : c)
		case TK_PIPEPIPE : op = ASTCLASS_CLOGOR  '' ||
		case TK_AMPAMP   : op = ASTCLASS_CLOGAND '' &&
		case TK_PIPE     : op = ASTCLASS_OR      '' |
		case TK_CIRC     : op = ASTCLASS_XOR     '' ^
		case TK_AMP      : op = ASTCLASS_AND     '' &
		case TK_EQ       : op = ASTCLASS_CASSIGN '' =
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
		case TK_LBRACKET : op = ASTCLASS_INDEX   '' [ ... ]
		case TK_DOT      : op = ASTCLASS_MEMBER  '' .
		case TK_ARROW    : op = ASTCLASS_MEMBERDEREF '' ->
		case TK_COMMA  '' ,
			if (parentheses = 0) and (not allow_toplevel_comma) then
				exit while
			end if
			op = ASTCLASS_CCOMMA
		case else
			exit while
		end select

		'' Higher/same level means process now (takes precedence),
		'' lower level means we're done and the parent call will
		'' continue. The first call will start with level 0.
		var oplevel = cprecedence(op)
		if oplevel < level then
			exit while
		end if

		select case op
		case ASTCLASS_IIF, ASTCLASS_CASSIGN
			'' Right associative

		case else
			'' Left associative
			oplevel += 1
		end select

		'' For [] we parse until the ], no precedence levels needed
		if op = ASTCLASS_INDEX then
			oplevel = 0
		end if

		'' operator
		c.x += 1

		'' rhs
		var b = hExpression(oplevel, parentheses + iif(op = ASTCLASS_INDEX, 1, 0), allow_toplevel_comma)

		'' Handle ?: special case
		if op = ASTCLASS_IIF then
			'' ':'
			cExpectMatch(TK_COLON, "for a?b:c iif operator")

			a = astNewIIF(a, b, hExpression(oplevel, parentheses, allow_toplevel_comma))
		else
			'' Handle [] special case
			if op = ASTCLASS_INDEX then
				'' ']'
				cExpectMatch(TK_RBRACKET, "for [] indexing operator")
			end if

			a = astNew(op, a, b)
		end if
	wend

	function = a
end function

private function cExpression(byval allow_toplevel_comma as integer) as ASTNODE ptr
	function = hExpression(0, 0, allow_toplevel_comma)
end function

'' Initializer:
'' '{' ExpressionOrInitializer (',' ExpressionOrInitializer)* [','] '}'
private function cInitializer() as ASTNODE ptr
	'' '{'
	assert(tkGet(c.x) = TK_LBRACE)
	c.x += 1

	var a = astNew(ASTCLASS_STRUCTINIT)

	do
		'' '}'?
		if tkGet(c.x) = TK_RBRACE then exit do

		astAppend(a, cExpressionOrInitializer())

		'' ','
	loop while cMatch(TK_COMMA) and c.parseok

	cExpectMatch(TK_RBRACE, "to close initializer")

	function = a
end function

private function cExpressionOrInitializer() as ASTNODE ptr
	'' '{'?
	if tkGet(c.x) = TK_LBRACE then
		function = cInitializer()
	else
		function = cExpression(FALSE)
	end if
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private sub cSkipToRparen()
	do
		select case tkGet(c.x)
		case TK_LPAREN, TK_LBRACKET, TK_LBRACE
			c.x = hFindClosingParen(c.x, cIsInsideDefineBody(), TRUE)
		case TK_RPAREN, TK_EOF
			exit do
		end select
		c.x += 1
	loop
end sub

private sub cGccAttribute(byref gccattribs as integer)
	if tkGet(c.x) < TK_ID then
		cError("expected attribute identifier inside __attribute__((...))")
		exit sub
	end if

	var attr = *tkSpellId(c.x)

	'' Each attribute can be given as foo or __foo__ -- normalize to foo.
	if (left(attr, 2) = "__") and (right(attr, 2) = "__") then
		attr = mid(attr, 3, len(attr) - 4)
	end if

	'' Most attributes aren't interesting for FB bindings and should be ignored,
	'' the main exception being the x86 calling conventions.
	select case attr
	case "alloc_size", _
	     "aligned", _
	     "always_inline", _
	     "cold", _
	     "const", _
	     "deprecated", _
	     "format", _
	     "format_arg", _
	     "gnu_inline", _
	     "malloc", _
	     "may_alias", _
	     "no_instrument_function", _
	     "noreturn", _
	     "nothrow", _
	     "pure", _
	     "sentinel", _
	     "unused", _
	     "visibility", _
	     "warn_unused_result", _
	     "dllexport"  '' used on declarations in some headers (SDL 1.2), looks like gcc ignores it
		c.x += 1

		'' Some of these attributes accept further arguments which we
		'' can just ignore.
		cSkipToRparen()

	case "cdecl"     : gccattribs or= ASTATTRIB_CDECL     : c.x += 1
	case "stdcall"   : gccattribs or= ASTATTRIB_STDCALL   : c.x += 1
	case "packed"    : gccattribs or= ASTATTRIB_PACKED    : c.x += 1
	case "dllimport" : gccattribs or= ASTATTRIB_DLLIMPORT : c.x += 1
	case else
		cError("unknown attribute '" + *tkSpellId(c.x) + "'")
	end select
end sub

private sub cGccAttributeList(byref gccattribs as integer)
	while c.parseok
		select case tkGet(c.x)
		case KW_VOLATILE, KW_INLINE, KW___INLINE, KW___INLINE__
			c.x += 1

		'' __attribute__((...)):
		'' __ATTRIBUTE__ '((' Attribute (',' Attribute)* '))'
		case KW___ATTRIBUTE, KW___ATTRIBUTE__
			c.x += 1

			'' '('?
			cExpectMatch(TK_LPAREN, "as 1st '(' in '__attribute__((...))'")

			'' '('?
			cExpectMatch(TK_LPAREN, "as 2nd '(' in '__attribute__((...))'")

			'' Attribute (',' Attribute)*
			do
				'' ')'?
				if tkGet(c.x) = TK_RPAREN then exit do

				'' Attribute
				cGccAttribute(gccattribs)

				'' ','?
			loop while cMatch(TK_COMMA) and c.parseok

			'' ')'?
			cExpectMatch(TK_RPAREN, "as 1st ')' in '__attribute__((...))'")

			'' ')'?
			cExpectMatch(TK_RPAREN, "as 2nd ')' in '__attribute__((...))'")

		case else
			exit while
		end select
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Enum constant: Identifier ['=' Expression] (',' | '}')
private function cEnumConst() as ASTNODE ptr
	'' Identifier
	if tkGet(c.x) <> TK_ID then
		cError("expected identifier for an enum constant" + tkButFound(c.x))
		exit function
	end if
	var enumconst = astNew(ASTCLASS_CONST, tkSpellId(c.x))
	enumconst->attrib or= ASTATTRIB_ENUMCONST
	c.x += 1

	'' '='?
	if cMatch(TK_EQ) then
		'' Expression
		enumconst->expr = cExpression(FALSE)
	end if

	'' (',' | '}')
	select case tkGet(c.x)
	case TK_COMMA
		c.x += 1

	case TK_RBRACE

	case else
		cError("expected ',' or '}' behind enum constant" + tkButFound(c.x))
	end select

	function = enumconst
end function

'' {STRUCT|UNION|ENUM} [Identifier] '{' StructBody|EnumBody '}'
'' {STRUCT|UNION|ENUM} Identifier
private function cTag() as ASTNODE ptr
	'' {STRUCT|UNION|ENUM}
	dim as integer astclass
	select case tkGet(c.x)
	case KW_UNION
		astclass = ASTCLASS_UNION
	case KW_ENUM
		astclass = ASTCLASS_ENUM
	case else
		assert(tkGet(c.x) = KW_STRUCT)
		astclass = ASTCLASS_STRUCT
	end select
	c.x += 1

	'' __attribute__((...))
	dim gccattrib as integer
	cGccAttributeList(gccattrib)

	'' [Identifier]
	dim tagid as zstring ptr
	if tkGet(c.x) = TK_ID then
		tagid = tkSpellId(c.x)
		c.x += 1
	end if

	'' '{'?
	if tkGet(c.x) = TK_LBRACE then
		var udt = astNew(astclass, tagid)
		udt->attrib or= gccattrib

		select case astclass
		case ASTCLASS_STRUCT, ASTCLASS_UNION
			var maxalign = c.pragmapack.stack(c.pragmapack.level)

			'' Preserve alignment if needed so we can emit FIELD = N,
			'' but not if N >= 8, because FB has no alignment > 8,
			'' so FIELD >= 8 is useless. Omitting it improves merging
			'' for some bindings.
			if (maxalign > 0) and (maxalign < 8) then
				udt->maxalign = maxalign
			end if
		end select

		'' '{'
		c.x += 1

		'' Parse struct/union/enum body
		astAppend(udt, cBody(astclass))

		'' '}'
		cExpectMatch(TK_RBRACE, "to close " + astDumpPrettyDecl(udt) + " block")

		'' __attribute__((...))
		cGccAttributeList(udt->attrib)

		function = udt
	else
		'' It's just a type name, not an UDT body - can't be anonymous
		if tagid = NULL then
			cError("expected '{' or tag name" + tkButFound(c.x))
			tagid = @"<error-recovery>"
		end if

		var n = astNewTEXT(tagid)
		n->attrib or= ASTATTRIB_TAGID
		function = n
	end if
end function

private function cTypedef() as ASTNODE ptr
	'' TYPEDEF
	c.x += 1
	function = cDeclaration(ASTCLASS_TYPEDEF, 0)
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private sub hTurnIntoUNKNOWN(byval n as ASTNODE ptr, byval first as integer, byval last as integer)
	n->class = ASTCLASS_UNKNOWN
	astSetText(n, tkSpell(first, last))
	astRemoveChildren(n)
	astDelete(n->expr)
	n->expr = NULL
	astSetType(n, TYPE_NONE, NULL)
end sub

'' Toplevel comma operators can be translated to a sequence of
'' statements; the decision about how to handle the return value (if
'' any) needs to be done manually anyways.
''
''    (a, b, c)
'' =>
''    scope
''        a
''        b
''        [return?] c
''    end scope
''
private function hUnwrapToplevelCommas(byval n as ASTNODE ptr) as ASTNODE ptr
	if n->class = ASTCLASS_CCOMMA then
		var l = n->head
		var r = n->tail
		astUnlink(n, l)
		astUnlink(n, r)
		astDelete(n)
		function = astNewGROUP(hUnwrapToplevelCommas(l), hUnwrapToplevelCommas(r))
	else
		function = n
	end if
end function

private sub hHandleToplevelAssign(byval n as ASTNODE ptr)
	'' C assignment expression is top-most expression?
	'' Can be translated to FB assignment statement easily.
	'' (unlike C assignments nested deeper in expressions etc.)
	if n->class = ASTCLASS_CASSIGN then
		n->class = ASTCLASS_ASSIGN
	end if
end sub

''
'' Turn toplevel C comma/assignment expressions into FB statements
''
'' If it's inside a macro body, then we wrap it in a scope block, to enforce its
'' use as statement, not expression. (otherwise, for example, assignments could
'' be mis-used as comparisons)
''
private function hHandleCommasAssigns(byval n as ASTNODE ptr, byval is_macrobody as integer) as ASTNODE ptr
	''
	'' Commas
	''
	'' first, in case they contain assignments that will become toplevel
	'' ones after unwrapping...
	''
	n = hUnwrapToplevelCommas(n)

	'' Any other commas?
	if astContains(n, ASTCLASS_CCOMMA) then
		cError("can't auto-translate C comma operator here [yet]")
	end if

	''
	'' Assignments
	''
	if n->class = ASTCLASS_GROUP then
		var i = n->head
		while i
			hHandleToplevelAssign(i)
			i = i->next
		wend
	else
		hHandleToplevelAssign(n)
	end if

	'' Any other assignments?
	if astContains(n, ASTCLASS_CASSIGN) then
		cError("can't auto-translate C assignment operator here [yet]")
	end if

	if is_macrobody then
		select case n->class
		case ASTCLASS_GROUP, ASTCLASS_ASSIGN
			n = astNew(ASTCLASS_SCOPEBLOCK, n)
		end select
	end if

	function = n
end function

''
'' Determine whether a sequence of tokens starting with '{' is a scope block or
'' an array/struct initializer.
''
'' If there is a ';' behind the first "element" then it surely is a scope block.
'' If there's a ',' instead, then it probably is an initializer. An empty '{}'
'' is treated as initializer. We can't just stop at the first ',' though. In
'' order to support "{ int a, b; }", we have to scan the whole '{...}' block
'' for ';'s.
''
private function hDefineBodyLooksLikeScopeBlock(byval x as integer) as integer
	'' '{'
	assert(tkGet(x) = TK_LBRACE)

	do
		x += 1

		select case tkGet(x)
		case TK_SEMI
			return TRUE

		case TK_EOL, TK_RBRACE
			exit do

		case TK_LPAREN, TK_LBRACKET, TK_LBRACE
			x = hFindClosingParen(x, TRUE, TRUE)
		end select
	loop

	function = FALSE
end function

private function cDefineBodyTokenLiteral() as string
	dim astclass as integer

	select case as const tkGet(c.x)
	case TK_NUMBER             : astclass = ASTCLASS_CONSTI
	case TK_STRING, TK_WSTRING : astclass = ASTCLASS_STRING
	case TK_CHAR, TK_WCHAR     : astclass = ASTCLASS_CHAR
	case else : assert(FALSE)
	end select

	var n = cLiteral(astclass, TRUE)
	function = emitExpr(n)
	astDelete(n)
end function

private function cDefineBodyToken() as string
	select case as const tkGet(c.x)
	case TK_NUMBER, TK_STRING, TK_WSTRING, TK_CHAR, TK_WCHAR
		function = cDefineBodyTokenLiteral()

	case TK_EXCLEQ    : function = "<>"      : c.x += 1  '' !=
	case TK_PERCENT   : function = "mod"     : c.x += 1  '' %
	case TK_PERCENTEQ : function = "mod="    : c.x += 1  '' %=
	case TK_AMP       : function = "and"     : c.x += 1  '' &
	case TK_AMPEQ     : function = "and="    : c.x += 1  '' &=
	case TK_AMPAMP    : function = "andalso" : c.x += 1  '' &&
	case TK_LTLT      : function = "shl"     : c.x += 1  '' <<
	case TK_LTLTEQ    : function = "shl="    : c.x += 1  '' <<=
	case TK_EQEQ      : function = "="       : c.x += 1  '' ==
	case TK_GTGT      : function = "shr"     : c.x += 1  '' >>
	case TK_GTGTEQ    : function = "shr="    : c.x += 1  '' >>=
	case TK_CIRC      : function = "xor"     : c.x += 1  '' ^
	case TK_CIRCEQ    : function = "xor="    : c.x += 1  '' ^=
	case TK_PIPE      : function = "or"      : c.x += 1  '' |
	case TK_PIPEEQ    : function = "or="     : c.x += 1  '' |=
	case TK_PIPEPIPE  : function = "orelse"  : c.x += 1  '' ||
	case TK_TILDE     : function = "not"     : c.x += 1  '' ~

	case else
		function = tkSpell(c.x)
		c.x += 1
	end select
end function

private function hSeparateBySpace(byval l as integer, byval r as integer) as integer
	'' Always add space behind a comma
	if l = TK_COMMA then return TRUE

	'' But not space in front of a comma
	if r = TK_COMMA then return FALSE

	'' No spaces on the inside of '()' or '[]'
	if (l = TK_LPAREN) or (r = TK_RPAREN) then return FALSE
	if (l = TK_LBRACKET) or (r = TK_RBRACKET) then return FALSE

	'' No spaces in front of '(' (function calls)
	if l = TK_RPAREN then return FALSE

	'' No spaces around ## merge operator
	if (l = TK_HASHHASH) or (r = TK_HASHHASH) then return FALSE

	'' No space behind # stringify operator
	if l = TK_HASH then return FALSE

	'' Add space between anything else
	function = TRUE
end function

private function cDefineBodyTokens() as string
	dim s as string

	var begin = c.x
	while tkGet(c.x) <> TK_EOL

		assert(tkGet(c.x) <> TK_EOF)

		if begin < c.x then
			if hSeparateBySpace(tkGet(c.x - 1), tkGet(c.x)) then
				s += " "
			end if
		end if

		s += cDefineBodyToken()
	wend

	function = s
end function

'' Return value: whether to keep the #define
private function cDefineBody(byval macro as ASTNODE ptr) as integer
	'' If -convbodytokens was given for this #define, then don't try to parse the #define body,
	'' but just convert the tokens individually from C to FB.
	if hashContains(@c.api->idopt(OPT_CONVBODYTOKENS), macro->text, hashHash(macro->text)) then
		var body = cDefineBodyTokens()
		if len(body) > 0 then
			macro->expr = astNewTEXT(body)
		end if
		assert(tkGet(c.x) = TK_EOL)
		return TRUE
	end if

	select case tkGet(c.x)
	'' Don't preserve #define if it just contains _Pragma's
	'' _Pragma("...")
	case KW__PRAGMA
		do
			'' _Pragma
			c.x += 1

			'' '('
			if tkGet(c.x) <> TK_LPAREN then exit do
			c.x += 1

			'' Skip to ')' - we don't care whether there is
			'' a string literal or something like a #macroparam or similar...
			cSkipToRparen()
			c.x += 1
		loop while tkGet(c.x) = KW__PRAGMA

		exit function

	case KW___ATTRIBUTE, KW___ATTRIBUTE__
		'' Don't preserve #define if it just contains an __attribute__
		cGccAttributeList(0)
		exit function

	'' '{'
	case TK_LBRACE
		if hDefineBodyLooksLikeScopeBlock(c.x) then
			macro->expr = cScope()
		else
			macro->expr = cInitializer()
		end if
		return TRUE

	'' Just a 'const'? It's common to have a #define for the const keyword
	'' in C headers...
	case KW_CONST
		if tkGet(c.x + 1) = TK_EOL then
			'' const
			c.x += 1
			exit function
		end if
	end select

	if hIsDataTypeOrAttribute(c.x) then
		macro->expr = cDataType()
		return TRUE
	end if

	macro->expr = cExpression(TRUE)
	function = TRUE
end function

private function hDefBodyContainsIds(byval y as integer) as integer
	do
		assert(tkGet(y) <> TK_EOF)
		select case tkGet(y)
		case TK_EOL
			exit do
		case TK_ID
			return TRUE
		end select
		y += 1
	loop
end function

private sub cParseDefBody(byval n as ASTNODE ptr, byval xbegin as integer, byref add_to_ast as integer)
	c.parentdefine = n

	'' Body
	add_to_ast and= cDefineBody(n)

	'' Didn't reach EOL? Then the beginning of the macro body could
	'' be parsed as expression, but not the rest.
	if tkGet(c.x) <> TK_EOL then
		cError("failed to parse full #define body")
		c.x = hSkipToEol(c.x)
	end if

	if n->expr then
		n->expr = hHandleCommasAssigns(n->expr, TRUE)
	end if

	'' If parsing the body failed, turn the PPDEFINE into an UNKNOWN without
	'' reallocating it (as it may already be linked into the AST).
	if c.parseok = FALSE then
		hTurnIntoUNKNOWN(n, xbegin, c.x)
		c.parseok = TRUE
	end if

	c.parentdefine = NULL
end sub

private function cDefine() as ASTNODE ptr
	'' define
	var defbegin = c.x - 1
	c.x += 1

	'' Identifier ['(' ParameterList ')']
	var macro = hDefineHead(c.x)

	'' Body?
	var add_to_ast = TRUE
	assert(macro->expr = NULL)
	if tkGet(c.x) <> TK_EOL then
		if hDefBodyContainsIds(c.x) then
			'' Delay parsing, until we've parsed all declarations in the input.
			'' This way we have more knowledge about typedefs etc. which could
			'' help parsing this #define body.
			cAddDefBody(defbegin, c.x, macro)
			c.x = hSkipToEol(c.x)

			'' cBody() mustn't delete the PPDEFINE node now that
			'' we're referencing it from the c.defbodies list
			assert(c.parseok)
		else
			'' Probably a simple #define body, parse right now
			cParseDefBody(macro, defbegin, add_to_ast)
		end if
	end if

	'' Eol
	assert(tkGet(c.x) = TK_EOL)
	c.x += 1

	if add_to_ast = FALSE then
		astDelete(macro)
		macro = astNewGROUP()
	end if
	function = macro
end function

private function cUndef() as ASTNODE ptr
	'' undef
	c.x += 1

	'' id
	assert(tkGet(c.x) >= TK_ID)
	function = astNew(ASTCLASS_UNDEF, tkSpellId(c.x))
	c.x += 1

	'' Eol
	assert(tkGet(c.x) = TK_EOL)
	c.x += 1
end function

private function cInclude() as ASTNODE ptr
	c.x += 1

	'' "filename" | <filename>
	'' TODO: Don't evaluate escape sequences in "filename"
	'' TODO: Don't evaluate escape sequences/comments in <filename>
	dim filename as string
	if tkGet(c.x) = TK_LT then
		'' <filename>

		'' Skip tokens until the '>'
		var begin = c.x
		do
			c.x += 1
			assert((tkGet(c.x) <> TK_EOL) and (tkGet(c.x) <> TK_EOF))
		loop until tkGet(c.x) = TK_GT

		'' Then spell them to get the filename
		filename = tkSpell(begin + 1, c.x - 1)
		c.x += 1
	else
		'' "filename"
		assert(tkGet(c.x) = TK_STRING)
		var s = cLiteral(ASTCLASS_STRING, FALSE)
		filename = *s->text
		astDelete(s)
	end if

	'' Eol
	assert(tkGet(c.x) = TK_EOL)
	c.x += 1

	function = astNew(ASTCLASS_PPINCLUDE, filename)
end function

private function cPragmaPackNumber() as integer
	var n = cLiteral(ASTCLASS_CONSTI, TRUE)
	if n->class <> ASTCLASS_CONSTI then
		exit function
	end if
	c.pragmapack.stack(c.pragmapack.level) = astEvalConstiAsInt64(n)
	astDelete(n)
	function = TRUE
end function

private function cPragmaPack() as ASTNODE ptr
	'' pack
	assert(tkGet(c.x) = TK_ID)
	assert(tkSpell(c.x) = "pack")
	c.x += 1

	'' '('
	cExpectMatch(TK_LPAREN, "as in '#pragma pack(...)'")

	select case tkGet(c.x)
	'' #pragma pack(N): Set max alignment for current top of stack
	case TK_NUMBER
		if cPragmaPackNumber() = FALSE then
			exit function
		end if

	'' #pragma pack(push, N)
	'' #pragma pack(pop)
	case TK_ID
		select case *tkSpellId(c.x)
		case "push"
			c.pragmapack.level += 1
			if c.pragmapack.level >= c.pragmapack.MAXLEVEL then
				oops("#pragma pack stack too small")
			end if
			cResetPragmaPack()
			c.x += 1

			'' ','
			cExpectMatch(TK_COMMA, "behind 'push'")

			'' 'N'
			if tkGet(c.x) <> TK_NUMBER then
				exit function
			end if
			if cPragmaPackNumber() = FALSE then
				exit function
			end if

		case "pop"
			if c.pragmapack.level > 0 then
				c.pragmapack.level -= 1
			else
				cError("#pragma pack(pop) without previous push")
			end if
			c.x += 1

		case else
			exit function
		end select

	'' #pragma pack(): Reset top of stack to default
	case TK_RPAREN
		cResetPragmaPack()

	case else
		exit function
	end select

	'' ')'
	cExpectMatch(TK_RPAREN, "as in '#pragma pack(...)'")

	'' Eol
	assert(tkGet(c.x) = TK_EOL)
	c.x += 1

	'' Don't preserve the directive
	function = astNewGROUP()
end function

'' #pragma comment(lib, "...")
function cPragmaComment() as ASTNODE ptr
	'' comment
	assert(tkGet(c.x) = TK_ID)
	assert(tkSpell(c.x) = "comment")
	c.x += 1

	'' '('
	assert(tkGet(c.x) = TK_LPAREN)
	c.x += 1

	'' lib
	assert(tkGet(c.x) = TK_ID)
	assert(tkSpell(c.x) = "lib")
	c.x += 1

	'' ','
	assert(tkGet(c.x) = TK_COMMA)
	c.x += 1

	'' "<library-file-name>"
	assert(tkGet(c.x) = TK_STRING)
	dim libname as string
	scope
		var s = cLiteral(ASTCLASS_STRING, TRUE)
		libname = *s->text
		astDelete(s)
	end scope

	'' ')'
	assert(tkGet(c.x) = TK_RPAREN)
	c.x += 1

	assert(tkGet(c.x) = TK_EOL)
	c.x += 1

	''
	'' Turn the #pragma comment(lib, "...") into #inclib "..."
	''
	'' It seems to be common to specify the library's full file name in the
	'' #pragma directive, i.e. "foo.lib". In FB it must be #inclib "foo"
	'' though, no extension or lib prefix. Thus, we need to do some
	'' conversion.
	''
	'' Besides "foo.lib", we also handle "libfoo.a" here which is another
	'' common library file name format. Anything else should probably be
	'' passed through as-is though.
	''

	'' Remove .lib suffix
	if right(libname, 4) = ".lib" then
		libname = left(libname, len(libname) - 4)
	'' Remove lib prefix and .a suffix
	elseif (left(libname, 3) = "lib") and (right(libname, 2) = ".a") then
		libname = right(libname, len(libname) - 3)
		libname = left(libname, len(libname) - 2)
	end if

	function = astNew(ASTCLASS_INCLIB, libname)
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
		byref is_tag as integer _
	)

	dtype = TYPE_NONE
	subtype = NULL
	is_tag = FALSE

	var signedmods = 0
	var unsignedmods = 0
	var constmods = 0
	var shortmods = 0
	var longmods = 0

	''
	'' 1. Parse base type and all modifiers, and count them
	''

	while c.parseok
		'' __ATTRIBUTE__((...))
		cGccAttributeList(gccattribs)

		select case tkGet(c.x)
		case KW_SIGNED
			if unsignedmods > 0 then
				cError("mixed SIGNED with previous UNSIGNED modifier")
			end if
			signedmods += 1

		case KW_UNSIGNED
			if signedmods > 0 then
				cError("mixed UNSIGNED with previous SIGNED modifier")
			end if
			unsignedmods += 1

		case KW_CONST
			constmods += 1

		case KW_SHORT
			if longmods > 0 then
				cError("mixed SHORT with previous LONG modifier")
			end if
			shortmods += 1
			if shortmods > 1 then
				cError("more than 1 SHORT modifier")
			end if

		case KW_LONG
			if shortmods > 0 then
				cError("mixed LONG with previous SHORT modifier")
			end if
			longmods += 1
			if longmods > 2 then
				cError("more than 2 LONG modifiers")
			end if

		case else
			'' Only one base type is allowed
			if dtype <> TYPE_NONE then
				exit while
			end if

			select case tkGet(c.x)
			case KW_ENUM, KW_STRUCT, KW_UNION
				dtype = TYPE_UDT
				subtype = cTag()
				is_tag = TRUE
				c.x -= 1

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
				if signedmods or unsignedmods or longmods or shortmods then
					'' Then don't treat this id as the type
					exit while
				end if

				'' Treat the id as the type
				var id = tkSpellId(c.x)
				dtype = extradatatypesLookup(id)
				if dtype = TYPE_NONE then
					dtype = TYPE_UDT
					subtype = astNewTEXT(id)
				end if

			case KW_VOID   : dtype = TYPE_ANY
			case KW_FLOAT  : dtype = TYPE_SINGLE
			case KW_DOUBLE : dtype = TYPE_DOUBLE
			case KW_CHAR   : dtype = TYPE_ZSTRING
			case KW_INT    : dtype = TYPE_LONG
			case KW__BOOL  : dtype = TYPE_BYTE

			case else
				exit while
			end select
		end select

		c.x += 1
	wend

	'' Some details can only be decided after parsing the whole thing,
	'' because for example "unsigned int" and "int unsigned" both are allowed.
	select case dtype
	case TYPE_DOUBLE
		if longmods = 1 then
			dtype = TYPE_CLONGDOUBLE
		end if

	case TYPE_ZSTRING
		'' SIGNED|UNSIGNED CHAR becomes BYTE|UBYTE,
		'' but plain CHAR probably means ZSTRING
		if signedmods > 0 then
			dtype = TYPE_BYTE
		elseif unsignedmods > 0 then
			dtype = TYPE_UBYTE
		end if

	case TYPE_LONG, TYPE_NONE
		'' Base type is "int" (either explicitly given, or implied
		'' because no other base type was given). Any modifiers are
		'' just added on top of that.
		if shortmods = 1 then
			dtype = iif(unsignedmods > 0, TYPE_USHORT, TYPE_SHORT)
		elseif longmods = 1 then
			if c.api->clong32 then
				'' C long => LONG (ok on Windows where C long is always 32bit)
				dtype = iif(unsignedmods > 0, TYPE_ULONG, TYPE_LONG)
			else
				'' C long => CLONG
				dtype = iif(unsignedmods > 0, TYPE_CULONG, TYPE_CLONG)
			end if
		elseif longmods = 2 then
			dtype = iif(unsignedmods > 0, TYPE_ULONGINT, TYPE_LONGINT)
		elseif dtype = TYPE_LONG then
			'' Explicit "int" base type and no modifiers
			dtype = iif(unsignedmods > 0, TYPE_ULONG, TYPE_LONG)
		elseif unsignedmods > 0 then
			'' UNSIGNED only
			dtype = TYPE_ULONG
		elseif signedmods > 0 then
			'' SIGNED only
			dtype = TYPE_LONG
		else
			'' No modifiers and no explicit "int" either
			cError("expected a data type" + tkButFound(c.x))
		end if
	end select

	select case dtype
	case TYPE_ANY, TYPE_SINGLE, TYPE_DOUBLE, TYPE_UDT
		if signedmods or unsignedmods or shortmods or longmods then
			cError("SIGNED|UNSIGNED|SHORT|LONG modifiers used with void/float/double/typedef/UDT")
		end if
	case TYPE_ZSTRING, TYPE_BYTE, TYPE_UBYTE
		if shortmods or longmods then
			cError("SHORT|LONG modifiers used with CHAR type")
		end if
	end select

	'' Any CONSTs on the base type are merged into one
	''    const int a;
	''    const int const a;
	''          int const a;
	''    const const int const const a;
	'' It's all the same...
	if constmods > 0 then
		dtype = typeSetIsConst(dtype)
	end if

	'' __ATTRIBUTE__((...))
	cGccAttributeList(gccattribs)
end sub

'' ParamDeclList = ParamDecl (',' ParamDecl)*
'' ParamDecl = '...' | Declaration{Param}
private function cParamDeclList() as ASTNODE ptr
	var group = astNewGROUP()

	do
		dim as ASTNODE ptr t

		'' '...'?
		if tkGet(c.x) = TK_ELLIPSIS then
			t = astNew(ASTCLASS_PARAM)
			c.x += 1
		else
			t = cDeclaration(ASTCLASS_PARAM, 0)
		end if

		astAppend(group, t)

		'' ','?
	loop while cMatch(TK_COMMA) and c.parseok

	function = group
end function

private function hCanHaveInitializer(byval n as ASTNODE ptr) as integer
	select case n->class
	case ASTCLASS_PARAM
		function = TRUE
	case ASTCLASS_VAR
		function = ((n->attrib and ASTATTRIB_EXTERN) = 0)
	end select
end function

private function hHasVarargParam(byval proc as ASTNODE ptr) as integer
	var param = proc->head
	while param
		if param->dtype = TYPE_NONE then
			return TRUE
		end if
		param = param->next
	wend
	function = FALSE
end function

private sub hPostprocessDeclarator(byval n as ASTNODE ptr)
	if n->class = ASTCLASS_PROC then
		'' Ignore extern on procedures, not needed explicitly
		n->attrib and= not ASTATTRIB_EXTERN

		'' Default to cdecl, if there's no explicit callconv attribute yet
		if (n->attrib and ASTATTRIB__CALLCONV) = 0 then
			n->attrib or= ASTATTRIB_CDECL

		'' And show an error if conflicting attributes were given
		'' (perhaps we just made a mistake assigning them - better safe...)
		elseif (n->attrib and ASTATTRIB__CALLCONV) = ASTATTRIB__CALLCONV then
			cError("cdecl/stdcall attributes specified together")
		end if

		'' vararg functions are always cdecl though; explicitly specified
		'' callconvs are ignored
		if n->attrib and ASTATTRIB_STDCALL then
			if hHasVarargParam(n) then
				n->attrib and= not ASTATTRIB__CALLCONV
				n->attrib or= ASTATTRIB_CDECL
			end if
		end if
	end if

	'' Visit procptr subtypes
	if n->subtype then hPostprocessDeclarator(n->subtype)
end sub

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
''    ()           empty parameter list for anonymous parameter: int f(int ());
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
''    procptrdtype      = typeAddrOf(TYPE_PROC)
''    innernode = NULL
''    node = NULL
''    t = VAR(f as int ptr)    (f as int ptr for now, in case it's just "int (*f);")
''
''         int (*f)(int a);
'' depth 0:    ^  ^^^^^^^^
''    innerprocptrdtype = typeAddrOf(TYPE_PROC)        (passed up from depth 1)
''    procptrdtype      = TYPE_PROC                    (unused)
''    innernode = VAR(f as int ptr)                    (passed up from depth 1)
''    node      = PROC(function(a as int) as int)      (new function pointer subtype)
''    t = VAR(f as function(a as int) as int)          (adjusted AST: f turned into function pointer)
''
'' Example 2:
''
''         int (*(*f)(int a))(int b);
'' depth 2:       ^^
''    innerprocptrdtype = TYPE_PROC                    (unused)
''    procptrdtype      = typeAddrOf(TYPE_PROC)
''    innernode = NULL
''    node      = NULL
''    t = VAR(f as int ptr ptr)      (f as int ptr ptr for now, in case it's just "int (*(*f));")
''
''         int (*(*f)(int a))(int b);
'' depth 1:     ^^  ^^^^^^^^
''    innerprocptrdtype = typeAddrOf(TYPE_PROC)        (passed up from depth 2)
''    procptrdtype      = typeAddrOf(TYPE_PROC)
''    innernode = VAR(f as int ptr ptr)                (passed up from depth 2)
''    node      = PROC(function(a as int) as int ptr)  (new function pointer subtype,
''                                                     result = int ptr for now, in case it's
''                                                     just "int (*(*f)(int a));")
''    t = VAR(f as function(a as int) as int ptr)      (adjusted AST: f turned into function pointer)
''
''         int (*(*f)(int a))(int b);
'' depth 0:    ^            ^^^^^^^^
''    innerprocptrdtype = typeAddrOf(TYPE_PROC)        (passed up from depth 1)
''    procptrdtype      = TYPE_PROC (unused)
''    innernode = PROC(function(a as int) as int ptr)  (passed up from depth 1)
''    node      = PROC(function(b as int) as int)      (new function pointer subtype)
''    t = VAR(f as function(a as int) as function(b as int) as int)
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
''    extern p as sub stdcall(byval a as long)
'' i.e. the stdcall attribute goes to the procptr subtype, no matter whether
'' it appears in the toplevel declarator (the proc) or the nested declarator
'' (the pointer var).
''
'' Here the stdcall goes to the proc that's being declared, not to its result type:
''    __attribute__((stdcall)) void (*f(int a))(int b);
''    void (*f(int a))(int b) __attribute__((stdcall));
''    declare function f stdcall(byval a as long) as sub cdecl(byval b as long)
''
'' Here the stdcall goes to the proc's result procptr subtype, not the proc itself:
''    void (__attribute__((stdcall)) *f(int a))(int b);
''    declare function f cdecl(byval a as long) as sub stdcall(byval b as long)
''
'' This proc returns a pointer to the above one:
''    void (__attribute__((stdcall)) *(*f(int a))(int b))(int c);
''                                      ^^^^^^^^
''                                    ^^        ^^^^^^^^
''          ^^^^^^^^^^^^^^^^^^^^^^^^^^                  ^^^^^^^^
''    ^^^^^^                                                    ^
''    declare function f cdecl  (byval a as long) as _
''            function   cdecl  (byval b as long) as _
''            sub        stdcall(byval c as long)
''
'' Here the stdcall still goes to the proc that's being declared:
''    __attribute__((stdcall)) void (*(*f(int a))(int b))(int c);
''    declare function f stdcall(byval a as long) as _
''            function   cdecl  (byval b as long) as _
''            sub        cdecl  (byval c as long)
''
'' Here the stdcall still goes to the proc's result type:
''    void (*(__attribute__((stdcall)) *f(int a))(int b))(int c);
''    declare function f cdecl  (byval a as long) as _
''            function   stdcall(byval b as long) as _
''            sub        cdecl  (byval c as long)
''
'' I.e. attributes from the base type go to the inner most declarator (which
'' ends up defining the toplevel object, a proc or var), while attributes from
'' declarators go to the nodes for those declarators.
''
'' More about function pointers because they seem to be more complicated, these
'' are all ok:
''
''    __attribute__((stdcall)) void (*f(int a))(int b);         // proc
''    declare function f stdcall(byval a as long) as sub cdecl(byval b as long)
''
''    extern __attribute__((stdcall)) void (*(*p)(int a))(int b) = f;  // ptr to it
''    extern void (*(__attribute__((stdcall)) *p)(int a))(int b) = f;  // same thing, apparently
''    extern p as function stdcall(byval a as long) as sub cdecl(byval b as long)
''
'' I.e. we see again that for procptr vars, the attributes from toplevel and
'' inner-most declarators have the same effect - they go to the procptr subtype
'' in both cases.
''
''    void (__attribute__((stdcall)) *f(int a))(int b);         // different proc
''    declare function f cdecl(byval a as long) as sub stdcall(byval b as long)
''
''    extern void (__attribute__((stdcall)) *(*p)(int a))(int b) = f;  // ptr to it
''    extern p as function cdecl(byval a as long) as sub stdcall(byval b as long)
''
'' Here the stdcall is in the middle declarator and goes to the proc type
'' corresponding to it, the procptr's result type.
''
private function cDeclarator _
	( _
		byval nestlevel as integer, _
		byval astclass as integer, _
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
	cGccAttributeList(gccattribs)

	'' Pointers: ('*')*
	while cMatch(TK_STAR) and c.parseok
		if (typeGetPtrCount(procptrdtype) = TYPEMAX_PTR) or _
		   (typeGetPtrCount(dtype       ) = TYPEMAX_PTR) then
			cError("too many pointers")
			exit while
		end if

		procptrdtype = typeAddrOf(procptrdtype)
		dtype = typeAddrOf(dtype)

		'' (CONST|RESTRICT|__ATTRIBUTE__((...)))*
		while c.parseok
			'' __ATTRIBUTE__((...))
			cGccAttributeList(gccattribs)

			select case tkGet(c.x)
			case KW_CONST
				procptrdtype = typeSetIsConst(procptrdtype)
				dtype = typeSetIsConst(dtype)
				c.x += 1

			case KW_RESTRICT, KW___RESTRICT, KW___RESTRICT__
				'' The restrict keyword is not interesting for FB bindings, just ignore
				c.x += 1

			case else
				exit while
			end select
		wend
	wend

	dim as ASTNODE ptr t, innernode

	''    '(' Declarator ')'    |    [Identifier]

	'' Special case for parameters:
	'' * They can be anonymous, and thus a '(' can indicate either a
	''   parenthesized identifier or a parameter list. It depends on the
	''   token behind the '(' - if it's a data type or a ')' then it's a
	''   parameter list.
	'' * If it's a parameter list, it can be parenthesized, even multiple
	''   times. This isn't possible with "normal" function declarations...
	var paramlistnesting = 0
	if astclass = ASTCLASS_PARAM then
		var y = c.x
		while tkGet(y) = TK_LPAREN
			y += 1
		wend
		if hIsDataType(y) or (tkGet(y) = TK_RPAREN) then
			paramlistnesting = y - c.x
		end if
	end if

	'' '(' for declarator?
	if (tkGet(c.x) = TK_LPAREN) and (paramlistnesting = 0) then
		c.x += 1

		t = cDeclarator(nestlevel + 1, astclass, dtype, basesubtype, 0, innernode, innerprocptrdtype, innergccattribs)

		'' ')'
		cExpectMatch(TK_RPAREN, "for '(...)' parenthesized declarator")
	else
		'' [Identifier]
		'' An identifier must exist, except for parameters/types, and
		'' in fact for types there mustn't be an id.
		dim id as zstring ptr
		if astclass <> ASTCLASS_DATATYPE then
			if tkGet(c.x) = TK_ID then
				id = tkSpellId(c.x)
				c.x += 1
			else
				if astclass <> ASTCLASS_PARAM then
					cError("expected identifier for the symbol declared in this declaration" + tkButFound(c.x))
					id = @"<error-recovery>"
				end if
			end if
		end if

		t = astNew(astclass, id)
		astSetType(t, dtype, basesubtype)
	end if

	node = t

	select case tkGet(c.x)
	'' ('[' [ArrayElements] ']')*
	case TK_LBRACKET
		'' Can't allow arrays on everything - currently, it's only
		'' handled for vars/fields/params/typedefs
		if node->class = ASTCLASS_DATATYPE then
			cError("TODO: arrays not supported here yet")
		end if

		assert(node->array = NULL)
		node->array = astNew(ASTCLASS_ARRAY)

		'' For each array dimension...
		do
			'' '['
			c.x += 1

			var d = astNew(ASTCLASS_DIMENSION)

			'' Just '[]'?
			if tkGet(c.x) = TK_RBRACKET then
				d->expr = astNew(ASTCLASS_ELLIPSIS)
			else
				d->expr = cExpression(TRUE)
			end if

			astAppend(node->array, d)

			'' ']'
			cExpectMatch(TK_RBRACKET, "to close this array dimension declaration")

			'' '['? (next dimension)
		loop while (tkGet(c.x) = TK_LBRACKET) and c.parseok

	'' ':' <bits>
	case TK_COLON
		if node->class <> ASTCLASS_FIELD then
			cError("bitfields not supported here")
		end if
		c.x += 1

		node->bits = cExpression(FALSE)

	'' '(' ParamList ')'
	case TK_LPAREN
		if paramlistnesting = 0 then
			paramlistnesting = 1
		end if
		c.x += paramlistnesting

		'' Parameters turn a vardecl/fielddecl into a procdecl,
		'' unless they're for a procptr type.
		if innerprocptrdtype <> TYPE_PROC then
			'' There were '()'s above and the recursive
			'' cDeclarator() call found pointers/CONSTs,
			'' these parameters are for a function pointer.
			''
			'' Whichever object should become the function pointer,
			'' its dtype/subtype must be adjusted accordingly.
			'' For the subtype, a new PROC node is created, which
			'' will hold the parameters etc. found at this level.

			'' New PROC node for the function pointer's subtype
			node = astNew(ASTCLASS_PROC)
			astSetType(node, dtype, basesubtype)

			'' Turn the object into a function pointer
			astDelete(innernode->subtype)
			innernode->dtype = innerprocptrdtype
			innernode->subtype = node

			innerprocptrdtype = TYPE_PROC
		else
			select case t->class
			'' A plain symbol, not a pointer, becomes a function
			case ASTCLASS_VAR, ASTCLASS_FIELD
				t->class = ASTCLASS_PROC

			'' Anything else though (typedefs, params, type casts...)
			'' with params isn't turned into a proc, but just has function type.
			case else
				node = astNew(ASTCLASS_PROC)
				astSetType(node, dtype, basesubtype)

				astDelete(t->subtype)
				t->dtype = TYPE_PROC
				t->subtype = node
			end select
		end if

		'' Just '(void)'?
		if (tkGet(c.x) = KW_VOID) and (tkGet(c.x + 1) = TK_RPAREN) then
			'' VOID
			c.x += 1
		'' Not just '()'?
		elseif tkGet(c.x) <> TK_RPAREN then
			assert(node->class = ASTCLASS_PROC)
			astAppend(node, cParamDeclList())
		end if

		'' ')'
		while paramlistnesting > 0
			cExpectMatch(TK_RPAREN, "to close parameter list in function declaration")
			paramlistnesting -= 1
		wend
	end select

	'' __ATTRIBUTE__((...))
	var endgccattribs = 0
	cGccAttributeList(endgccattribs)

	if nestlevel > 0 then
		'' __attribute__'s from this level should always be passed up
		gccattribs or= endgccattribs

		'' Pass innerprocptrdtype and innergccattribs up again if they
		'' weren't used up on this level
		if innerprocptrdtype <> TYPE_PROC then
			gccattribs or= innergccattribs
			procptrdtype = typeExpand(innerprocptrdtype, procptrdtype)
			if procptrdtype = TYPE_NONE then
				cError("too many pointers")
			end if
		else
			node->attrib or= innergccattribs
		end if
	else
		'' At toplevel nothing can be passed up, everything must be assigned.
		'' __attribute__'s from the base type go to the toplevel symbol
		'' that's being declared. If it's a function pointer, then callconv
		'' attributes go to the proc subtype, not the procptr var.

		basegccattribs or= gccattribs or endgccattribs

		if (typeGetDt(t->dtype) = TYPE_PROC) and (t->class <> ASTCLASS_PROC) then
			assert(t->subtype->class = ASTCLASS_PROC)
			t->subtype->attrib or= basegccattribs and ASTATTRIB__CALLCONV
			t->attrib or= basegccattribs and (not ASTATTRIB__CALLCONV)
		else
			t->attrib or= basegccattribs
		end if

		node->attrib or= innergccattribs

		'' dllimport implies extern, and isn't allowed together with static
		if t->attrib and ASTATTRIB_DLLIMPORT then
			if t->attrib and ASTATTRIB_STATIC then
				cError("static dllimport")
				t->attrib and= not ASTATTRIB_STATIC
			end if
			t->attrib or= ASTATTRIB_EXTERN
		end if

		hPostprocessDeclarator(t)
	end if

	function = t
end function

'' Data type parsing for cast operations and sizeof():
''    BaseType Declarator
'' Parsing just the base type isn't enough, because it could be a function
'' pointer cast with parameter list etc. We need to do full declarator parsing
'' to handle that.
private function cDataType() as ASTNODE ptr
	dim as integer dtype, gccattribs
	dim as ASTNODE ptr subtype
	cBaseType(dtype, subtype, gccattribs, FALSE)

	'' Disallow UDT bodies in type "expressions" - FB doesn't support it,
	'' and our highlevel passes/code emitter don't expect it.
	if subtype then
		select case subtype->class
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			cError("UDT in datatype expression; not supported in FB")
			dtype = TYPE_INTEGER
			astDelete(subtype)
			subtype = NULL
		end select
	end if

	function = cDeclarator(0, ASTCLASS_DATATYPE, dtype, subtype, gccattribs, NULL, 0, 0)
end function

private function hUpdateTypeNameReferences(byval n as ASTNODE ptr) as integer
	if typeGetDt(n->dtype) = TYPE_UDT then
		assert(astIsTEXT(n->subtype))
		if *n->subtype->text = c.oldid then
			astSetText(n->subtype, c.newid)
		end if
	end if
	function = TRUE
end function

'' Move nested named UDT bodies outside of the given UDT (if it contains any).
'' FB does not support named UDTs to be nested like that.
''    struct A {
''        struct B { }; /* gcc warning: declaration does not declare anything */
''    };
'' =>
''    struct B { };
''    struct A { };
private sub hUnscopeNestedNamedUdts(byval result as ASTNODE ptr, byval udt as ASTNODE ptr)
	var i = udt->head
	while i
		var nxt = i->next

		select case i->class
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			if i->text then
				if udt->text then
					'' If the nested UDT was given an auto-generated name,
					'' prepend the parent UDT name as "namespace".
					'' All references to the old name must be updated.
					if i->attrib and ASTATTRIB_GENERATEDID then
						c.oldid = *i->text
						c.newid = *udt->text + "_" + *i->text
						astVisit(udt, @hUpdateTypeNameReferences)
						astSetText(i, c.newid)
					end if
				end if

				astUnlink(udt, i)
				astAppend(result, i)
			end if
		end select

		i = nxt
	wend
end sub

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
private function cDeclaration(byval astclass as integer, byval gccattribs as integer) as ASTNODE ptr
	assert(astclass <> ASTCLASS_DATATYPE)

	'' BaseType
	dim as integer dtype, is_tag
	dim as ASTNODE ptr subtype
	cBaseType(dtype, subtype, gccattribs, is_tag)

	var result = astNewGROUP()

	'' Special case for standalone struct/union/enum declarations (even with CONST bits):
	if (typeGetDtAndPtr(dtype) = TYPE_UDT) and is_tag then
		'' Tag body?
		''    STRUCT|UNION|ENUM [Identifier] '{' ... '}' ';'
		select case subtype->class
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			'' ';'?
			if cMatch(TK_SEMI) then
				hUnscopeNestedNamedUdts(result, subtype)
				astAppend(result, subtype)
				return result
			end if

		'' Useless tag declaration?
		''    STRUCT|UNION|ENUM Identifier ';'
		case ASTCLASS_TEXT
			'' ';'?
			if cMatch(TK_SEMI) then
				'' Ignore & treat as no-op
				astDelete(subtype)
				return result
			end if
		end select
	end if

	'' Special case for inline struct/union/enum bodies, for example:
	''    typedef struct { ... } myTypedef;
	''    struct { ... } myVarOrField;
	'' Turn the struct/union/enum body into a separate declaration (as
	'' needed by FB) and make the main declaration reference it by name.
	if typeGetDtAndPtr(dtype) = TYPE_UDT then
		select case subtype->class
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			var udt = subtype

			'' If the UDT is anonymous, we have to generate a name for it.
			'' (FB doesn't allow anonymous non-nested UDTs)
			if udt->text = NULL then
				'' Try to name it after the symbol declared in this declaration
				if tkGet(c.x) = TK_ID then
					astSetText(udt, tkSpellId(c.x))
				else
					'' Auto-generate id
					'' TODO:
					''  * avoid inter-binding conflicts
					''  * name after fields (name & type) instead of counter
					''  * If this is really only needed for anonymous parameters,
					''    then it's probably very rare in practice. No need to bother.
					astSetText(udt, "_" + str(c.tempids))
					c.tempids += 1
				end if
				udt->attrib or= ASTATTRIB_GENERATEDID
			end if

			subtype = astNewTEXT(udt->text)
			subtype->attrib or= ASTATTRIB_TAGID or (udt->attrib and ASTATTRIB_GENERATEDID)

			hUnscopeNestedNamedUdts(result, udt)
			astAppend(result, udt)
		end select
	end if

	var require_semi = TRUE

	'' ... (',' ...)*
	var declarator_count = 0
	do
		declarator_count += 1
		var n = cDeclarator(0, astclass, dtype, subtype, gccattribs, NULL, 0, 0)
		astAppend(result, n)

		if n->class = ASTCLASS_TYPEDEF then
			'' Register known typedefs, so we can disambiguate type casts
			cAddTypedef(n->text)
		end if

		if hCanHaveInitializer(n) then
			'' ['=' Initializer]
			if cMatch(TK_EQ) then
				assert(n->expr = NULL)
				n->expr = cExpressionOrInitializer()

				'' If it's an array, then it must be an array initializer (or a string literal),
				'' not a struct initializer
				if n->array then
					if n->expr->class = ASTCLASS_STRUCTINIT then
						n->expr->class = ASTCLASS_ARRAYINIT
					end if
				end if
			end if
		end if

		'' Parameters can't have commas and more identifiers,
		'' and don't need the ';' either.
		if n->class = ASTCLASS_PARAM then
			require_semi = FALSE
			exit do
		end if

		'' '{', procedure body?
		if (n->class = ASTCLASS_PROC) and (tkGet(c.x) = TK_LBRACE) then
			'' A procedure with body must be the first and only
			'' declarator in the declaration.
			if declarator_count = 1 then
				var originalparseok = c.parseok

				assert(n->expr = NULL)
				n->expr = cScope()

				if c.api->nofunctionbodies then
					c.parseok = originalparseok
					n->expr = NULL
				end if

				require_semi = FALSE
				exit do
			end if
		end if

		'' ','?
	loop while cMatch(TK_COMMA) and c.parseok

	if require_semi then
		'' ';'
		cExpectMatch(TK_SEMI, "to finish this declaration")
	end if

	astDelete(subtype)
	function = result
end function

'' Variable/procedure declarations
''    GccAttributeList [EXTERN|STATIC] Declaration
private function cVarOrProcDecl(byval is_local as integer) as ASTNODE ptr
	'' __ATTRIBUTE__((...))
	var gccattribs = 0
	cGccAttributeList(gccattribs)

	'' [EXTERN|STATIC]
	select case tkGet(c.x)
	case KW_EXTERN
		gccattribs or= ASTATTRIB_EXTERN
		c.x += 1
	case KW_STATIC
		gccattribs or= ASTATTRIB_STATIC
		c.x += 1
	end select

	if is_local then
		gccattribs or= ASTATTRIB_LOCAL
	end if

	'' Declaration. Assume that it's a variable for now; the declarator
	'' parser may turn it into a procedure if it has parameters.
	function = cDeclaration(ASTCLASS_VAR, gccattribs)
end function

'' Expression statement: Assignments, function calls, i++, etc.
private function cExpressionStatement() as ASTNODE ptr
	function = cExpression(TRUE)

	'' ';'?
	cExpectMatch(TK_SEMI, "(end of expression statement)")
end function

'' RETURN Expression ';'
private function cReturn() as ASTNODE ptr
	'' RETURN
	assert(tkGet(c.x) = KW_RETURN)
	c.x += 1

	'' Expression
	function = astNew(ASTCLASS_RETURN, cExpression(TRUE))

	'' ';'
	cExpectMatch(TK_SEMI, "(end of statement)")
end function

'' '{ ... }' statement block
'' Using cBody() to allow the constructs in this scope block to be parsed
'' separately. If we can't parse one of them, then only that one will become an
'' unknown construct. The rest of the scope can potentially be parsed fine.
private function cScope() as ASTNODE ptr
	'' '{'
	assert(tkGet(c.x) = TK_LBRACE)
	c.x += 1

	function = astNew(ASTCLASS_SCOPEBLOCK, cBody(ASTCLASS_SCOPEBLOCK))

	'' '}'
	cExpectMatch(TK_RBRACE, "to close compound statement")
end function

private function cIfBlock() as ASTNODE ptr
	var ifblock = astNew(ASTCLASS_IFBLOCK)

	'' IF
	assert(tkGet(c.x) = KW_IF)
	c.x += 1

	'' '('
	cExpectMatch(TK_LPAREN, "in front of if condition")

	'' condition expression
	var ifpart = astNew(ASTCLASS_IFPART)
	ifpart->expr = cExpression(TRUE)

	'' ')'
	cExpectMatch(TK_RPAREN, "behind if condition")

	'' if/true statement
	astAppend(ifpart, cConstruct(ASTCLASS_SCOPEBLOCK))
	astAppend(ifblock, ifpart)

	'' ELSE?
	if cMatch(KW_ELSE) then
		'' else/false statement
		astAppend(ifblock, astNew(ASTCLASS_ELSEPART, cConstruct(ASTCLASS_SCOPEBLOCK)))
	end if

	function = ifblock
end function

private function cDoWhile() as ASTNODE ptr
	var dowhile = astNew(ASTCLASS_DOWHILE)

	'' DO
	assert(tkGet(c.x) = KW_DO)
	c.x += 1

	'' loop body
	astAppend(dowhile, cConstruct(ASTCLASS_SCOPEBLOCK))

	'' WHILE
	cExpectMatch(KW_WHILE, "behind do loop body")

	'' '('
	cExpectMatch(TK_LPAREN, "in front of loop condition")

	'' loop condition expression
	dowhile->expr = cExpression(TRUE)

	'' ')'
	cExpectMatch(TK_RPAREN, "behind loop condition")

	cExpectMatch(TK_SEMI, "behind do/while loop")

	function = dowhile
end function

private function cWhile() as ASTNODE ptr
	var whileloop = astNew(ASTCLASS_WHILE)

	'' WHILE
	assert(tkGet(c.x) = KW_WHILE)
	c.x += 1

	'' '('
	cExpectMatch(TK_LPAREN, "in front of loop condition")

	'' loop condition expression
	whileloop->expr = cExpression(TRUE)

	'' ')'
	cExpectMatch(TK_RPAREN, "behind loop condition")

	'' loop body
	astAppend(whileloop, cConstruct(ASTCLASS_SCOPEBLOCK))

	function = whileloop
end function

private function cConstruct(byval bodyastclass as integer) as ASTNODE ptr
	if tkGet(c.x) = TK_FBCODE then
		var n = astNew(ASTCLASS_FBCODE, tkGetText(c.x))
		c.x += 1
		return n
	end if

	'' TODO: only parse #defines at toplevel, not inside structs etc.
	'' '#'?
	if (tkGet(c.x) = TK_HASH) and tkIsOriginal(c.x) then
		c.x += 1

		dim directive as ASTNODE ptr
		if tkGet(c.x) = TK_ID then
			select case *tkSpellId(c.x)
			case "define"
				directive = cDefine()
			case "undef"
				directive = cUndef()
			case "include"
				directive = cInclude()
			case "pragma"
				c.x += 1

				select case tkSpell(c.x)
				case "pack"
					directive = cPragmaPack()
				case "comment"
					directive = cPragmaComment()
				end select
			end select
		end if

		if directive = NULL then
			cError("unknown CPP directive")
			directive = astNewGROUP()
		end if

		return directive
	end if

	if bodyastclass = ASTCLASS_ENUM then
		return cEnumConst()
	end if

	select case tkGet(c.x)
	case KW_TYPEDEF
		return cTypedef()
	case TK_SEMI
		'' Ignore standalone ';'
		c.x += 1
		return astNewGROUP()
	case TK_LBRACE : return cScope()
	case KW_IF     : return cIfBlock()
	case KW_DO     : return cDoWhile()
	case KW_WHILE  : return cWhile()
	case KW_RETURN : return cReturn()
	end select

	select case bodyastclass
	case ASTCLASS_STRUCT, ASTCLASS_UNION
		'' Field declaration
		function = cDeclaration(ASTCLASS_FIELD, 0)
	case ASTCLASS_SCOPEBLOCK
		'' Disambiguate: local declaration vs. expression
		'' If it starts with a data type, __attribute__, or 'static',
		'' then it must be a declaration.
		if (tkGet(c.x) = KW_STATIC) orelse hIsDataTypeOrAttribute(c.x) then
			function = cVarOrProcDecl(TRUE)
		else
			function = cExpressionStatement()
		end if
	case else
		function = cVarOrProcDecl(FALSE)
	end select
end function

private function cBody(byval bodyastclass as integer) as ASTNODE ptr
	var result = astNewGROUP()

	do
		select case tkGet(c.x)
		case TK_EOF
			exit do

		'' End of #define body
		case TK_EOL
			assert(c.parentdefine)
			exit do

		'' '}' (end of block)
		case TK_RBRACE
			if bodyastclass >= 0 then
				exit do
			end if
		end select

		var begin = c.x
		var t = cConstruct(bodyastclass)

		t = hHandleCommasAssigns(t, FALSE)

		if c.parseok = FALSE then
			c.parseok = TRUE

			'' Skip current construct and preserve its tokens in
			'' an UNKNOWN node
			c.x = hSkipConstruct(begin, FALSE)
			hTurnIntoUNKNOWN(t, begin, c.x - 1)
		end if

		'' If at toplevel, assign source location to toplevel ASTNODEs
		if bodyastclass < 0 then
			var location = tkGetLocation(begin)
			if t->class = ASTCLASS_GROUP then
				var i = t->head
				while i
					i->location = location
					i = i->next
				wend
			else
				t->location = location
			end if
		end if

		astAppend(result, t)
	loop

	function = result
end function

function cMain() as ASTNODE ptr
	var t = cBody(-1)

	'' Process the #define bodies which weren't parsed yet
	for i as integer = 0 to c.defbodycount - 1
		with c.defbodies[i]
			c.parseok = TRUE
			c.x = .xbodybegin

			'' Parse #define body
			var add_to_ast = TRUE
			cParseDefBody(.n, .xdefbegin, add_to_ast)

			if add_to_ast = FALSE then
				astRemove(t, .n)
			end if
		end with
	next

	function = t
end function

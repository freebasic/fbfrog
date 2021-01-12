''
'' C parsing
''
'' * parses the content of the tk buffer and builds an AST
'' * parsing one "construct" (declaration/statement) at a time
'' * able to recover from parsing errors, by skipping the current construct and
''   continuing to the next one. Bad constructs are stored in form
''   ASTKIND_UNKNOWN ASTNODEs. Even if all C constructs and gcc extensions
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

#include once "c-parser.bi"

#include once "c-common.bi"
#include once "emit.bi"
#include once "fbfrog.bi"
#include once "util.bi"

using tktokens

type DATATYPEINFO
	id as zstring ptr
	dtype as integer
end type

dim shared extradatatypes(0 to ...) as DATATYPEINFO => _
{ _
	(@"__int8"   , TYPE_BYTE    ), _
	(@"__int16"  , TYPE_SHORT   ), _
	(@"__int32"  , TYPE_LONG    ), _
	(@"__int64"  , TYPE_LONGINT ), _
	(@"int8_t"   , TYPE_BYTE    ), _
	(@"int16_t"  , TYPE_SHORT   ), _
	(@"int32_t"  , TYPE_LONG    ), _
	(@"int64_t"  , TYPE_LONGINT ), _
	(@"uint8_t"  , TYPE_UBYTE   ), _
	(@"uint16_t" , TYPE_USHORT  ), _
	(@"uint32_t" , TYPE_ULONG   ), _
	(@"uint64_t" , TYPE_ULONGINT), _
	(@"intptr_t" , TYPE_INTEGER ), _
	(@"uintptr_t", TYPE_UINTEGER), _
	(@"ptrdiff_t", TYPE_INTEGER ), _
	(@"size_t"   , TYPE_UINTEGER), _
	(@"ssize_t"  , TYPE_INTEGER ), _
	(@"wchar_t"  , TYPE_WSTRING )  _
}

function CParser.match(byval t as integer) as integer
	if tk->get(x) = t then
		x += 1
		function = TRUE
	end if
end function

sub CParser.showError(byref message as string)
	if parseok then
		parseok = FALSE
		if frog.verbose then
			print tk->report(x, message)
		end if
	end if
end sub

sub CParser.expectMatch(byval t as integer, byref message as string)
	if tk->get(x) = t then
		x += 1
	elseif parseok then
		parseok = FALSE
		if frog.verbose then
			print tk->report(x, tk->makeExpectedMessage(x, tkInfoPretty(t) + " " + message))
		end if
	end if
end sub

function CParser.isInsideDefineBody() as integer
	return (parentdefine <> NULL)
end function

sub CParser.resetPragmaPack()
	pragmapack.stack(pragmapack.level) = 0
end sub

function CParser.isTypedef(byval id as zstring ptr) as integer
	'' 1. Check typedefs seen by C parser
	if typedefs.contains(id, hashHash(id)) then
		return TRUE
	end if

	'' 2. Check -typedefhint options
	function = api->idopt(OPT_TYPEDEFHINT).matches(id)
end function

function CParser.lookupExtraDataType(byval id as zstring ptr) as integer
	var item = extradatatypehash.lookup(id, hashHash(id))
	if item->s then
		function = cint(item->data)
	else
		function = TYPE_NONE
	end if
end function

function CParser.identifierIsMacroParam(byval id as zstring ptr) as integer
	if parentdefine then
		function = (parentdefine->lookupMacroParam(id) >= 0)
	else
		function = FALSE
	end if
end function

constructor CParser(byref tk as TokenBuffer, byref api as ApiInfo)
	this.tk = @tk
	this.api = @api
	x = 0
	parseok = TRUE
	parentdefine = NULL
	tempids = 0

	for i as integer = 0 to ubound(extradatatypes)
		extradatatypehash.addOverwrite(extradatatypes(i).id, cast(any ptr, extradatatypes(i).dtype))
	next

	'' Initially no packing
	pragmapack.level = 0
	resetPragmaPack()

	defbodies = NULL
	defbodycount = 0
	defbodyroom = 0
end constructor

destructor CParser()
	deallocate(defbodies)
end destructor

sub CParser.addTypedef(byval id as const zstring ptr)
	typedefs.addOverwrite(id, NULL)
end sub

sub CParser.addDefBody(byval xdefbegin as integer, byval xbodybegin as integer, byval n as AstNode ptr)
	if defbodyroom = defbodycount then
		if defbodyroom = 0 then
			defbodyroom = 512
		else
			defbodyroom *= 2
		end if
		defbodies = reallocate(defbodies, defbodyroom * sizeof(*defbodies))
	end if
	with defbodies[defbodycount]
		.xdefbegin = xdefbegin
		.xbodybegin = xbodybegin
		.n = n
	end with
	defbodycount += 1
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function CParser.parseLiteral(byval astkind as integer, byval eval_escapes as integer) as AstNode ptr
	dim errmsg as string
	dim n as AstNode ptr

	if astkind = ASTKIND_CONSTI then
		n = hNumberLiteral(*tk, x, FALSE, errmsg, api->clong32)
	else
		n = hStringLiteral(*tk, x, eval_escapes, errmsg)
	end if

	if n = NULL then
		showError(errmsg)

		select case astkind
		case ASTKIND_CONSTI
			n = astNew(ASTKIND_CONSTI, "0")
			n->dtype = TYPE_LONG
		case ASTKIND_STRING
			n = astNew(ASTKIND_STRING, "abc")
			n->dtype = TYPE_ZSTRING
		case ASTKIND_CHAR
			n = astNew(ASTKIND_CHAR, "0")
			n->dtype = TYPE_BYTE
		end select
	end if

	x += 1
	function = n
end function

'' ("..." | [#]id)*
function CParser.parseStringLiteralSequence() as AstNode ptr
	var strcat = astNew(ASTKIND_STRCAT)

	while parseok
		select case tk->get(x)
		case TK_ID
			strcat->append(astNewTEXT(tk->spellId(x)))
			x += 1

		case TK_STRING, TK_WSTRING
			strcat->append(parseLiteral(ASTKIND_STRING, TRUE))

		'' '#' stringify operator
		case TK_HASH
			'' #id?
			if tk->get(x + 1) <> TK_ID then
				exit while
			end if
			x += 1

			strcat->append(astNew(ASTKIND_STRINGIFY, astNewTEXT(tk->getText(x))))
			x += 1

		case else
			exit while
		end select
	wend

	'' Strip the STRCAT node if it's a single literal only...
	if strcat->head = strcat->tail then
		var result = strcat->head
		strcat->unlink(strcat->head)
		delete strcat
		strcat = result
	end if

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
function CParser.isDataType(byval y as integer) as integer
	var is_type = FALSE
	select case tk->get(y)
	case KW_SIGNED, KW_UNSIGNED, KW_CONST, KW_SHORT, KW_LONG, _
	     KW_ENUM, KW_STRUCT, KW_UNION, _
	     KW_VOID, KW_CHAR, KW_FLOAT, KW_DOUBLE, KW_INT
		is_type = not identifierIsMacroParam(tk->spellId(y))
	case TK_ID
		var id = tk->spellId(y)
		if (lookupExtraDataType(id) <> TYPE_NONE) or isTypedef(id) then
			is_type = not identifierIsMacroParam(id)
		end if
	end select
	function = is_type
end function

function CParser.isDataTypeOrAttribute(byval y as integer) as integer
	select case tk->get(y)
	case KW___ATTRIBUTE, KW___ATTRIBUTE__
		function = not identifierIsMacroParam(tk->spellId(y))
	case else
		function = isDataType(y)
	end select
end function

function CParser.parseCall(byval functionexpr as AstNode ptr, byval allow_idseq as integer) as AstNode ptr
	assert(tk->get(x) = TK_LPAREN)
	x += 1

	functionexpr = astNew(ASTKIND_CALL, functionexpr)

	'' [Arguments]
	if tk->get(x) <> TK_RPAREN then
		'' Expr (',' Expr)*
		do
			functionexpr->append(parseExpr(FALSE, allow_idseq))

			'' ','?
		loop while match(TK_COMMA) and parseok
	end if

	'' ')'?
	expectMatch(TK_RPAREN, "to close call argument list")

	function = functionexpr
end function

'' C expression parser based on precedence climbing
function CParser.parseExprRecursive _
	( _
		byval level as integer, _
		byval parentheses as integer, _
		byval allow_toplevel_comma as integer, _
		byval allow_idseq as integer _
	) as AstNode ptr

	'' Unary prefix operators
	var op = -1
	select case tk->get(x)
	case TK_EXCL   : op = ASTKIND_CLOGNOT   '' !
	case TK_TILDE  : op = ASTKIND_NOT       '' ~
	case TK_MINUS  : op = ASTKIND_NEGATE    '' -
	case TK_PLUS   : op = ASTKIND_UNARYPLUS '' +
	case TK_AMP    : op = ASTKIND_ADDROF    '' &
	case TK_STAR   : op = ASTKIND_DEREF     '' *
	end select

	dim as AstNode ptr a
	if op >= 0 then
		x += 1
		a = astNew(op, parseExprRecursive(cprecedence(op), parentheses, allow_toplevel_comma, allow_idseq))
	else
		'' Atoms
		select case tk->get(x)

		'' '(' Expr ')'
		'' '(' DataType ')' Expr
		'' '(' FunctionTypeExpr ')' '(' ArgumentList ')'
		case TK_LPAREN
			'' '('
			x += 1

			var is_cast = isDataTypeOrAttribute(x)

			'' Find the ')' and check the token behind it, in some cases
			'' we can tell that it probably isn't a cast.
			var closingparen = tk->findClosingParen(x - 1, isInsideDefineBody(), FALSE)
			select case tk->get(closingparen + 1)
			case TK_RPAREN, TK_EOF, TK_EOL
				is_cast = FALSE
			end select

			'' Something of the form '(id*)' or just in general a
			'' '*' in front of the closing ')'? It most likely is a pointer cast.
			is_cast or= (tk->get(closingparen - 1) = TK_STAR)

			if is_cast then
				'' DataType
				var t = parseDataType()

				'' ')'
				expectMatch(TK_RPAREN, "behind the data type")

				'' Expr
				a = parseExprRecursive(cprecedence(ASTKIND_CAST), parentheses, allow_toplevel_comma, allow_idseq)

				assert(t->kind = ASTKIND_DATATYPE)
				a = astNew(ASTKIND_CAST, a)
				a->setType(t->dtype, t->subtype)
				delete t
			else
				'' Expr
				a = parseExprRecursive(0, parentheses + 1, allow_toplevel_comma, allow_idseq)

				'' ')'
				expectMatch(TK_RPAREN, "to close '(...)' parenthesized expression")

				if astIsTEXT(a) then
					if identifierIsMacroParam(a->text) then
						a->attrib or= ASTATTRIB_PARENTHESIZEDMACROPARAM
					end if
				end if

				'' '('?
				if tk->get(x) = TK_LPAREN then
					''
					'' Function call on parenthesized function name
					''
					'' TODO: Allow any expressions as function. But this requires
					'' function-pointer-to-function-type derefs (e.g. <(*myFunctionPtr)(arg1, arg2)>)
					'' to be removed automatically since they're not needed/allowed in FB.
					'' But how to differentiate from function-ptr-ptr-to-function-ptr derefs, where
					'' the deref is important?
					''
					if astIsTEXT(a) then
						a = parseCall(a, allow_idseq)
					end if
				end if
			end if

		case TK_NUMBER
			a = parseLiteral(ASTKIND_CONSTI, TRUE)

		case TK_STRING, TK_WSTRING, TK_HASH
			a = parseStringLiteralSequence()

		case TK_CHAR, TK_WCHAR
			a = parseLiteral(ASTKIND_CHAR, TRUE)

		'' Id
		'' Id '(' ArgumentList ')'
		'' Id ## Id ## ...
		'' Id ("String"|Id)*
		case TK_ID
			select case tk->get(x + 1)
			'' '('?
			case TK_LPAREN
				a = astNewTEXT(tk->spellId(x))
				x += 1

				a = parseCall(a, allow_idseq)

			'' '##'?
			case TK_HASHHASH
				a = astNew(ASTKIND_PPMERGE)
				a->append(astNewTEXT(tk->spellId(x)))
				x += 2

				'' Identifier ('##' Identifier)*
				do
					'' Identifier?
					if tk->get(x) = TK_ID then
						a->append(astNewTEXT(tk->spellId(x)))
						x += 1
					else
						showError("expected identifier as operand of '##' PP merge operator" + tk->butFound(x))
					end if

					'' '##'?
				loop while match(TK_HASHHASH) and parseok

			case TK_ID
				'' A B
				if allow_idseq then
					'' In a #define body, chances are this is a string literal sequence...
					a = parseStringLiteralSequence()
				else
					'' But as a statement, this is more likely to be a vardecl, where we
					'' didn't recognize the A as typedef yet. This shouldn't be misparsed
					'' as string literal sequence silently, so we parse A as normal expression
					'' and let B trigger an error.
					a = astNewTEXT(tk->spellId(x))
					x += 1
				end if

			case TK_STRING, TK_WSTRING, TK_HASH
				a = parseStringLiteralSequence()

			case else
				a = astNewTEXT(tk->spellId(x))
				x += 1
			end select

		'' SIZEOF Expr
		'' SIZEOF '(' DataType ')'
		case KW_SIZEOF
			x += 1

			'' ('(' DataType)?
			if (tk->get(x) = TK_LPAREN) andalso isDataTypeOrAttribute(x + 1) then
				'' '('
				x += 1

				'' DataType
				a = parseDataType()

				'' ')'
				expectMatch(TK_RPAREN, "behind the data type")
			else
				a = parseExprRecursive(cprecedence(ASTKIND_SIZEOF), parentheses + 1, allow_toplevel_comma, allow_idseq)
			end if
			a = astNew(ASTKIND_SIZEOF, a)

		'' DEFINED ['('] Identifier [')']
		case KW_DEFINED
			x += 1

			'' '('
			var have_parens = match(TK_LPAREN)

			'' Identifier
			dim as string id
			if tk->get(x) = TK_ID then
				id = *tk->spellId(x)
			else
				showError("expected identifier" + tk->butFound(x))
				id = "<error-recovery>"
			end if
			a = astNew(ASTKIND_CDEFINED, id)
			x += 1

			if have_parens then
				'' ')'
				expectMatch(TK_RPAREN, "to finish defined(...) expression")
			end if

		case else
			showError("expected expression" + tk->butFound(x))
			a = astNew(ASTKIND_CONSTI, "0")
			a->dtype = TYPE_INTEGER
		end select
	end if

	'' Infix operators
	while parseok
		select case as const tk->get(x)
		case TK_QUEST    : op = ASTKIND_IIF      '' ? (a ? b : c)
		case TK_PIPEPIPE : op = ASTKIND_CLOGOR   '' ||
		case TK_AMPAMP   : op = ASTKIND_CLOGAND  '' &&
		case TK_PIPE     : op = ASTKIND_OR       '' |
		case TK_PIPEEQ   : op = ASTKIND_CSELFOR  '' |=
		case TK_CIRC     : op = ASTKIND_XOR      '' ^
		case TK_CIRCEQ   : op = ASTKIND_CSELFXOR '' ^=
		case TK_AMP      : op = ASTKIND_AND      '' &
		case TK_AMPEQ    : op = ASTKIND_CSELFAND '' &=
		case TK_EQ       : op = ASTKIND_CASSIGN  '' =
		case TK_EQEQ     : op = ASTKIND_CEQ      '' ==
		case TK_EXCLEQ   : op = ASTKIND_CNE      '' !=
		case TK_LT       : op = ASTKIND_CLT      '' <
		case TK_LTEQ     : op = ASTKIND_CLE      '' <=
		case TK_GT       : op = ASTKIND_CGT      '' >
		case TK_GTEQ     : op = ASTKIND_CGE      '' >=
		case TK_LTLT     : op = ASTKIND_SHL      '' <<
		case TK_LTLTEQ   : op = ASTKIND_CSELFSHL '' <<=
		case TK_GTGT     : op = ASTKIND_SHR      '' >>
		case TK_GTGTEQ   : op = ASTKIND_CSELFSHR '' >>=
		case TK_PLUS     : op = ASTKIND_ADD      '' +
		case TK_PLUSEQ   : op = ASTKIND_CSELFADD '' +=
		case TK_MINUS    : op = ASTKIND_SUB      '' -
		case TK_MINUSEQ  : op = ASTKIND_CSELFSUB '' -=
		case TK_STAR     : op = ASTKIND_MUL      '' *
		case TK_STAREQ   : op = ASTKIND_CSELFMUL '' *=
		case TK_SLASH    : op = ASTKIND_DIV      '' /
		case TK_SLASHEQ  : op = ASTKIND_CSELFDIV '' /=
		case TK_PERCENT  : op = ASTKIND_MOD      '' %
		case TK_PERCENTEQ : op = ASTKIND_CSELFMOD '' %=
		case TK_LBRACKET : op = ASTKIND_INDEX    '' [ ... ]
		case TK_DOT      : op = ASTKIND_MEMBER   '' .
		case TK_ARROW    : op = ASTKIND_MEMBERDEREF '' ->
		case TK_COMMA  '' ,
			if (parentheses = 0) and (not allow_toplevel_comma) then
				exit while
			end if
			op = ASTKIND_CCOMMA
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

		if cOpIsLeftAssoc(op) then
			oplevel += 1
		end if

		'' For [] we parse until the ], no precedence levels needed
		if op = ASTKIND_INDEX then
			oplevel = 0
		end if

		'' operator
		x += 1

		'' rhs
		var b = parseExprRecursive(oplevel, parentheses + iif(op = ASTKIND_INDEX, 1, 0), allow_toplevel_comma, allow_idseq)

		'' Handle ?: special case
		if op = ASTKIND_IIF then
			'' ':'
			expectMatch(TK_COLON, "for a?b:c iif operator")

			a = astNewIIF(a, b, parseExprRecursive(oplevel, parentheses, allow_toplevel_comma, allow_idseq))
		else
			'' Handle [] special case
			if op = ASTKIND_INDEX then
				'' ']'
				expectMatch(TK_RBRACKET, "for [] indexing operator")
			end if

			a = astNew(op, a, b)
		end if
	wend

	function = a
end function

function CParser.parseExpr(byval allow_toplevel_comma as integer, byval allow_idseq as integer) as AstNode ptr
	function = parseExprRecursive(0, 0, allow_toplevel_comma, allow_idseq)
end function

'' Init:
'' '{' ExprOrInit (',' ExprOrInit)* [','] '}'
function CParser.parseInit(byval allow_idseq as integer) as AstNode ptr
	'' '{'
	assert(tk->get(x) = TK_LBRACE)
	x += 1

	var a = astNew(ASTKIND_STRUCTINIT)

	do
		'' '}'?
		if tk->get(x) = TK_RBRACE then
			exit do
		end if

		a->append(parseExprOrInit(allow_idseq))

		'' ','
	loop while match(TK_COMMA) and parseok

	expectMatch(TK_RBRACE, "to close initializer")

	function = a
end function

function CParser.parseExprOrInit(byval allow_idseq as integer) as AstNode ptr
	'' '{'?
	if tk->get(x) = TK_LBRACE then
		function = parseInit(allow_idseq)
	else
		function = parseExpr(FALSE, allow_idseq)
	end if
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub CParser.skipToCommaOrRparen()
	do
		select case tk->get(x)
		case TK_LPAREN, TK_LBRACKET, TK_LBRACE
			x = tk->findClosingParen(x, isInsideDefineBody(), TRUE)
		case TK_COMMA, TK_RPAREN, TK_EOF
			exit do
		end select
		x += 1
	loop
end sub

sub CParser.parseGccAttribute(byref gccattribs as integer)
	if tk->get(x) < TK_ID then
		showError("expected attribute identifier inside __attribute__((...))")
		exit sub
	end if

	var attr = *tk->spellId(x)

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
	     "gcc_struct", _
	     "gnu_inline", _
	     "leaf", _
	     "malloc", _
	     "may_alias", _
	     "no_instrument_function", _
	     "nonnull", _
	     "noreturn", _
	     "nothrow", _
	     "pure", _
	     "sentinel", _
	     "unused", _
	     "visibility", _
	     "warn_unused_result", _
	     "dllexport"  '' used on declarations in some headers (SDL 1.2), looks like gcc ignores it
		x += 1

		'' Some of these attributes accept further arguments which we
		'' can just ignore.
		skipToCommaOrRparen()

	case "cdecl"     : gccattribs or= ASTATTRIB_CDECL     : x += 1
	case "stdcall"   : gccattribs or= ASTATTRIB_STDCALL   : x += 1
	case "packed"    : gccattribs or= ASTATTRIB_PACKED    : x += 1
	case "dllimport" : gccattribs or= ASTATTRIB_DLLIMPORT : x += 1
	case else
		showError("unknown attribute '" + *tk->spellId(x) + "'")
	end select
end sub

sub CParser.parseGccAttributeList(byref gccattribs as integer)
	while parseok
		select case tk->get(x)
		case KW_VOLATILE, KW_INLINE, KW___INLINE, KW___INLINE__
			x += 1

		'' __attribute__((...)):
		'' __ATTRIBUTE__ '((' Attribute (',' Attribute)* '))'
		case KW___ATTRIBUTE, KW___ATTRIBUTE__
			x += 1

			'' '('?
			expectMatch(TK_LPAREN, "as 1st '(' in '__attribute__((...))'")

			'' '('?
			expectMatch(TK_LPAREN, "as 2nd '(' in '__attribute__((...))'")

			'' Attribute (',' Attribute)*
			do
				'' ')'?
				if tk->get(x) = TK_RPAREN then exit do

				'' Attribute
				parseGccAttribute(gccattribs)

				'' ','?
			loop while match(TK_COMMA) and parseok

			'' ')'?
			expectMatch(TK_RPAREN, "as 1st ')' in '__attribute__((...))'")

			'' ')'?
			expectMatch(TK_RPAREN, "as 2nd ')' in '__attribute__((...))'")

		case else
			exit while
		end select
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Enum constant: Identifier ['=' Expr] (',' | '}')
function CParser.parseEnumConst() as AstNode ptr
	'' Identifier
	if tk->get(x) <> TK_ID then
		showError("expected identifier for an enum constant" + tk->butFound(x))
		exit function
	end if
	var enumconst = astNew(ASTKIND_CONST, tk->spellId(x))
	enumconst->attrib or= ASTATTRIB_ENUMCONST
	x += 1

	'' '='?
	if match(TK_EQ) then
		'' Expr
		enumconst->expr = parseExpr(FALSE, FALSE)
	end if

	'' (',' | '}')
	select case tk->get(x)
	case TK_COMMA
		x += 1

	case TK_RBRACE

	case else
		showError("expected ',' or '}' behind enum constant" + tk->butFound(x))
	end select

	function = enumconst
end function

'' {STRUCT|UNION|ENUM} [Identifier] '{' StructBody|EnumBody '}'
'' {STRUCT|UNION|ENUM} Identifier
function CParser.parseTag() as AstNode ptr
	'' {STRUCT|UNION|ENUM}
	dim as integer astkind
	select case tk->get(x)
	case KW_UNION
		astkind = ASTKIND_UNION
	case KW_ENUM
		astkind = ASTKIND_ENUM
	case else
		assert(tk->get(x) = KW_STRUCT)
		astkind = ASTKIND_STRUCT
	end select
	x += 1

	'' __attribute__((...))
	dim gccattrib as integer
	parseGccAttributeList(gccattrib)

	'' [Identifier]
	dim tagid as zstring ptr
	if tk->get(x) = TK_ID then
		tagid = tk->spellId(x)
		x += 1
	end if

	'' '{'?
	if tk->get(x) = TK_LBRACE then
		var udt = astNew(astkind, tagid)
		udt->attrib or= gccattrib

		select case astkind
		case ASTKIND_STRUCT, ASTKIND_UNION
			var maxalign = pragmapack.stack(pragmapack.level)

			'' Preserve alignment if needed so we can emit FIELD = N,
			'' but not if N >= 8, because FB has no alignment > 8,
			'' so FIELD >= 8 is useless. Omitting it improves merging
			'' for some bindings.
			if (maxalign > 0) and (maxalign < 8) then
				udt->maxalign = maxalign
			end if
		end select

		'' '{'
		x += 1

		'' Parse struct/union/enum body
		udt->append(parseBody(astkind))

		'' '}'
		expectMatch(TK_RBRACE, "to close " + udt->dumpPrettyDecl(false) + " block")

		'' __attribute__((...))
		parseGccAttributeList(udt->attrib)

		function = udt
	else
		'' It's just a type name, not an UDT body - can't be anonymous
		if tagid = NULL then
			showError("expected '{' or tag name" + tk->butFound(x))
			tagid = @"<error-recovery>"
		end if

		var n = astNewTEXT(tagid)
		n->attrib or= ASTATTRIB_TAGID
		function = n
	end if
end function

function CParser.parseTypedef() as AstNode ptr
	'' TYPEDEF
	x += 1
	function = parseDecl(ASTKIND_TYPEDEF, 0)
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub CParser.turnIntoUNKNOWN(byval n as AstNode ptr, byval first as integer, byval last as integer)
	n->kind = ASTKIND_UNKNOWN
	n->setText(tk->spell(first, last))
	n->removeChildren()
	delete n->expr
	n->expr = NULL
	n->setType(TYPE_NONE, NULL)
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
private function hUnwrapToplevelCommas(byval n as AstNode ptr) as AstNode ptr
	if n->kind = ASTKIND_CCOMMA then
		var l = n->head
		var r = n->tail
		n->unlink(l)
		n->unlink(r)
		delete n
		function = astNewGROUP(hUnwrapToplevelCommas(l), hUnwrapToplevelCommas(r))
	else
		function = n
	end if
end function

private sub hHandleToplevelAssign(byval n as AstNode ptr)
	'' C assignment expression is top-most expression?
	'' Can be translated to FB assignment statement easily.
	'' (unlike C assignments nested deeper in expressions etc.)
	#macro remap(id)
		case ASTKIND_C##id : n->kind = ASTKIND_##id
	#endmacro
	select case n->kind
	remap(ASSIGN)
	remap(SELFOR)
	remap(SELFXOR)
	remap(SELFAND)
	remap(SELFSHL)
	remap(SELFSHR)
	remap(SELFADD)
	remap(SELFSUB)
	remap(SELFMUL)
	remap(SELFDIV)
	remap(SELFMOD)
	end select
end sub

''
'' Turn toplevel C comma/assignment expressions into FB statements
''
'' If it's inside a macro body, then we wrap it in a scope block, to enforce its
'' use as statement, not expression. (otherwise, for example, assignments could
'' be mis-used as comparisons)
''
private function hTryToFixCommasAndAssigns(byval n as AstNode ptr) as AstNode ptr
	'' Commas (first -- in case they contain assignments that will become
	'' toplevel ones after unwrapping)
	n = hUnwrapToplevelCommas(n)

	'' Assignments
	if n->kind = ASTKIND_GROUP then
		var i = n->head
		while i
			hHandleToplevelAssign(i)
			i = i->nxt
		wend
	else
		hHandleToplevelAssign(n)
	end if

	function = n
end function

sub CParser.showErrorForRemainingCommasOrAssigns(byval n as AstNode ptr)
	if n->contains(ASTKIND_CCOMMA) then
		showError("can't auto-translate C comma operator here [yet]")
	end if
	if n->containsCAssignments() then
		showError("can't auto-translate C assignment operator here [yet]")
	end if
end sub

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
function CParser.defineBodyLooksLikeScopeBlock(byval y as integer) as integer
	'' '{'
	assert(tk->get(y) = TK_LBRACE)
	y += 1

	'' Any keyword? (then it's likely not an expression, as most C keywords
	'' are for statements...)
	select case tk->get(y)
	case KW_SIZEOF
		'' sizeof() is an exception: a keyword, but for expressions, not statements
	case is > TK_ID
		return TRUE
	end select

	do
		select case tk->get(y)
		case TK_SEMI
			return TRUE

		case TK_EOL, TK_RBRACE
			exit do

		case TK_LPAREN, TK_LBRACKET, TK_LBRACE
			y = tk->findClosingParen(y, TRUE, TRUE)
		end select

		y += 1
	loop

	function = FALSE
end function

function CParser.parseDefineBodyTokenLiteral() as string
	dim astkind as integer
	select case as const tk->get(x)
	case TK_NUMBER             : astkind = ASTKIND_CONSTI
	case TK_STRING, TK_WSTRING : astkind = ASTKIND_STRING
	case TK_CHAR, TK_WCHAR     : astkind = ASTKIND_CHAR
	case else : assert(FALSE)
	end select
	var n = parseLiteral(astkind, TRUE)
	function = emitFbExpr(n)
	delete n
end function

function CParser.parseDefineBodyToken() as string
	select case as const tk->get(x)
	case TK_NUMBER, TK_STRING, TK_WSTRING, TK_CHAR, TK_WCHAR
		function = parseDefineBodyTokenLiteral()

	case TK_EXCLEQ    : function = "<>"      : x += 1  '' !=
	case TK_PERCENT   : function = "mod"     : x += 1  '' %
	case TK_PERCENTEQ : function = "mod="    : x += 1  '' %=
	case TK_AMP       : function = "and"     : x += 1  '' &
	case TK_AMPEQ     : function = "and="    : x += 1  '' &=
	case TK_AMPAMP    : function = "andalso" : x += 1  '' &&
	case TK_LTLT      : function = "shl"     : x += 1  '' <<
	case TK_LTLTEQ    : function = "shl="    : x += 1  '' <<=
	case TK_EQEQ      : function = "="       : x += 1  '' ==
	case TK_GTGT      : function = "shr"     : x += 1  '' >>
	case TK_GTGTEQ    : function = "shr="    : x += 1  '' >>=
	case TK_CIRC      : function = "xor"     : x += 1  '' ^
	case TK_CIRCEQ    : function = "xor="    : x += 1  '' ^=
	case TK_PIPE      : function = "or"      : x += 1  '' |
	case TK_PIPEEQ    : function = "or="     : x += 1  '' |=
	case TK_PIPEPIPE  : function = "orelse"  : x += 1  '' ||
	case TK_TILDE     : function = "not"     : x += 1  '' ~

	case else
		function = tk->spell(x)
		x += 1
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

function CParser.parseDefineBodyTokens() as string
	dim s as string

	var begin = x
	while tk->get(x) <> TK_EOL

		assert(tk->get(x) <> TK_EOF)

		if begin < x then
			if hSeparateBySpace(tk->get(x - 1), tk->get(x)) then
				s += " "
			end if
		end if

		s += parseDefineBodyToken()
	wend

	function = s
end function

'' Return value: whether to keep the #define
function CParser.parseDefineBody(byval macro as AstNode ptr) as integer
	'' If -convbodytokens was given for this #define, then don't try to parse the #define body,
	'' but just convert the tokens individually from C to FB.
	if api->idopt(OPT_CONVBODYTOKENS).matches(macro->text) then
		'' TODO: should use emit.TokenBuffer, so hlAutoRenameConflictingMacroParams() could work
		var body = parseDefineBodyTokens()
		if len(body) > 0 then
			macro->expr = astNewTEXT(body)
		end if
		assert(tk->get(x) = TK_EOL)
		return TRUE
	end if

	select case tk->get(x)
	'' Don't preserve #define if it just contains _Pragma's
	'' _Pragma("...")
	case KW__PRAGMA
		do
			'' _Pragma
			x += 1

			'' '('
			if tk->get(x) <> TK_LPAREN then exit do
			x += 1

			'' Skip to ')' - we don't care whether there is
			'' a string literal or something like a #macroparam or similar...
			skipToCommaOrRparen()
			x += 1
		loop while tk->get(x) = KW__PRAGMA

		exit function

	case KW___ATTRIBUTE, KW___ATTRIBUTE__
		'' Don't preserve #define if it just contains an __attribute__
		parseGccAttributeList(0)
		exit function

	'' '{'
	case TK_LBRACE
		if defineBodyLooksLikeScopeBlock(x) then
			macro->expr = parseScope()
		else
			macro->expr = parseInit(TRUE)
		end if
		return TRUE

	'' Just a 'const'? It's common to have a #define for the const keyword
	'' in C headers...
	case KW_CONST
		if tk->get(x + 1) = TK_EOL then
			'' const
			x += 1
			exit function
		end if

	case KW_IF
		macro->expr = parseIfBlock()
		return TRUE

	case KW_DO
		macro->expr = parseDoWhile(TRUE)
		return TRUE

	case KW_WHILE
		macro->expr = parseWhile()
		return TRUE
	end select

	if isDataTypeOrAttribute(x) then
		macro->expr = parseDataType()
		return TRUE
	end if

	macro->expr = hTryToFixCommasAndAssigns(parseExpr(FALSE, TRUE))

	select case macro->expr->kind
	case ASTKIND_GROUP, ASTKIND_ASSIGN, _
	     ASTKIND_SELFOR, ASTKIND_SELFXOR, ASTKIND_SELFAND, _
	     ASTKIND_SELFSHL, ASTKIND_SELFSHR, _
	     ASTKIND_SELFADD, ASTKIND_SELFSUB, _
	     ASTKIND_SELFMUL, ASTKIND_SELFDIV, ASTKIND_SELFMOD
		macro->expr = astNew(ASTKIND_SCOPEBLOCK, macro->expr)
	end select

	function = TRUE
end function

function CParser.defBodyContainsIds(byval y as integer) as integer
	do
		assert(tk->get(y) <> TK_EOF)
		select case tk->get(y)
		case TK_EOL
			exit do
		case TK_ID
			return TRUE
		end select
		y += 1
	loop
end function

sub CParser.parseDefBody(byval n as AstNode ptr, byval xbegin as integer, byref add_to_ast as integer)
	parentdefine = n

	'' Body
	add_to_ast and= parseDefineBody(n)

	'' Didn't reach EOL? Then the beginning of the macro body could
	'' be parsed as expression, but not the rest.
	assert(tk->get(x) <> TK_EOF)
	if tk->get(x) <> TK_EOL then
		showError("failed to parse full #define body")
		x = tk->skipToEol(x)
	end if

	if n->expr then
		showErrorForRemainingCommasOrAssigns(n->expr)
	end if

	'' If parsing the body failed, turn the PPDEFINE into an UNKNOWN without
	'' reallocating it (as it may already be linked into the AST).
	if parseok = FALSE then
		turnIntoUNKNOWN(n, xbegin, x)
		parseok = TRUE
	end if

	parentdefine = NULL
end sub

function CParser.parseDefine() as AstNode ptr
	'' define
	var defbegin = x - 1
	x += 1

	'' Identifier ['(' ParameterList ')']
	var macro = hDefineHead(*tk, x)

	'' Body?
	var add_to_ast = TRUE
	assert(macro->expr = NULL)
	if tk->get(x) <> TK_EOL then
		if defBodyContainsIds(x) then
			'' Delay parsing, until we've parsed all declarations in the input.
			'' This way we have more knowledge about typedefs etc. which could
			'' help parsing this #define body.
			addDefBody(defbegin, x, macro)
			x = tk->skipToEol(x)

			'' parseBody() mustn't delete the PPDEFINE node now that
			'' we're referencing it from the c.defbodies list
			assert(parseok)
		else
			'' Probably a simple #define body, parse right now
			parseDefBody(macro, defbegin, add_to_ast)
		end if
	end if

	'' Eol
	assert(tk->get(x) = TK_EOL)
	x += 1

	if add_to_ast = FALSE then
		delete macro
		macro = astNewGROUP()
	end if
	function = macro
end function

function CParser.parseUndef() as AstNode ptr
	'' undef
	x += 1

	'' id
	assert(tk->get(x) >= TK_ID)
	function = astNew(ASTKIND_UNDEF, tk->spellId(x))
	x += 1

	'' Eol
	assert(tk->get(x) = TK_EOL)
	x += 1
end function

function CParser.parseInclude() as AstNode ptr
	x += 1

	'' "filename" | <filename>
	'' TODO: Don't evaluate escape sequences in "filename"
	'' TODO: Don't evaluate escape sequences/comments in <filename>
	dim filename as string
	if tk->get(x) = TK_LT then
		'' <filename>

		'' Skip tokens until the '>'
		var begin = x
		do
			x += 1
			assert((tk->get(x) <> TK_EOL) and (tk->get(x) <> TK_EOF))
		loop until tk->get(x) = TK_GT

		'' Then spell them to get the filename
		filename = tk->spell(begin + 1, x - 1)
		x += 1
	else
		'' "filename"
		assert(tk->get(x) = TK_STRING)
		var s = parseLiteral(ASTKIND_STRING, FALSE)
		filename = *s->text
		delete s
	end if

	'' Eol
	assert(tk->get(x) = TK_EOL)
	x += 1

	function = astNew(ASTKIND_PPINCLUDE, filename)
end function

function CParser.parsePragmaPackNumber() as integer
	var n = parseLiteral(ASTKIND_CONSTI, TRUE)
	if n->kind <> ASTKIND_CONSTI then
		exit function
	end if
	pragmapack.stack(pragmapack.level) = n->evalConstiAsInt64()
	delete n
	function = TRUE
end function

function CParser.parsePragmaPack() as AstNode ptr
	'' pack
	assert(tk->get(x) = TK_ID)
	assert(tk->spell(x) = "pack")
	x += 1

	'' '('
	expectMatch(TK_LPAREN, "as in '#pragma pack(...)'")

	select case tk->get(x)
	'' #pragma pack(N): Set max alignment for current top of stack
	case TK_NUMBER
		if parsePragmaPackNumber() = FALSE then
			exit function
		end if

	'' #pragma pack(push, N)
	'' #pragma pack(pop)
	case TK_ID
		select case *tk->spellId(x)
		case "push"
			pragmapack.level += 1
			if pragmapack.level >= pragmapack.MAXLEVEL then
				oops("#pragma pack stack too small")
			end if
			resetPragmaPack()
			x += 1

			'' ','
			expectMatch(TK_COMMA, "behind 'push'")

			'' 'N'
			if tk->get(x) <> TK_NUMBER then
				exit function
			end if
			if parsePragmaPackNumber() = FALSE then
				exit function
			end if

		case "pop"
			if pragmapack.level > 0 then
				pragmapack.level -= 1
			else
				showError("#pragma pack(pop) without previous push")
			end if
			x += 1

		case else
			exit function
		end select

	'' #pragma pack(): Reset top of stack to default
	case TK_RPAREN
		resetPragmaPack()

	case else
		exit function
	end select

	'' ')'
	expectMatch(TK_RPAREN, "as in '#pragma pack(...)'")

	'' Eol
	assert(tk->get(x) = TK_EOL)
	x += 1

	'' Don't preserve the directive
	function = astNewGROUP()
end function

'' #pragma comment(lib, "...")
function CParser.parsePragmaComment() as AstNode ptr
	'' comment
	assert(tk->get(x) = TK_ID)
	assert(tk->spell(x) = "comment")
	x += 1

	'' '('
	assert(tk->get(x) = TK_LPAREN)
	x += 1

	'' lib
	assert(tk->get(x) = TK_ID)
	assert(tk->spell(x) = "lib")
	x += 1

	'' ','
	assert(tk->get(x) = TK_COMMA)
	x += 1

	'' "<library-file-name>"
	assert(tk->get(x) = TK_STRING)
	dim libname as string
	scope
		var s = parseLiteral(ASTKIND_STRING, TRUE)
		libname = *s->text
		delete s
	end scope

	'' ')'
	assert(tk->get(x) = TK_RPAREN)
	x += 1

	assert(tk->get(x) = TK_EOL)
	x += 1

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

	function = astNew(ASTKIND_INCLIB, libname)
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
sub CParser.parseBaseType _
	( _
		byref dtype as integer, _
		byref subtype as AstNode ptr, _
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

	while parseok
		'' __ATTRIBUTE__((...))
		parseGccAttributeList(gccattribs)

		select case tk->get(x)
		case KW_SIGNED
			if unsignedmods > 0 then
				showError("mixed SIGNED with previous UNSIGNED modifier")
			end if
			signedmods += 1

		case KW_UNSIGNED
			if signedmods > 0 then
				showError("mixed UNSIGNED with previous SIGNED modifier")
			end if
			unsignedmods += 1

		case KW_CONST
			constmods += 1

		case KW_SHORT
			if longmods > 0 then
				showError("mixed SHORT with previous LONG modifier")
			end if
			shortmods += 1
			if shortmods > 1 then
				showError("more than 1 SHORT modifier")
			end if

		case KW_LONG
			if shortmods > 0 then
				showError("mixed LONG with previous SHORT modifier")
			end if
			longmods += 1
			if longmods > 2 then
				showError("more than 2 LONG modifiers")
			end if

		case else
			'' Only one base type is allowed
			if dtype <> TYPE_NONE then
				exit while
			end if

			select case tk->get(x)
			case KW_ENUM, KW_STRUCT, KW_UNION
				dtype = TYPE_UDT
				subtype = parseTag()
				is_tag = TRUE
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
				if signedmods or unsignedmods or longmods or shortmods then
					'' Then don't treat this id as the type
					exit while
				end if

				'' Treat the id as the type
				var id = tk->spellId(x)
				dtype = lookupExtraDataType(id)
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

		x += 1
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
			dtype = typeGetCLong(unsignedmods > 0, api->clong32)
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
			showError("expected a data type" + tk->butFound(x))
		end if
	end select

	select case dtype
	case TYPE_ANY, TYPE_SINGLE, TYPE_DOUBLE, TYPE_UDT
		if signedmods or unsignedmods or shortmods or longmods then
			showError("SIGNED|UNSIGNED|SHORT|LONG modifiers used with void/float/double/typedef/UDT")
		end if
	case TYPE_ZSTRING, TYPE_BYTE, TYPE_UBYTE
		if shortmods or longmods then
			showError("SHORT|LONG modifiers used with CHAR type")
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
	parseGccAttributeList(gccattribs)
end sub

'' ParamDeclList = ParamDecl (',' ParamDecl)*
'' ParamDecl = '...' | Decl{Param}
function CParser.parseParamDeclList() as AstNode ptr
	var group = astNewGROUP()

	do
		dim as AstNode ptr t

		'' '...'?
		if tk->get(x) = TK_ELLIPSIS then
			t = astNew(ASTKIND_PARAM)
			x += 1
		else
			t = parseDecl(ASTKIND_PARAM, 0)
		end if

		group->append(t)

		'' ','?
	loop while match(TK_COMMA) and parseok

	function = group
end function

private function hCanHaveInit(byval n as AstNode ptr) as integer
	select case n->kind
	case ASTKIND_PARAM
		function = TRUE
	case ASTKIND_VAR
		function = ((n->attrib and ASTATTRIB_EXTERN) = 0)
	end select
end function

private function hHasVarargParam(byval proc as AstNode ptr) as integer
	var param = proc->head
	while param
		if param->dtype = TYPE_NONE then
			return TRUE
		end if
		param = param->nxt
	wend
	function = FALSE
end function

sub CParser.postprocessDeclarator(byval n as AstNode ptr)
	if n->kind = ASTKIND_PROC then
		'' Ignore extern on procedures, not needed explicitly
		n->attrib and= not ASTATTRIB_EXTERN

		'' Default to cdecl, if there's no explicit callconv attribute yet
		if (n->attrib and ASTATTRIB__CALLCONV) = 0 then
			n->attrib or= ASTATTRIB_CDECL

		'' And show an error if conflicting attributes were given
		'' (perhaps we just made a mistake assigning them - better safe...)
		elseif (n->attrib and ASTATTRIB__CALLCONV) = ASTATTRIB__CALLCONV then
			showError("cdecl/stdcall attributes specified together")
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
	if n->subtype then postprocessDeclarator(n->subtype)
end sub

''
'' Declarator =
''    GccAttributeList
''    ('*' [CONST|GccAttributeList])*
''    ['&']
''    { [Identifier] | '(' Declarator ')' }
''    { '(' ParamList ')' | ('[' ArrayElements ']')* }
''    [ '=' Init ]
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
''
'' t = The whole declaration's AST. Typically this starts out as a VAR.
'' node = The AstNode at the current recursion level. If we find function
'' parameters or an array size, they go to this node.
'' If the declarator is a plain VAR, function parameters turn it into a function.
'' But if it's a pointer, it becomes a function pointer VAR. Then t stays the same,
'' but "node" starts pointing to the procptr subtype, which receives the function parameters,
'' and callconv attributes, etc.
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
function CParser.parseDeclarator _
	( _
		byval nestlevel as integer, _
		byval astkind as integer, _
		byval outerdtype as integer, _
		byval basesubtype as AstNode ptr, _
		byval basegccattribs as integer, _
		byref node as AstNode ptr, _
		byref procptrdtype as integer, _
		byref gccattribs as integer _
	) as AstNode ptr

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
	parseGccAttributeList(gccattribs)

	'' Pointers: ('*')*
	while match(TK_STAR) and parseok
		if (typeGetPtrCount(procptrdtype) = TYPEMAX_PTR) or _
		   (typeGetPtrCount(dtype       ) = TYPEMAX_PTR) then
			showError("too many pointers")
			exit while
		end if

		procptrdtype = typeAddrOf(procptrdtype)
		dtype = typeAddrOf(dtype)

		'' (CONST|RESTRICT|__ATTRIBUTE__((...)))*
		while parseok
			'' __ATTRIBUTE__((...))
			parseGccAttributeList(gccattribs)

			select case tk->get(x)
			case KW_CONST
				procptrdtype = typeSetIsConst(procptrdtype)
				dtype = typeSetIsConst(dtype)
				x += 1

			case KW_RESTRICT, KW___RESTRICT, KW___RESTRICT__
				'' The restrict keyword is not interesting for FB bindings, just ignore
				x += 1

			case else
				exit while
			end select
		wend
	wend

	'' Reference: '&'
	if match(TK_AMP) then
		procptrdtype = typeSetIsRef(procptrdtype)
		dtype = typeSetIsRef(dtype)
	end if

	dim as AstNode ptr t, innernode

	''    '(' Declarator ')'    |    [Identifier]

	'' Special case for parameters:
	'' * They can be anonymous, and thus a '(' can indicate either a
	''   parenthesized identifier or a parameter list. It depends on the
	''   token behind the '(' - if it's a data type or a ')' then it's a
	''   parameter list.
	'' * If it's a parameter list, it can be parenthesized, even multiple
	''   times. This isn't possible with "normal" function declarations...
	var paramlistnesting = 0
	if astkind = ASTKIND_PARAM then
		var y = x
		while tk->get(y) = TK_LPAREN
			y += 1
		wend
		if isDataType(y) or (tk->get(y) = TK_RPAREN) then
			paramlistnesting = y - x
		end if
	end if

	'' '(' for declarator?
	if (tk->get(x) = TK_LPAREN) and (paramlistnesting = 0) then
		x += 1

		t = parseDeclarator(nestlevel + 1, astkind, dtype, basesubtype, 0, innernode, innerprocptrdtype, innergccattribs)

		'' ')'
		expectMatch(TK_RPAREN, "for '(...)' parenthesized declarator")
	else
		'' [Identifier]
		'' An identifier must exist, except for parameters/types, and
		'' in fact for types there mustn't be an id.
		dim id as zstring ptr
		if astkind <> ASTKIND_DATATYPE then
			if tk->get(x) = TK_ID then
				id = tk->spellId(x)
				x += 1
			else
				if astkind <> ASTKIND_PARAM then
					showError("expected identifier for the symbol declared in this declaration" + tk->butFound(x))
					id = @"<error-recovery>"
				end if
			end if
		end if

		t = astNew(astkind, id)
		t->setType(dtype, basesubtype)
	end if

	select case tk->get(x)
	'' ('[' [ArrayElements] ']')*
	case TK_LBRACKET
		node = t

		'' Can't allow arrays on everything - currently, it's only
		'' handled for vars/fields/params/typedefs
		if node->kind = ASTKIND_DATATYPE then
			showError("TODO: arrays not supported here yet")
		end if

		assert(node->array = NULL)
		node->array = astNew(ASTKIND_ARRAY)

		'' For each array dimension...
		do
			'' '['
			x += 1

			var d = astNew(ASTKIND_DIMENSION)

			'' Just '[]'?
			if tk->get(x) = TK_RBRACKET then
				d->expr = astNew(ASTKIND_ELLIPSIS)
			else
				d->expr = parseExpr(TRUE, FALSE)
			end if

			node->array->append(d)

			'' ']'
			expectMatch(TK_RBRACKET, "to close this array dimension declaration")

			'' '['? (next dimension)
		loop while (tk->get(x) = TK_LBRACKET) and parseok

		if innerprocptrdtype <> TYPE_PROC then
			'' It's a pointer to an array - unsupported in FB.
			'' Drop the array type, effectively making it a pointer to just one array element.
			'' see also expandArrayTypedef()
			delete node->array
			node->array = NULL
		end if

	'' ':' <bits>
	case TK_COLON
		node = t

		if (innerprocptrdtype <> TYPE_PROC) or (node->kind <> ASTKIND_FIELD) then
			showError("bitfields not supported here")
		end if
		x += 1

		node->bits = parseExpr(FALSE, FALSE)

	'' '(' ParamList ')'
	case TK_LPAREN
		if paramlistnesting = 0 then
			paramlistnesting = 1
		end if
		x += paramlistnesting

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
			node = astNew(ASTKIND_PROC)
			node->setType(dtype, basesubtype)

			'' Turn the object into a function pointer
			delete innernode->subtype
			innernode->dtype = innerprocptrdtype
			innernode->subtype = node

			innerprocptrdtype = TYPE_PROC
		else
			select case t->kind
			'' A plain symbol, not a pointer, becomes a function
			case ASTKIND_VAR, ASTKIND_FIELD
				node = t
				t->kind = ASTKIND_PROC

			'' Anything else though (typedefs, params, type casts...)
			'' with params isn't turned into a proc, but just has function type.
			case else
				node = astNew(ASTKIND_PROC)
				node->setType(dtype, basesubtype)

				delete t->subtype
				t->dtype = TYPE_PROC
				t->subtype = node
			end select
		end if

		'' Just '(void)'?
		if (tk->get(x) = KW_VOID) and (tk->get(x + 1) = TK_RPAREN) then
			'' VOID
			x += 1
		'' Not just '()'?
		elseif tk->get(x) <> TK_RPAREN then
			assert(node->kind = ASTKIND_PROC)
			node->append(parseParamDeclList())
		end if

		'' ')'
		while paramlistnesting > 0
			expectMatch(TK_RPAREN, "to close parameter list in function declaration")
			paramlistnesting -= 1
		wend
	case else
		node = t
	end select

	'' __ATTRIBUTE__((...))
	var endgccattribs = 0
	parseGccAttributeList(endgccattribs)

	if nestlevel > 0 then
		'' __attribute__'s from this level should always be passed up
		gccattribs or= endgccattribs

		'' Pass innerprocptrdtype and innergccattribs up again if they
		'' weren't used up on this level
		if innerprocptrdtype <> TYPE_PROC then
			gccattribs or= innergccattribs
			procptrdtype = typeExpand(innerprocptrdtype, procptrdtype)
			if procptrdtype = TYPE_NONE then
				showError("too many pointers, or ref to ref")
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

		if (typeGetDt(t->dtype) = TYPE_PROC) and (t->kind <> ASTKIND_PROC) then
			assert(t->subtype->kind = ASTKIND_PROC)
			t->subtype->attrib or= basegccattribs and ASTATTRIB__CALLCONV
			t->attrib or= basegccattribs and (not ASTATTRIB__CALLCONV)
		else
			t->attrib or= basegccattribs
		end if

		node->attrib or= innergccattribs

		'' dllimport implies extern, and isn't allowed together with static
		if t->attrib and ASTATTRIB_DLLIMPORT then
			if t->attrib and ASTATTRIB_STATIC then
				showError("static dllimport")
				t->attrib and= not ASTATTRIB_STATIC
			end if
			t->attrib or= ASTATTRIB_EXTERN
		end if

		postprocessDeclarator(t)
	end if

	function = t
end function

'' Data type parsing for cast operations and sizeof():
''    BaseType Declarator
'' Parsing just the base type isn't enough, because it could be a function
'' pointer cast with parameter list etc. We need to do full declarator parsing
'' to handle that.
function CParser.parseDataType() as AstNode ptr
	dim as integer dtype, gccattribs
	dim as AstNode ptr subtype
	parseBaseType(dtype, subtype, gccattribs, FALSE)

	'' Disallow UDT bodies in type "expressions" - FB doesn't support it,
	'' and our highlevel passes/code emitter don't expect it.
	if subtype then
		select case subtype->kind
		case ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM
			showError("UDT in datatype expression; not supported in FB")
			dtype = TYPE_INTEGER
			delete subtype
			subtype = NULL
		end select
	end if

	function = parseDeclarator(0, ASTKIND_DATATYPE, dtype, subtype, gccattribs, NULL, 0, 0)
end function

type TypeNameRefUpdater extends AstVisitor
	as string oldid, newid
	declare function visit(byref n as AstNode) as integer override
end type

function TypeNameRefUpdater.visit(byref n as AstNode) as integer
	if typeGetDt(n.dtype) = TYPE_UDT then
		assert(astIsTEXT(n.subtype))
		if *n.subtype->text = oldid then
			n.subtype->setText(newid)
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
private sub hUnscopeNestedNamedUdts(byval result as AstNode ptr, byval udt as AstNode ptr)
	var i = udt->head
	while i
		var nxt = i->nxt

		select case i->kind
		case ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM
			if i->text then
				if udt->text then
					'' If the nested UDT was given an auto-generated name,
					'' prepend the parent UDT name as "namespace".
					'' All references to the old name must be updated.
					if i->attrib and ASTATTRIB_GENERATEDID then
						dim updater as TypeNameRefUpdater
						updater.oldid = *i->text
						updater.newid = *udt->text + "_" + *i->text
						udt->visit(updater)
						i->setText(updater.newid)
					end if
				end if

				udt->unlink(i)
				result->append(i)
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
'' Decl = GccAttributeList BaseType Declarator (',' Declarator)* [';']
''
function CParser.parseDecl(byval astkind as integer, byval gccattribs as integer) as AstNode ptr
	assert(astkind <> ASTKIND_DATATYPE)

	'' BaseType
	dim as integer dtype, is_tag
	dim as AstNode ptr subtype
	parseBaseType(dtype, subtype, gccattribs, is_tag)

	var result = astNewGROUP()

	'' Special case for standalone struct/union/enum declarations (even with CONST bits):
	if (typeGetDtAndPtr(dtype) = TYPE_UDT) and is_tag then
		'' Tag body?
		''    STRUCT|UNION|ENUM [Identifier] '{' ... '}' ';'
		select case subtype->kind
		case ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM
			'' ';'?
			if match(TK_SEMI) then
				hUnscopeNestedNamedUdts(result, subtype)
				result->append(subtype)
				return result
			end if

		'' Useless tag declaration?
		''    STRUCT|UNION|ENUM Identifier ';'
		case ASTKIND_TEXT
			'' ';'?
			if match(TK_SEMI) then
				'' Ignore & treat as no-op
				delete subtype
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
		select case subtype->kind
		case ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM
			var udt = subtype

			'' If the UDT is anonymous, we have to generate a name for it.
			'' (FB doesn't allow anonymous non-nested UDTs)
			if udt->text = NULL then
				'' Try to name it after the symbol declared in this declaration
				if tk->get(x) = TK_ID then
					udt->setText(tk->spellId(x))
				else
					'' Auto-generate id
					'' TODO:
					''  * avoid inter-binding conflicts
					''  * name after fields (name & type) instead of counter
					''  * If this is really only needed for anonymous parameters,
					''    then it's probably very rare in practice. No need to bother.
					udt->setText("_" + str(tempids))
					tempids += 1
				end if
				udt->attrib or= ASTATTRIB_GENERATEDID
			end if

			subtype = astNewTEXT(udt->text)
			subtype->attrib or= ASTATTRIB_TAGID or (udt->attrib and ASTATTRIB_GENERATEDID)

			hUnscopeNestedNamedUdts(result, udt)
			result->append(udt)
		end select
	end if

	var require_semi = TRUE

	'' ... (',' ...)*
	var declarator_count = 0
	do
		declarator_count += 1
		var n = parseDeclarator(0, astkind, dtype, subtype, gccattribs, NULL, 0, 0)
		result->append(n)

		if n->kind = ASTKIND_TYPEDEF then
			'' Register known typedefs, so we can disambiguate type casts
			addTypedef(n->text)
		end if

		select case tk->get(x)
		case KW_ASM, KW___ASM, KW___ASM__
			x += 1

			expectMatch(TK_LPAREN, "for asm()")

			if tk->get(x) = TK_STRING then
				var s = parseLiteral(ASTKIND_STRING, TRUE)
				n->takeAliasFromId(s)
				delete s
			else
				showError("expected ""name"" inside asm()")
			end if

			expectMatch(TK_RPAREN, "for asm()")
		end select

		if hCanHaveInit(n) then
			'' ['=' Init]
			if match(TK_EQ) then
				assert(n->expr = NULL)
				n->expr = parseExprOrInit(FALSE)

				'' If it's an array, then it must be an array initializer (or a string literal),
				'' not a struct initializer
				if n->array then
					if n->expr->kind = ASTKIND_STRUCTINIT then
						n->expr->kind = ASTKIND_ARRAYINIT
					end if
				end if
			end if
		end if

		'' Parameters can't have commas and more identifiers,
		'' and don't need the ';' either.
		if n->kind = ASTKIND_PARAM then
			require_semi = FALSE
			exit do
		end if

		'' '{', procedure body?
		if (n->kind = ASTKIND_PROC) and (tk->get(x) = TK_LBRACE) then
			'' A procedure with body must be the first and only
			'' declarator in the declaration.
			if declarator_count = 1 then
				var originalparseok = parseok

				assert(n->expr = NULL)
				n->expr = parseScope()

				if api->nofunctionbodies then
					parseok = originalparseok
					n->expr = NULL
				end if

				require_semi = FALSE
				exit do
			end if
		end if

		'' ','?
	loop while match(TK_COMMA) and parseok

	if require_semi then
		'' ';'
		expectMatch(TK_SEMI, "to finish this declaration")
	end if

	delete subtype
	function = result
end function

'' Variable/procedure declarations
''    GccAttributeList [EXTERN|STATIC] Decl
function CParser.parseVarOrProcDecl(byval is_local as integer) as AstNode ptr
	'' __ATTRIBUTE__((...))
	var gccattribs = 0
	parseGccAttributeList(gccattribs)

	'' [EXTERN|STATIC]
	select case tk->get(x)
	case KW_EXTERN
		gccattribs or= ASTATTRIB_EXTERN
		x += 1
	case KW_STATIC
		gccattribs or= ASTATTRIB_STATIC
		x += 1
	end select

	if is_local then
		gccattribs or= ASTATTRIB_LOCAL
	end if

	'' Decl. Assume that it's a variable for now; the declarator
	'' parser may turn it into a procedure if it has parameters.
	function = parseDecl(ASTKIND_VAR, gccattribs)
end function

'' Expression statement: Assignments, function calls, i++, etc.
function CParser.parseExprStatement() as AstNode ptr
	function = hTryToFixCommasAndAssigns(parseExpr(TRUE, FALSE))

	'' ';'?
	expectMatch(TK_SEMI, "(end of expression statement)")
end function

'' RETURN [Expr] ';'
function CParser.parseReturn() as AstNode ptr
	'' RETURN
	assert(tk->get(x) = KW_RETURN)
	x += 1

	var n = astNew(ASTKIND_RETURN)

	'' [Expr]
	if tk->get(x) <> TK_SEMI then
		n->append(parseExpr(TRUE, FALSE))
	end if

	'' ';'
	expectMatch(TK_SEMI, "(end of statement)")

	function = n
end function

'' '{ ... }' statement block
'' Using parseBody() to allow the constructs in this scope block to be parsed
'' separately. If we can't parse one of them, then only that one will become an
'' unknown construct. The rest of the scope can potentially be parsed fine.
function CParser.parseScope() as AstNode ptr
	'' '{'
	assert(tk->get(x) = TK_LBRACE)
	x += 1

	function = astNew(ASTKIND_SCOPEBLOCK, parseBody(ASTKIND_SCOPEBLOCK))

	'' '}'
	expectMatch(TK_RBRACE, "to close compound statement")
end function

function CParser.parseConditionExpr() as AstNode ptr
	'' The parentheses around the condition expression are part of the
	'' if/do/while/... syntax. They have to be parsed explicitly, not as
	'' part of the condition expression. Otherwise, code such as this:
	''    if (foo)
	''        (bar);
	'' could be mis-parsed as
	''    if (foo)(bar)
	''        ;

	'' '('
	expectMatch(TK_LPAREN, "in front of condition")

	'' condition expression
	function = parseExpr(TRUE, FALSE)

	'' ')'
	expectMatch(TK_RPAREN, "behind condition")
end function

'' IF '(' Expr ')' Construct [ELSE Construct]
function CParser.parseIfBlock() as AstNode ptr
	var ifblock = astNew(ASTKIND_IFBLOCK)

	'' IF
	assert(tk->get(x) = KW_IF)
	x += 1

	'' condition expression
	var ifpart = astNew(ASTKIND_IFPART)
	ifpart->expr = parseConditionExpr()

	'' if/true statement
	ifpart->append(parseConstruct(ASTKIND_SCOPEBLOCK))
	ifblock->append(ifpart)

	'' ELSE?
	if match(KW_ELSE) then
		'' else/false statement
		ifblock->append(astNew(ASTKIND_ELSEPART, parseConstruct(ASTKIND_SCOPEBLOCK)))
	end if

	function = ifblock
end function

'' DO Construct WHILE '(' Expr ')' [';']
function CParser.parseDoWhile(byval semi_is_optional as integer) as AstNode ptr
	var dowhile = astNew(ASTKIND_DOWHILE)

	'' DO
	assert(tk->get(x) = KW_DO)
	x += 1

	'' loop body
	dowhile->append(parseConstruct(ASTKIND_SCOPEBLOCK))

	'' WHILE
	expectMatch(KW_WHILE, "behind do loop body")

	'' loop condition expression
	dowhile->expr = parseConditionExpr()

	if semi_is_optional then
		match(TK_SEMI)
	else
		expectMatch(TK_SEMI, "behind do/while loop")
	end if

	function = dowhile
end function

'' WHILE '(' Expr ')' Construct
function CParser.parseWhile() as AstNode ptr
	var whileloop = astNew(ASTKIND_WHILE)

	'' WHILE
	assert(tk->get(x) = KW_WHILE)
	x += 1

	'' loop condition expression
	whileloop->expr = parseConditionExpr()

	'' loop body
	whileloop->append(parseConstruct(ASTKIND_SCOPEBLOCK))

	function = whileloop
end function

'' EXTERN "C" { ... }
function CParser.parseExternBlock() as AstNode ptr
	assert(tk->get(x) = KW_EXTERN)
	x += 1

	if tk->get(x) <> TK_STRING then
		showError("expected <""C""> behind <extern>")
		return astNewGROUP()
	end if
	scope
		var s = parseLiteral(ASTKIND_STRING, TRUE)
		if *s->text <> "C" then
			showError("expected <""C""> behind <extern>")
			return astNewGROUP()
		end if
		delete s
	end scope

	expectMatch(TK_LBRACE, "for <extern ""C""> block")

	function = parseBody(ASTKIND_EXTERNBLOCKBEGIN)

	expectMatch(TK_RBRACE, "for <extern ""C""> block")
end function

function CParser.parseConstruct(byval bodyastkind as integer) as AstNode ptr
	if tk->get(x) = TK_FBCODE then
		var n = astNew(ASTKIND_FBCODE, tk->getText(x))
		x += 1
		return n
	end if

	'' TODO: only parse #defines at toplevel, not inside structs etc.
	'' '#'?
	if (tk->get(x) = TK_HASH) and tk->isStartOfDirective(x) then
		x += 1

		dim directive as AstNode ptr
		if tk->get(x) = TK_ID then
			select case *tk->spellId(x)
			case "define"
				directive = parseDefine()
			case "undef"
				directive = parseUndef()
			case "include"
				directive = parseInclude()
			case "pragma"
				x += 1

				select case tk->spell(x)
				case "pack"
					directive = parsePragmaPack()
				case "comment"
					directive = parsePragmaComment()
				end select
			end select
		end if

		if directive = NULL then
			showError("unknown CPP directive")
			directive = astNewGROUP()
		end if

		return directive
	end if

	if bodyastkind = ASTKIND_ENUM then
		return parseEnumConst()
	end if

	select case tk->get(x)
	case KW_TYPEDEF
		return parseTypedef()
	case TK_SEMI
		'' Ignore standalone ';'
		x += 1
		return astNewGROUP()
	case TK_LBRACE : return parseScope()
	case KW_IF     : return parseIfBlock()
	case KW_DO     : return parseDoWhile(FALSE)
	case KW_WHILE  : return parseWhile()
	case KW_RETURN : return parseReturn()
	case KW_EXTERN
		if tk->get(x + 1) = TK_STRING then
			return parseExternBlock()
		end if
	end select

	select case bodyastkind
	case ASTKIND_STRUCT, ASTKIND_UNION
		'' Field declaration
		function = parseDecl(ASTKIND_FIELD, 0)
	case ASTKIND_SCOPEBLOCK
		'' Disambiguate: local declaration vs. expression
		'' If it starts with a data type, __attribute__, or 'static',
		'' then it must be a declaration.
		if (tk->get(x) = KW_STATIC) orelse isDataTypeOrAttribute(x) then
			function = parseVarOrProcDecl(TRUE)
		else
			function = parseExprStatement()
		end if
	case else
		function = parseVarOrProcDecl(FALSE)
	end select
end function

private sub hSetLocationIfNeeded(byval n as AstNode ptr, byval location as TkLocation)
	if n->location.value = 0 then
		n->location = location
	end if
end sub

function CParser.parseBody(byval bodyastkind as integer) as AstNode ptr
	var result = astNewGROUP()

	do
		select case tk->get(x)
		case TK_EOF
			exit do

		'' End of #define body
		case TK_EOL
			assert(parentdefine)
			exit do

		'' '}' (end of block)
		case TK_RBRACE
			if bodyastkind >= 0 then
				exit do
			end if
		end select

		var begin = x
		var t = parseConstruct(bodyastkind)

		showErrorForRemainingCommasOrAssigns(t)

		if parseok = FALSE then
			parseok = TRUE

			'' Skip current construct and preserve its tokens in
			'' an UNKNOWN node
			x = tk->skipConstruct(begin, FALSE)
			turnIntoUNKNOWN(t, begin, x - 1)
		end if

		'' Assign source location to declarations
		'' For toplevel declarations (not fields/parameters) this will
		'' be used to distribute them based on -emit patterns.
		var location = tk->getLocation(begin)
		if t->kind = ASTKIND_GROUP then
			var i = t->head
			while i
				hSetLocationIfNeeded(i, location)
				i->location = location
				i = i->nxt
			wend
		else
			hSetLocationIfNeeded(t, location)
		end if

		result->append(t)
	loop

	function = result
end function

function CParser.parseToplevel() as AstNode ptr
	var t = parseBody(-1)

	'' Process the #define bodies which weren't parsed yet
	for i as integer = 0 to defbodycount - 1
		with defbodies[i]
			parseok = TRUE
			x = .xbodybegin

			'' Parse #define body
			var add_to_ast = TRUE
			parseDefBody(.n, .xdefbegin, add_to_ast)

			if add_to_ast = FALSE then
				t->remove(.n)
			end if
		end with
	next

	function = t
end function

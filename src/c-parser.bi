#include once "ast.bi"
#include once "fbfrog-apiinfo.bi"

type PragmaPackStack
	const MAXLEVEL = 128
	stack(0 to MAXLEVEL-1) as integer
	level as integer
end type

type DEFBODYNODE
	xdefbegin	as integer  '' Begin of the #define
	xbodybegin	as integer  '' Begin of the #define's body
	n		as AstNode ptr  '' #define node
end type

type CParser
	tk as TokenBuffer ptr
	api as ApiInfo ptr
	as integer x, parseok, tempids
	parentdefine as AstNode ptr

	typedefs as THash = THash(8, FALSE)
	extradatatypehash as THash = THash(6, FALSE)

	'' #pragma pack stack
	pragmapack as PragmaPackStack

	defbodies as DEFBODYNODE ptr
	as integer defbodycount, defbodyroom

	declare function match(byval tk as integer) as integer
	declare sub showError(byref message as string)
	declare sub expectMatch(byval tk as integer, byref message as string)
	declare function isInsideDefineBody() as integer
	declare sub resetPragmaPack()
	declare function isTypedef(byval id as zstring ptr) as integer
	declare function lookupExtraDataType(byval id as zstring ptr) as integer
	declare function identifierIsMacroParam(byval id as zstring ptr) as integer
	declare constructor(byref tk as TokenBuffer, byref api as ApiInfo)
	declare destructor()
	declare sub addTypedef(byval id as const zstring ptr)
	declare sub addDefBody(byval xdefbegin as integer, byval xbodybegin as integer, byval n as AstNode ptr)
	declare function parseLiteral(byval astkind as integer, byval eval_escapes as integer) as AstNode ptr
	declare function parseStringLiteralSequence() as AstNode ptr
	declare function isDataType(byval y as integer) as integer
	declare function isDataTypeOrAttribute(byval y as integer) as integer
	declare function parseCall(byval functionexpr as AstNode ptr, byval allow_idseq as integer) as AstNode ptr
	declare function parseExprRecursive _
		( _
			byval level as integer, _
			byval parentheses as integer, _
			byval allow_toplevel_comma as integer, _
			byval allow_idseq as integer _
		) as AstNode ptr
	declare function parseExpr(byval allow_toplevel_comma as integer, byval allow_idseq as integer) as AstNode ptr
	declare function parseInit(byval allow_idseq as integer) as AstNode ptr
	declare function parseExprOrInit(byval allow_idseq as integer) as AstNode ptr
	declare sub skipToCommaOrRparen()
	declare sub parseGccAttribute(byref gccattribs as integer)
	declare sub parseGccAttributeList(byref gccattribs as integer)
	declare function parseEnumConst() as AstNode ptr
	declare function parseTag() as AstNode ptr
	declare function parseTypedef() as AstNode ptr
	declare sub turnIntoUNKNOWN(byval n as AstNode ptr, byval first as integer, byval last as integer)
	declare sub showErrorForRemainingCommasOrAssigns(byval n as AstNode ptr)
	declare function defineBodyLooksLikeScopeBlock(byval y as integer) as integer
	declare function parseDefineBodyTokenLiteral() as string
	declare function parseDefineBodyToken() as string
	declare function parseDefineBodyTokens() as string
	declare function parseDefineBody(byval macro as AstNode ptr) as integer
	declare function defBodyContainsIds(byval y as integer) as integer
	declare sub parseDefBody(byval n as AstNode ptr, byval xbegin as integer, byref add_to_ast as integer)
	declare function parseDefine() as AstNode ptr
	declare function parseUndef() as AstNode ptr
	declare function parseInclude() as AstNode ptr
	declare function parsePragmaPackNumber() as integer
	declare function parsePragmaPack() as AstNode ptr
	declare function parsePragmaComment() as AstNode ptr
	declare sub parseBaseType _
		( _
			byref dtype as integer, _
			byref subtype as AstNode ptr, _
			byref gccattribs as integer, _
			byref is_tag as integer _
		)
	declare function parseParamDeclList() as AstNode ptr
	declare sub postprocessDeclarator(byval n as AstNode ptr)
	declare function parseDeclarator _
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
	declare function parseDataType() as AstNode ptr
	declare function parseDecl(byval astkind as integer, byval gccattribs as integer) as AstNode ptr
	declare function parseVarOrProcDecl(byval is_local as integer) as AstNode ptr
	declare function parseExprStatement() as AstNode ptr
	declare function parseReturn() as AstNode ptr
	declare function parseScope() as AstNode ptr
	declare function parseConditionExpr() as AstNode ptr
	declare function parseIfBlock() as AstNode ptr
	declare function parseDoWhile(byval semi_is_optional as integer) as AstNode ptr
	declare function parseWhile() as AstNode ptr
	declare function parseExternBlock() as AstNode ptr
	declare function parseConstruct(byval bodyastkind as integer) as AstNode ptr
	declare function parseBody(byval bodyastkind as integer) as AstNode ptr
	declare function parseToplevel() as AstNode ptr

	declare operator let(byref as const CParser) '' unimplemented
end type

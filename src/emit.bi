#include once "ast.bi"
#include once "util-hash.bi"

namespace emit

enum
	TK_EOF
	TK_EOL
	TK_TAB
	TK_SPACE
	TK_NUMLIT
	TK_STRLIT
	TK_TEXT '' pre-rendered FB code
	TK_COMMENT

	TK_COLON    '' :
	TK_COMMA    '' ,
	TK_ELLIPSIS '' ...
	TK_DOT      '' .
	TK_ARROW    '' ->
	TK_HASHHASH '' ##
	TK_HASH     '' #
	TK_AT       '' @

	TK_PLUS     '' +
	TK_MINUS    '' -
	TK_STAR     '' *
	TK_SLASH    '' /

	TK_EQ       '' =
	TK_NE       '' <>
	TK_LT       '' <
	TK_LE       '' <=
	TK_GT       '' >
	TK_GE       '' >=

	TK_LPAREN   '' (
	TK_RPAREN   '' )
	TK_LBRACKET '' [
	TK_RBRACKET '' ]
	TK_LBRACE   '' {
	TK_RBRACE   '' }

	'' >= TK_ID: keywords/identifiers
	TK_ID

	KW_ALIAS
	KW_AND
	KW_ANDALSO
	KW_ANY
	KW_AS
	KW_ASC
	KW_BYTE
	KW_BYREF
	KW_BYVAL
	KW_CAST
	KW_CBYTE
	KW_CDBL
	KW_CDECL
	KW_CINT
	KW_CLNG
	KW_CLNGINT
	KW_CLONG
	KW_CLONGDOUBLE
	KW_CONST
	KW_CPTR
	KW_CSHORT
	KW_CSNG
	KW_CUBYTE
	KW_CUINT
	KW_CULNG
	KW_CULNGINT
	KW_CULONG
	KW_CUSHORT
	KW_DECLARE
	KW_DEFINE
	KW_DEFINED
	KW_DIM
	KW_DO
	KW_DOUBLE
	KW_ELSE
	KW_ELSEIF
	KW_END
	KW_ENDIF
	KW_ENDMACRO
	KW_ENUM
	KW_EXTERN
	KW_FIELD
	KW_FUNCTION
	KW_IF
	KW_IFDEF
	KW_IFNDEF
	KW_IIF
	KW_IMPORT
	KW_INCLIB
	KW_INCLUDE
	KW_INTEGER
	KW_LONG
	KW_LONGINT
	KW_LOOP
	KW_MACRO
	KW_MOD
	KW_NOT
	KW_ONCE
	KW_OR
	KW_ORELSE
	KW_PRAGMA
	KW_PRIVATE
	KW_PTR
	KW_RETURN
	KW_SCOPE
	KW_SHARED
	KW_SHL
	KW_SHORT
	KW_SHR
	KW_SINGLE
	KW_SIZEOF
	KW_STATIC
	KW_STDCALL
	KW_SUB
	KW_THEN
	KW_TO
	KW_TYPE
	KW_TYPEOF
	KW_UBYTE
	KW_UINTEGER
	KW_ULONG
	KW_ULONGINT
	KW_UNDEF
	KW_UNION
	KW_USHORT
	KW_WCHAR_T
	KW_WEND
	KW_WHILE
	KW_WSTR
	KW_WSTRING
	KW_XOR
	KW_ZSTRING

	TK__COUNT
end enum

#define TkTypeBits 7
#define PayloadBits 25

#assert (TkTypeBits + PayloadBits) = 32
#assert TK__COUNT <= (2 ^ TkTypeBits)
const MaxPayload = (2 ^ PayloadBits) - 1

type Token field = 1
	typ     : TkTypeBits  as ulong
	payload : PayloadBits as ulong
end type

#assert sizeof(Token) = 4

extern tokentext(0 to TK__COUNT-1) as zstring ptr

type TokenBuffer
	p as Token ptr
	count as integer
	strings as StrBuffer
private:
	room as integer
public:
	declare destructor()
	declare operator let(byref as const TokenBuffer) '' unimplemented
	declare function storePayload(byval payload as const zstring ptr) as ulong
	declare function storePayload(byval payload as const ubyte ptr, byval size as uinteger) as ulong
	declare sub add(byval tk as Token)
	declare sub add(byval typ as ulong, byval payload as const zstring ptr = NULL)
	declare sub add(byval typ as ulong, byval payload as const ubyte ptr, byval size as uinteger)
	declare sub removeLast()
	declare const function get(byval i as integer) as ulong
end type

type CodeGen
	tokens as TokenBuffer
	as integer indent, comment, commentspaces, singleline, singlelinebols
	have_bol as boolean
	declare operator let(byref as const CodeGen) '' unimplemented
	declare sub add(byval typ as ulong, byval payload as const zstring ptr = NULL)
	declare sub add(byval typ as ulong, byval payload as const ubyte ptr, byval size as uinteger)
	declare sub emitType(byval dtype as integer, byval subtype as AstNode ptr)
	declare sub emitType(byval n as AstNode ptr)
	declare sub emitAlias(byval n as AstNode ptr)
	declare sub emitIdAndArray(byval n as AstNode ptr, byval allow_alias as integer)
	declare sub emitSeparatedList(byval n as AstNode ptr, byval skip_head as integer)
	declare sub emitParamList(byval n as AstNode ptr, byval skip_head as integer)
	declare sub emitInitializer(byval n as AstNode ptr)
	declare sub emitProcHeader(byval n as AstNode ptr, byval is_expr as integer)
	declare sub emitTodoForQuirkKeywordType(byval id as zstring ptr)
	declare sub emitMacroHeader(byval n as AstNode ptr, byval macrokw as ulong)
	declare sub emitExpr(byval n as AstNode ptr, byval need_parens as integer = FALSE, byval need_macroparam_parens as integer = TRUE)
	declare sub bol()
	declare sub eol()
	declare sub eolSingleLineBegin()
	declare sub eolSingleLineEnd()
	declare sub emitLine(byval begin as const zstring ptr, byval p as const ubyte ptr)
	declare sub emitLines(byval lines as const zstring ptr)
	declare sub emitIndentedChildren(byval n as AstNode ptr, byval parentkind as integer = -1)
	declare sub emitVarDecl _
		( _
			byval kw1 as integer, _
			byval kw2 as integer, _
			byval spaces as integer, _
			byval n as AstNode ptr, _
			byval is_extern as integer _
		)
	declare sub emitSelfBop(byval n as AstNode ptr, byval op as ulong)
	declare sub emitCode(byval n as AstNode ptr, byval parentkind as integer = -1)
	declare sub emitHeader(byref header as HeaderInfo)
end type

type TokenRenderer extends object
	declare abstract sub emitLine(byref ln as const string)
	declare sub render(byref tokens as const TokenBuffer)
end type

type FileWriter extends TokenRenderer
private:
	fo as integer
public:
	declare constructor(byref filename as const string)
	declare destructor()
	declare sub emitLine(byref ln as const string) override
end type

type StdoutWriter extends TokenRenderer
	declare sub emitLine(byref ln as const string) override
end type

type StringWriter extends TokenRenderer
	s as string
	declare sub emitLine(byref ln as const string) override
end type

end namespace

declare function emitFbType(byval dtype as integer, byval subtype as AstNode ptr) as string
declare function emitFbExpr(byval n as AstNode ptr) as string
declare sub emitFbFile(byref filename as string, byval header as HeaderInfo ptr, byval ast as AstNode ptr)
declare sub emitFbStdout(byval ast as AstNode ptr, byval indent as integer)

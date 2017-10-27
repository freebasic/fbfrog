#include once "ast.bi"
#include once "tk.bi"

declare function hNumberLiteral _
	( _
		byref tk as TokenBuffer, _
		byval x as integer, _
		byval is_cpp as integer, _
		byref errmsg as string, _
		byval clong32 as integer _
	) as AstNode ptr
declare function hStringLiteral _
	( _
		byref tk as TokenBuffer, _
		byval x as integer, _
		byval eval_escapes as integer, _
		byref errmsg as string _
	) as AstNode ptr
declare function hDefineHead(byref tk as TokenBuffer, byref x as integer) as AstNode ptr

type COperatorInfo
	precedence as byte
	is_right_assoc as byte
end type

extern copinfo(ASTKIND_CLOGOR to ASTKIND_IIF) as COperatorInfo

#define cprecedence(op) copinfo(op).precedence
#define cOpIsLeftAssoc(op) (not copinfo(op).is_right_assoc)

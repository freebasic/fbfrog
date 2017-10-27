#include once "ast.bi"
#include once "fbfrog-apiinfo.bi"

type COperatorInfo
	precedence as byte
	is_right_assoc as byte
end type

extern copinfo(ASTCLASS_CLOGOR to ASTCLASS_IIF) as COperatorInfo

#define cprecedence(op) copinfo(op).precedence
#define cOpIsLeftAssoc(op) (not copinfo(op).is_right_assoc)

declare function hNumberLiteral _
	( _
		byval x as integer, _
		byval is_cpp as integer, _
		byref errmsg as string, _
		byval clong32 as integer _
	) as ASTNODE ptr
declare function hStringLiteral _
	( _
		byval x as integer, _
		byval eval_escapes as integer, _
		byref errmsg as string _
	) as ASTNODE ptr
declare function hDefineHead(byref x as integer) as ASTNODE ptr

declare sub cppInit(byref api as ApiInfo)
declare sub cppEnd()
declare sub cppAddPredefine(byval id as zstring ptr, byval body as zstring ptr)
declare sub cppAddTargetPredefines(byval target as TargetInfo)
declare sub cppAddIncDir(byval incdir as zstring ptr)
declare sub cppAppendIncludeDirective(byval filename as zstring ptr, byval tkflags as integer)
declare sub cppMain()
declare sub hMoveDirectivesOutOfConstructs()
declare sub hApplyReplacements()

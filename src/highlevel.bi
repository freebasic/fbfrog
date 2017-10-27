#include once "ast.bi"
#include once "fbfrog-apiinfo.bi"

type ApiSpecificBiOptions
	as ASTNODE ptr inclibs, undefs, addincludes
end type

declare sub hlAutoAddDividers(byval ast as ASTNODE ptr)
declare sub hlGlobal(byval ast as ASTNODE ptr, byref api as ApiInfo)
declare sub hlFile(byval ast as ASTNODE ptr, byref api as ApiInfo, byref bioptions as ApiSpecificBiOptions)
declare function hlCountDecls(byval ast as ASTNODE ptr) as integer
declare function hlCountTodos(byval n as ASTNODE ptr) as integer

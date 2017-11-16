#include once "ast.bi"
#include once "fbfrog-apiinfo.bi"

declare sub hlAutoAddDividers(byval ast as AstNode ptr)
declare sub hlGlobal(byval ast as AstNode ptr, byref api as ApiInfo)
declare sub hlFile(byval ast as AstNode ptr, byref api as ApiInfo)
declare function hlCountDecls(byval ast as AstNode ptr) as integer
declare function hlCountTodos(byval n as AstNode ptr) as integer

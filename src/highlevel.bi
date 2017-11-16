#include once "ast.bi"
#include once "options.bi"

declare sub hlAutoAddDividers(byval ast as AstNode ptr)
declare sub hlGlobal(byval ast as AstNode ptr, byref options as BindingOptions)
declare sub hlFile(byval ast as AstNode ptr, byref options as BindingOptions)
declare function hlCountDecls(byval ast as AstNode ptr) as integer
declare function hlCountTodos(byval n as AstNode ptr) as integer

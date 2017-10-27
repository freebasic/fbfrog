#include once "ast.bi"

declare function astDumpPrettyVersion(byval n as AstNode ptr) as string
declare function astNewVERAND(byval a as AstNode ptr = NULL, byval b as AstNode ptr = NULL) as AstNode ptr
declare function astNewVEROR(byval a as AstNode ptr = NULL, byval b as AstNode ptr = NULL) as AstNode ptr
declare function astNewVERNUMCHECK(byval vernum as integer) as AstNode ptr
declare function astMergeVerblocks _
	( _
		byval a as AstNode ptr, _
		byval b as AstNode ptr _
	) as AstNode ptr
declare sub astMergeNext(byval api as ApiBits, byref final as AstNode ptr, byref incoming as AstNode ptr)
declare sub astProcessVerblocks(byval code as AstNode ptr)

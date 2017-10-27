#include once "ast.bi"

declare function astDumpPrettyVersion(byval n as ASTNODE ptr) as string
declare function astNewVERAND(byval a as ASTNODE ptr = NULL, byval b as ASTNODE ptr = NULL) as ASTNODE ptr
declare function astNewVEROR(byval a as ASTNODE ptr = NULL, byval b as ASTNODE ptr = NULL) as ASTNODE ptr
declare function astNewVERNUMCHECK(byval vernum as integer) as ASTNODE ptr
declare function astMergeVerblocks _
	( _
		byval a as ASTNODE ptr, _
		byval b as ASTNODE ptr _
	) as ASTNODE ptr
declare sub astMergeNext(byval api as ApiBits, byref final as ASTNODE ptr, byref incoming as ASTNODE ptr)
declare sub astProcessVerblocks(byval code as ASTNODE ptr)

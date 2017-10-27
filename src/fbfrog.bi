#include once "api.bi"
#include once "ast.bi"
#include once "fbfrog-apiinfo.bi"
#include once "highlevel.bi"

type BIFILE
	filename	as zstring ptr
	header as HeaderInfo

	'' command line options specific to this .bi (e.g. -inclib) from the current API
	options as ApiSpecificBiOptions

	'' Used to hold the .bi file-specific tree for the current API, after
	'' that API was parsed and its big AST was split up. Reset to NULL after
	'' being merged into "final".
	incoming	as ASTNODE ptr

	'' Holds/accumulates the merged AST for this .bi file, containing all
	'' APIs. The "incoming" trees are merged into this one after another
	'' until all APIs were processed.
	final		as ASTNODE ptr
end type

namespace frog
	extern as integer verbose
	extern as integer enabledoscount
	extern as ASTNODE ptr script
	extern as ASTNODE ptr completeverors, fullveror
	extern as ApiInfo ptr apis
	extern as integer apicount
	extern as ApiBits fullapis
	extern vernums(any) as string
	extern versiondefine as string
	extern as BIFILE ptr bis
	extern as integer bicount
end namespace

declare function frogLookupBiFromBi(byref filename as string) as integer
declare function frogLookupBiFromH(byval hfile as zstring ptr) as integer

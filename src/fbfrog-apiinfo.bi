#include once "ast-match.bi"
#include once "tk.bi"
#include once "util-hash.bi"
#include once "util-str.bi"

type CodeReplacement
	as zstring ptr fromcode, tocode
	tofb as integer
	patternlen as integer '' used temporarily by hApplyReplacements()
end type

type ApiInfo
	verand as AstNode ptr
	script as AstNode ptr

	target as TargetInfo
	as integer windowsms, clong32, fixunsizedarrays
	as integer nofunctionbodies, dropmacrobodyscopes, removeEmptyReservedDefines

	have_renames as integer
	renameopt(tktokens.OPT_RENAMETYPEDEF to tktokens.OPT_RENAME) as THash = any
	idopt(tktokens.OPT_RENAME_ to tktokens.OPT_EXPAND) as StringMatcher
	patterns(tktokens.OPT_NOSTRING to tktokens.OPT_STRING) as DeclPatterns
	removeinclude as THash = THash(3, FALSE)
	setarraysizeoptions as THash = THash(3, FALSE)
	moveaboveoptions as AstNode ptr

	replacements as CodeReplacement ptr
	replacementcount as integer

	log as AstNode ptr

	declare constructor()
	declare destructor()
	declare sub addReplacement(byval fromcode as zstring ptr, byval tocode as zstring ptr, byval tofb as integer)
	declare sub loadOption(byval opt as integer, byval param1 as zstring ptr, byval param2 as zstring ptr)
	declare sub loadOptions()
	declare sub print(byref ln as string)
	declare function prettyId() as string
	declare operator let(byref as const ApiInfo) '' unimplemented
end type

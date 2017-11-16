#include once "ast.bi"
#include once "options.bi"

type DefineInfo
	xbody	as integer  '' 1st token of the #define body
	xeol	as integer  '' eol/eof token behind the #define body

	'' PPDEFINE node with information about the macro's parameters etc.
	macro	as AstNode ptr

	declare destructor()
	declare function clone() as DefineInfo ptr
	declare sub copyBody(byref tk as TokenBuffer, byval x as integer)
	declare function equals(byref tk as TokenBuffer, byval b as DefineInfo ptr) as integer
end type

type CppContext
	sourcectx as SourceContext ptr
	tk as TokenBuffer ptr
	options as BindingOptions ptr
	as integer x  '' Current token index

	'' Lookup table of macros known to be either defined or undefined.
	''   defined <=> data = a DEFINEINFO object
	'' undefined <=> data = NULL
	'' Even though unregistered symbols are implicitly undefined,
	'' registering them is useful to show the "assuming undefined" warning
	'' (and to only show it once).
	macros		as THash = THash(4, TRUE)

	declare constructor(byref sourcectx as SourceContext, byref tk as TokenBuffer, byref options as BindingOptions)
	declare destructor()
	declare function lookupMacro(byval id as zstring ptr) as DefineInfo ptr
	declare sub addMacro(byval id as zstring ptr, byval definfo as DefineInfo ptr)
	declare sub addKnownUndefined(byval id as zstring ptr)
	declare function checkForMacroCall(byval y as integer) as DefineInfo ptr
	declare function expandInTkBeginEnd(byval y as integer, byval inside_ifexpr as integer) as integer
	declare function expandInRange _
		( _
			byval first as integer, _
			byval last as integer, _
			byval inside_ifexpr as integer _
		) as integer
	declare function insertMacroExpansion _
		( _
			byval callbehindspace as integer, _
			byval expansionbegin as integer, _
			byref definfo as DefineInfo, _
			byval argbegin as integer ptr, _
			byval argend as integer ptr, _
			byval argcount as integer, _
			byval inside_ifexpr as integer _
		) as integer
	declare sub expandMacro _
		( _
			byref definfo as DefineInfo, _
			byval callbegin as integer, _
			byval callend as integer, _
			byval argbegin as integer ptr, _
			byval argend as integer ptr, _
			byval argcount as integer, _
			byval inside_ifexpr as integer, _
			byval expand_recursively as integer _
		)
	declare function maybeExpandMacro(byval y as integer, byval inside_ifexpr as integer, byval expand_recursively as integer) as integer
	declare sub maybeExpandMacroInDefineBody(byval parentdefine as AstNode ptr)
	declare function shouldRemoveDefine(byval id as zstring ptr) as integer
	declare sub parseDefine(byref flags as integer)

	declare operator let(byref as const CppContext) '' unimplemented
end type

declare sub hMoveDirectivesOutOfConstructs(byref tk as TokenBuffer)
declare sub hApplyReplacements(byref sourcectx as SourceContext, byref tk as TokenBuffer, byref options as BindingOptions)

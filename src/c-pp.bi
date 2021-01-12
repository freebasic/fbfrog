#include once "ast.bi"
#include once "fbfrog-apiinfo.bi"

type CppStackNode
	state		as integer  '' STATE_*
	knownfile	as integer  '' Index into cpp.files array, if it's an #include context, or -1
	'' file contexts:
	'' A node from the incdirs list, representing the incdir where
	'' this file was found (for #include_next support)
	incdir		as AstNode ptr
end type

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

type SavedMacro
	id		as zstring ptr		'' Macro's name

	'' A DefineInfo object if the macro was defined when being saved,
	'' or NULL if it was undefined.
	definfo		as DefineInfo ptr

	declare destructor()
end type

type KNOWNFILE
	incfile as zstring ptr  '' Normalized file name
	guardstate	as integer
	guard		as zstring ptr  '' #include guard symbol, if any
	pragmaonce as integer  '' Whether #pragma once was found in this file
end type

type CPPVALUE
	vali as longint
	dtype as integer
end type

type CppContext
	sourcectx as SourceContext ptr
	tk as TokenBuffer ptr
	api as ApiInfo ptr
	as integer x  '' Current token index

	'' #if/file context stack
	'' * starts out with only the toplevel file context
	'' * #if blocks and #include contexts are put on this stack
	'' * an #endif found in an #include won't be able to close an #if from
	''   the parent file, since the #include stack node is in the way, and
	''   must be popped first.
	const MAXSTACK = 128
	stack(0 to MAXSTACK-1) as CppStackNode
	as integer level  '' Current top of stack

	'' Stack level which caused #if 0 skipping
	'' * Allows us to continue parsing #ifs/#endifs even while skipping an
	''   #if 0 block, and this way, to determine which #endif ends skipping
	as integer skiplevel

	as integer filelevel

	'' Lookup table of macros known to be either defined or undefined.
	''   defined <=> data = a DEFINEINFO object
	'' undefined <=> data = NULL
	'' Even though unregistered symbols are implicitly undefined,
	'' registering them is useful to show the "assuming undefined" warning
	'' (and to only show it once).
	macros		as THash = THash(4, TRUE)

	'' Array of macros saved by #pragma push_macro, for use by #pragma
	'' pop_macro later.
	''  * push_macro saves the macro's current state - whether it's defined
	''    or not, and if it's defined, it's body/value.
	''  * push_macro even saves duplicate copies of the macro state: two
	''    equal push_macro's directly following each other create two stack
	''    entries, not one.
	''  * pop_macro restores the macro to the last saved state, overwriting
	''    the current value, #defining the macro again if it was #undef'ed
	''    in the meantime, or even #undef'ing it.
	''  * pop_macro does nothing if the stack for that macro is empty.
	'' Macros are appended to the array in the order they are saved. Popping
	'' a macro requires searching the array backwards for an entry for the
	'' given macro name. A hash table could be used instead, but that does
	'' not seem to be worth it.
	savedmacros		as SavedMacro ptr
	savedmacrocount	as integer

	incdirs		as AstNode ptr

	'' Information about known files
	files as KNOWNFILE ptr
	filecount as integer
	filetb as THash = THash(4, FALSE)  '' data = index into files array

	declare constructor(byval sourcectx as SourceContext ptr, byval tk as TokenBuffer ptr, byref api as ApiInfo)
	declare destructor()
	declare function isSkipping() as integer
	declare sub addPredefine(byval id as zstring ptr, byval body as zstring ptr)
	declare sub addTargetPredefines(byval target as TargetInfo)
	declare sub addIncDir(byval incdir as zstring ptr)
	declare sub appendIncludeDirective(byval filename as zstring ptr, byval tkflags as integer)
	declare function lookupMacro(byval id as zstring ptr) as DefineInfo ptr
	declare function isKnownSymbol(byval id as zstring ptr) as integer
	declare function isMacroCurrentlyDefined(byval id as zstring ptr) as integer
	declare sub addMacro(byval id as zstring ptr, byval definfo as DefineInfo ptr)
	declare sub addKnownUndefined(byval id as zstring ptr)
	declare sub appendSavedMacro(byval id as zstring ptr, byval definfo as DefineInfo ptr)
	declare sub removeSavedMacro(byval i as integer)
	declare function lookupSavedMacro(byval id as zstring ptr) as integer
	declare sub saveMacro(byval id as zstring ptr)
	declare sub restoreMacro(byval id as zstring ptr)
	declare function lookupOrAppendKnownFile(byval incfile as zstring ptr, byref prettyfile as string) as integer
	declare sub parseEol()
	declare function parseStringLiteral(byval eval_escapes as integer) as string
	declare sub checkForUnknownSymbol(byval id as zstring ptr)
	declare sub parseExpr(byref a as CPPVALUE, byval dtype_only as integer, byval level as integer = 0)
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
	declare function getFileContext() as CppStackNode ptr
	declare sub push(byval state as integer, byval knownfile as integer = -1)
	declare sub pop()
	declare sub applyIf(byval condition as integer)
	declare function parseIfExpr() as integer
	declare sub parseIf()
	declare sub parseIfdef(byval directivekw as integer)
	declare sub disableIncludeGuardOptimization()
	declare function isInsideFileLevelBlock() as integer
	declare sub parseElseIf()
	declare sub parseElse()
	declare sub parseEndIf()
	declare sub maybePrintIncludeTree(byref inctext as string, byref prettyfile as string, byval include_skipped as integer)
	declare function searchHeaderFile _
		( _
			byref contextfile as string, _
			byval contextincdir as AstNode ptr, _
			byref inctext as string, _
			byval is_system_include as integer, _
			byref incdir as AstNode ptr _
		) as string
	declare function parseIncludeFilename(byref is_system_include as integer) as string
	declare sub parseInclude(byval begin as integer, byref flags as integer, byval is_include_next as integer)
	declare sub parseEndInclude()
	declare sub maybeExpandMacroInDefineBody(byval parentdefine as AstNode ptr)
	declare function shouldRemoveDefine(byval id as zstring ptr) as integer
	declare sub parseDefine(byref flags as integer)
	declare sub parseUndef(byref flags as integer)
	declare sub parsePragmaPushPopMacro(byval is_push as integer)
	declare function parsePragma(byref flags as integer) as integer
	declare sub parseDirective()
	declare sub parseNext()
	declare sub parseToplevel()

	declare operator let(byref as const CppContext) '' unimplemented
end type

declare sub hMoveDirectivesOutOfConstructs(byref tk as TokenBuffer)
declare sub hApplyReplacements(byref sourcectx as SourceContext, byref tk as TokenBuffer, byref api as ApiInfo)

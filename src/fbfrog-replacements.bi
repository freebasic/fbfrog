#include once "fbfrog-apiinfo.bi"

''
'' Read "replacement" files
''
'' Format for one replacement entry:
''    convert c: <C code>
''    to c: <C code>
'' or:
''    convert c: <C code>
''    to fb: <FB code>
''
'' Code can be given either in a single line behind the keywords, or in a
'' multi-line block below the keyword:
''    <keyword>:
''        <multi-line code>
''
'' # for comments
''
'' Smart indentation handling for multi-line code snippets: The indentation in
'' the first line specifies the indentation in the replacements file. Any
'' additional whitespace is preserved as-is as part of the code snippets.
'' (doesn't matter for the C code though because they'll be lexed, which drops
'' whitespace anyways)
''
'' TODO: auto-generate empty replacement files: add a "convert:" line for all
'' TODOs, leave the "to:" empty
''
type ReplacementsParser
	as integer f, linenum, reachedeof
	as string filename, ln

	const ConvertKeyword = "convert c:"
	const ToCKeyword = "to c:"
	const ToFbKeyword = "to fb:"

	declare constructor(byref filename as string)
	declare destructor()
	declare sub nextLine()
	declare sub parseOops(byref message as string)
	declare function parseCode(byref keyword as string) as string
	declare sub parse(byref api as ApiInfo)
end type

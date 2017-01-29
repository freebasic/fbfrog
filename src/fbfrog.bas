''
'' Main module, command line interface
''
'' This is how fbfrog works:
''
'' 1. Read command line. Evaluate -declare*, -if, -select options. Determine the
''    list of APIs to parse. Collect input files and options for each API.
''
''    Often APIs just means compilation targets:
''        dos, linux-x86, linux-x86_64, win32, win64
''    but it can be more than that. For example, fbfrog can be told about
''    conditional #defines used in the input header, to make them available in
''    the .bi output (e.g. Allegro's ALLEGRO_STATICLINK), and fbfrog can even be
''    given separate input headers for each API. That's useful to create a
''    multi-version .bi binding, where you can specify a #define to select the
''    desired API version (e.g. for GTK2 or GTK3).
''
'' 2. For each API, parse the .h input files (C lexer, C preprocessor, C parser)
''    into an AST. Do highlevel transformations on that AST to adjust it to FB,
''    e.g. fix up array parameters, or solve out redundant typedefs.
''
''    The CPP expands as many #includes and macros as it can, so that fbfrog
''    gets to see as much information about the API as possible. The CPP uses
''    API-specific pre-#defines: each API has a certain compilation target.
''
'' 3. Split up the API-specific ASTs into .bi files according to -emit options.
''    Each .bi file ends up with its parts of all the APIs. Do .bi-file-specific
''    highlevel transformations on the individual parts (e.g. add Extern blocks).
''
'' 4. For each .bi file, merge the API-specific trees into one, adding #if
''    checks where needed (e.g. to enclose target-specific code). Then emit the
''    .bi file.
''

#include once "fbfrog.bi"
#include once "emit.bi"
#include once "file.bi"

using tktokens

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
	dim shared as integer verbose
	dim shared as string outname, defaultoutname
	dim shared header as HeaderInfo  '' global titles etc. - will be added to all generated .bi files

	dim shared as integer have_declareversions

	dim shared os(0 to OS__COUNT-1) as byte
	dim shared arch(0 to ARCH__COUNT-1) as byte
	dim shared as integer enabledoscount

	dim shared as ASTNODE ptr script
	dim shared as ASTNODE ptr completeverors, fullveror
	dim shared as ApiInfo ptr apis
	dim shared as integer apicount
	dim shared as ApiBits fullapis

	dim shared as ASTNODE ptr mergedlog

	'' *.bi output file names from the -emit options
	dim shared as BIFILE ptr bis
	dim shared as integer bicount
	dim shared as THash ucasebihash = THash(6, TRUE)
	dim shared as THash bilookupcache = THash(6, TRUE)

	'' *.h file name patterns from the -emit options, associated to the
	'' corresponding bis array index
	type HPATTERN
		pattern as string
		bi as integer
	end type
	dim shared as HPATTERN ptr patterns
	dim shared as integer patterncount

	dim shared vernums(any) as string
	dim shared versiondefine as string
end namespace

private sub frogSetArchs(byval enabled as integer)
	for i as integer = 0 to ARCH__COUNT - 1
		frog.arch(i) = enabled
	next
end sub

private sub frogSetOSes(byval enabled as integer)
	for i as integer = 0 to OS__COUNT - 1
		frog.os(i) = enabled
	next
end sub

private sub frogSetTargets(byval enabled as integer)
	frogSetOSes(enabled)
	frogSetArchs(enabled)
end sub

private function frogAddVernum(byref vernum as string) as integer
	var i = ubound(frog.vernums) + 1
	redim preserve frog.vernums(0 to i)
	frog.vernums(i) = vernum
	return i
end function

private function frogLookupVernum(byref vernum as string) as integer
	for i as integer = 0 to ubound(frog.vernums)
		if frog.vernums(i) = vernum then return i
	next
	function = -1
end function

private sub frogAddPattern(byref pattern as string, byval bi as integer)
	var i = frog.patterncount
	frog.patterncount += 1
	frog.patterns = reallocate(frog.patterns, frog.patterncount * sizeof(*frog.patterns))
	clear(frog.patterns[i], 0, sizeof(*frog.patterns))
	with frog.patterns[i]
		.pattern = pattern
		.bi = bi
	end with
end sub

private function frogLookupBiFromBi(byref filename as string) as integer
	var ucasefilename = ucase(filename, 1)
	var ucasehash = hashHash(ucasefilename)
	var item = frog.ucasebihash.lookup(ucasefilename, ucasehash)
	if item->s then
		function = cint(item->data)
	else
		function = -1
	end if
end function

private sub frogAddBi(byref filename as string, byref pattern as string)
	dim bi as integer

	'' Put bi files into a hashtb, so that multiple -emit options with the
	'' same filename will be redirected to the same bi file. This check
	'' should be case-insensitive, because of Windows' file system...
	var ucasefilename = ucase(filename, 1)
	var ucasehash = hashHash(ucasefilename)
	var item = frog.ucasebihash.lookup(ucasefilename, ucasehash)
	if item->s then
		'' Already exists
		bi = cint(item->data)
	else
		'' Add new bi file
		bi = frog.bicount
		frog.bicount += 1
		frog.bis = reallocate(frog.bis, frog.bicount * sizeof(*frog.bis))
		clear(frog.bis[bi], 0, sizeof(*frog.bis))
		with frog.bis[bi]
			.filename = strDuplicate(filename)
		end with
		frog.ucasebihash.add(item, ucasehash, ucasefilename, cptr(any ptr, bi))
	end if

	frogAddPattern(pattern, bi)
end sub

function frogLookupBiFromH(byval hfile as zstring ptr) as integer
	'' Check whether we've already cached the .bi for this .h file
	var hfilehash = hashHash(hfile)
	var item = frog.bilookupcache.lookup(hfile, hfilehash)
	if item->s then
		return cint(item->data)
	end if

	'' Slow lookup
	var bi = -1
	var hfilestr = *hfile
	for pattern as integer = 0 to frog.patterncount - 1
		if strMatch(hfilestr, frog.patterns[pattern].pattern) then
			bi = frog.patterns[pattern].bi
			exit for
		end if
	next

	'' Cache the lookup results to improve performance
	'' (the CPP does repeated lookups on each #include, which can add up on certain
	'' input headers, such as the Windows API headers)
	frog.bilookupcache.add(item, hfilehash, hfile, cptr(any ptr, bi))
	function = bi
end function

'' Find a *.fbfrog or *.h file in fbfrog's include/ dir, its "library" of
'' premade collections of pre-#defines etc. useful when creating bindings.
private function hFindResource(byref filename as string) as string
	'' 1. Absolute path? Use as-is
	if pathIsAbsolute(filename) then return filename

	'' 2. Current directory?
	if fileexists(filename) then return filename

	'' 3. <exepath>/include/fbfrog/?
	const INCLUDEDIR = "include" + PATHDIV + "fbfrog" + PATHDIV
	var dir1 = hExepath() + INCLUDEDIR
	var found = dir1 + filename
	if fileexists(found) then return found

	'' 4. <exepath>/../include/fbfrog/?
	var dir2 = hExepath() + ".." + PATHDIV + INCLUDEDIR
	found = dir2 + filename
	if fileexists(found) then return found

	print "error: could not find '" + filename + "'"
	print "search dirs:"
	print "  <curdir> (" + curdir() + ")"
	print "  <exepath>/include/fbfrog (" + dir1 + ")"
	print "  <exepath>/../include/fbfrog (" + pathNormalize(dir2) + ")"
	end 1
end function

private sub hPrintHelpAndExit()
	print "fbfrog 1.12 (" + __DATE_ISO__ + "), FreeBASIC *.bi binding generator"
	print "usage: fbfrog foo.h [options]"
	print "global options:"
	print "  @<file> | <*.fbfrog>  Read more command line arguments from a file"
	print "  -o <path/file>        Set output .bi file name, or just the output directory"
	print "  -emit '*.h' foo.bi    Emit code from matching .h into specified .bi"
	print "  -dontemit '*.h'       Drop code from matching .h files"
	print "  -title <package + version> original-license.txt translators.txt [<destination .bi file>]"
	print "     Add text at the top of .bi file(s): package name + version, copyright, license"
	print "  -v                    Show verbose/debugging info. Pass -v -v for more debug info"
	print "  -target nodos|noarm|<os>|<arch>|<os>-<arch>  Specify OS/arch to translate for, instead of all"
	print "API script logic:"
	print "  -declareversions <symbol> (<number>)+     Version numbers"
	print "  -declarebool <symbol>                     Single on/off #define"
	print "  -selecttarget  (-case <target> ...)+ [-caseelse ...] -endselect"
	print "  -selectversion (-case <number> ...)+ [-caseelse ...] -endselect"
	print "  -selectdefine  (-case <symbol> ...)+ [-caseelse ...] -endselect"
	print "  -iftarget <target> ... [-else ...] -endif"
	print "  -ifdef <symbol> ... [-else ...] -endif"
	print "CPP (options are API-specific):"
	print "  -define <id> [<body>]    Add pre-#define"
	print "  -include <file>          Add pre-#include"
	print "  -fbfroginclude <file>    Add pre-#include from include/fbfrog/"
	print "  -incdir <path>           Add search directory for .h #includes"
	print "binding generation (options are API-specific):"
	print "  -windowsms       Use Extern ""Windows-MS"" instead of Extern ""Windows"""
	print "  -clong32         Translate C long as 32bit LONG, instead of CLONG"
	print "  -fixunsizedarrays  Wrap [] arrays with a #define"
	print "  -nofunctionbodies  Don't preserve function bodies"
	print "  -dropmacrobodyscopes  Drop scope blocks with only 1 statement in macro bodies"
	print "  -replacements <file>  Load patterns for search/replace"
	print "  options for renaming symbols (-rename* <oldid> <newid>):"
	print "    -renametypedef, -renametag (struct/union/enum),"
	print "    -renameproc (procedures)"
	print "    -renamedefine, -renamemacroparam"
	print "    -rename (any matching symbol)"
	print "  -removeEmptyReservedDefines Remove empty (and parameter-less) #defines with __* or _U* names"
	print "  -rename_ <id> Rename symbol by appending an _ underscore"
	print "  options for removing declarations (-remove* <id>, where <id> is an id or a pattern):"
	print "    -removedefine, -removeproc, -removevar, -remove1st, -remove2nd"
	print "    -remove (any matching symbol)"
	print "  -dropprocbody <id>  Don't preserve a certain procedure's body"
	print "  -typedefhint <id>   Mark <id> as typedef, to help parsing of type casts"
	print "  -addforwarddecl <id>  Force a forward declaration to be added for the given type"
	print "  -undefbeforedecl <id>  Insert an #undef above a declaration"
	print "  -ifndefdecl <id>    Wrap declaration of symbol named <id> in an #ifndef block"
	print "  -convbodytokens <id>  Translate a #define's body only by converting the tokens, no parsing"
	print "  -forcefunction2macro <id>  Force an inline function to be converted to a macro,"
	print "                             even if parameters are used multiple times"
	print "  -expandindefine <id>  Expand macro in #define body"
	print "  -noexpand <id>      Disable expansion of certain #define"
	print "  -expand <id>        Expand and remove matching typedefs"
	print "  -nostring <decl-pattern> Prevent a symbol from being turned into a zstring"
	print "  -string <decl-pattern>   Force a [U]Byte [Ptr] symbol to be turned into a ZString [Ptr]"
	print "  -removeinclude <filename>  Remove matching #include directives"
	print "  -setarraysize <id> <size>  Set size of an [] array"
	print "  -moveabove <id> <ref>  Move declaration of <id> above declaration of <ref>"
	print "  -inclib <name> [<destination .bi file>]  Add #inclib ""<name>"""
	print "  -undef  <id>   [<destination .bi file>]  Add #undef <id>"
	print "  -addinclude <.bi file> [<destination .bi file>]  Add #include <.bi file>"
	end 1
end sub

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

constructor ReplacementsParser(byref filename as string)
	this.filename = filename
	f = freefile()
	if open(filename, for input, as #f) <> 0 then
		oops("couldn't open file '" + filename + "'")
	end if
end constructor

destructor ReplacementsParser()
	close #f
end destructor

sub ReplacementsParser.nextLine()
	do
		if reachedeof then
			ln = "<EOF>"
			exit do
		end if

		linenum += 1
		line input #f, ln

		'' Neither empty line, nor a comment?
		if len(ln) > 0 then
			if ln[0] <> CH_HASH then
				exit do
			end if
		end if

		reachedeof = eof(f)
	loop
end sub

sub ReplacementsParser.parseOops(byref message as string)
	print filename + "(" & linenum & "): error: " + message + ":"
	print "    " + ln
	end 1
end sub

function ReplacementsParser.parseCode(byref keyword as string) as string
	'' Any code behind the keyword?
	var code = hTrim(right(ln, len(ln) - len(keyword)))
	nextLine()
	if len(code) > 0 then
		return code
	end if

	'' Any indentation in the first line of the FB code block is treated as part
	'' of the replacements file, not the FB code block.
	''  * the first line must have some indentation
	''  * all lines of an FB code block must have at least the same indentation
	''    as the first line
	var trimmedln = hLTrim(ln)
	code += trimmedln
	var indentation = left(ln, len(ln) - len(trimmedln))
	assert(indentation + trimmedln = ln)
	if len(indentation) = 0 then
		parseOops("missing indentation in code block")
	end if
	nextLine()

	do
		'' Treat all following indented lines as part of the code block
		if (len(ln) = 0) orelse ((ln[0] <> CH_SPACE) and (ln[0] <> CH_TAB)) then
			exit do
		end if

		if left(ln, len(indentation)) <> indentation then
			parseOops("indentation here doesn't match the first line of this code block")
		end if
		code += !"\n" + right(ln, len(ln) - len(indentation))
		nextLine()
	loop until reachedeof

	function = code
end function

sub ReplacementsParser.parse(byref api as ApiInfo)
	nextLine()

	while reachedeof = FALSE
		'' Read convert/to line pair

		'' "convert" line
		if strStartsWith(ln, ConvertKeyword) = FALSE then
			parseOops("expected '" + ConvertKeyword + "' line, but found something else")
		end if
		var fromcode = parseCode(ConvertKeyword)

		'' "to" line
		dim tocode as string
		dim tofb as integer
		if strStartsWith(ln, ToCKeyword) then
			tocode = parseCode(ToCKeyword)
		elseif strStartsWith(ln, ToFbKeyword) then
			tocode = parseCode(ToFbKeyword)
			tofb = TRUE
		else
			parseOops("expected line to start with '" + ToCKeyword + "' or '" + ToFbKeyword + "', but found something else")
		end if

		api.addReplacement(fromcode, tocode, tofb)
	wend
end sub

constructor ApiInfo()
	for i as integer = lbound(renameopt) to ubound(renameopt)
		renameopt(i).constructor(3, FALSE)
	next
	log = astNewGROUP()
end constructor

destructor ApiInfo()
	for i as integer = 0 to replacementcount - 1
		deallocate(replacements[i].fromcode)
		deallocate(replacements[i].tocode)
	next
	deallocate(replacements)
	astDelete(log)
end destructor

sub ApiInfo.addReplacement(byval fromcode as zstring ptr, byval tocode as zstring ptr, byval tofb as integer)
	var i = replacementcount
	replacementcount += 1
	replacements = reallocate(replacements, replacementcount * sizeof(*replacements))
	clear(replacements[i], 0, sizeof(*replacements))
	with replacements[i]
		.fromcode = strDuplicate(fromcode)
		.tocode = strDuplicate(tocode)
		.tofb = tofb
	end with
end sub

sub ApiInfo.loadOption(byval opt as integer, byval param1 as zstring ptr, byval param2 as zstring ptr)
	select case as const opt
	case OPT_WINDOWSMS        : windowsms        = TRUE
	case OPT_CLONG32          : clong32          = TRUE
	case OPT_FIXUNSIZEDARRAYS : fixunsizedarrays = TRUE
	case OPT_NOFUNCTIONBODIES : nofunctionbodies = TRUE
	case OPT_DROPMACROBODYSCOPES : dropmacrobodyscopes = TRUE
	case OPT_REMOVEEMPTYRESERVEDDEFINES : removeEmptyReservedDefines = TRUE

	case OPT_RENAMETYPEDEF, OPT_RENAMETAG, OPT_RENAMEPROC, _
	     OPT_RENAMEDEFINE, OPT_RENAMEMACROPARAM, OPT_RENAME
		renameopt(opt).addOverwrite(param1, param2)
		have_renames = TRUE

	case OPT_RENAME_, OPT_REMOVE, OPT_REMOVEDEFINE, OPT_REMOVEPROC, OPT_REMOVEVAR, OPT_REMOVE1ST, OPT_REMOVE2ND, _
	     OPT_DROPPROCBODY, OPT_TYPEDEFHINT, OPT_ADDFORWARDDECL, OPT_UNDEFBEFOREDECL, OPT_IFNDEFDECL, _
	     OPT_CONVBODYTOKENS, OPT_FORCEFUNCTION2MACRO, OPT_EXPANDINDEFINE, OPT_NOEXPAND, OPT_EXPAND
		if opt = OPT_RENAME_ then
			have_renames = TRUE
		end if
		idopt(opt).addPattern(param1)

	case OPT_NOSTRING, OPT_STRING
		patterns(opt).parseAndAdd(*param1)

	case OPT_REMOVEINCLUDE
		removeinclude.addOverwrite(param1, NULL)

	case OPT_SETARRAYSIZE
		setarraysizeoptions.addOverwrite(param1, param2)

	case OPT_MOVEABOVE
		var n = astNewTEXT(param1)
		astSetAlias(n, param2)
		astBuildGroupAndAppend(moveaboveoptions, n)

	case OPT_REPLACEMENTS
		dim parser as ReplacementsParser = ReplacementsParser(*param1)
		parser.parse(this)

	'' Distribute .bi-file-specific options for this API to invidiual .bi files
	case OPT_INCLIB, OPT_UNDEF, OPT_ADDINCLUDE
		dim bi as integer
		if param2 then
			bi = frogLookupBiFromBi(*param2)
			if bi < 0 then
				oops("couldn't find destination .bi '" + *param2 + "', for '" + *tkInfoText(opt) + " " + *param1 + "'")
			end if
		else
			'' No destination .bi file given for this -inclib/-undef option;
			'' add it to the first .bi.
			'' This allows the simple use case where there's just one output .bi,
			'' and it's not necessary to specify a destination .bi for each option.
			bi = 0
		end if

		assert((bi >= 0) and (bi < frog.bicount))
		with frog.bis[bi]
			select case opt
			case OPT_INCLIB
				astBuildGroupAndAppend(.options.inclibs, astNew(ASTCLASS_INCLIB, param1))
			case OPT_UNDEF
				astBuildGroupAndAppend(.options.undefs, astNew(ASTCLASS_UNDEF, param1))
			case OPT_ADDINCLUDE
				astBuildGroupAndAppend(.options.addincludes, astNew(ASTCLASS_PPINCLUDE, param1))
			end select
		end with
	end select
end sub

sub ApiInfo.loadOptions()
	var i = script->head
	while i
		assert(i->class = ASTCLASS_OPTION)
		loadOption(i->opt, i->text, i->alias)
		i = i->next
	wend
end sub

sub ApiInfo.print(byref ln as string)
	astAppend(log, astNewTEXT(ln))
end sub

function ApiInfo.prettyId() as string
	var s = target.id()
	var extras = astDumpPrettyVersion(verand)
	if len(extras) > 0 then
		s += " " + extras
	end if
	function = s
end function

private function hTurnArgsIntoString(byval argc as integer, byval argv as zstring ptr ptr) as string
	dim s as string

	'' Even including argv[0] so it's visible in error messages
	'' (specially parsed in hParseArgs())
	for i as integer = 0 to argc-1
		var arg = *argv[i]

		'' If the argument contains special chars (white-space, ", '),
		'' enclose it in quotes as needed for lexLoadArgs().

		'' Contains '?
		if instr(arg, "'") > 0 then
			'' Must enclose in "..." and escape included " or \ chars properly.
			'' This also works if " or whitespace are included too.

			'' Insert \\ for \ before inserting \" for ", so \" won't accidentally
			'' be turned into \\".
			arg = strReplace(arg, $"\", $"\\")
			arg = strReplace(arg, """", $"\""")
			arg = """" + arg + """"
		'' Contains no ', but " or white-space?
		elseif instr(arg, any !""" \t\f\r\n\v") > 0 then
			'' Enclose in '...', so no escaping is needed.
			arg = "'" + arg + "'"
		'' Empty? Represent it as ""
		elseif len(arg) = 0 then
			arg = """"""
		end if

		if len(s) > 0 then
			s += " "
		end if
		s += arg
	next

	function = s
end function

private sub hLoadArgsFile _
	( _
		byval x as integer, _
		byref filename as string, _
		byval location as TkLocation _
	)

	const MAX_FILES = 1024  '' Arbitrary limit to detect recursion
	static filecount as integer

	if filecount > MAX_FILES then
		tkOops(x, "suspiciously many @file expansions, recursion? (limit=" & MAX_FILES & ")")
	end if

	filename = hFindResource(filename)

	'' Load the file content at the specified position
	var file = filebuffersAdd(filename, location)
	lexLoadArgs(x, file->buffer, file->source)
	filecount += 1

end sub

'' Expand @file arguments in the tk buffer
private sub hExpandArgsFiles()
	var x = 0
	do
		select case tkGet(x)
		case TK_EOF
			exit do

		case TK_ARGSFILE
			var filename = *tkGetText(x)

			'' Complain if argument was only '@'
			if len(filename) = 0 then
				tkOopsExpected(x, "file name directly behind @ (no spaces in between)")
			end if

			'' If the @file argument comes from an @file,
			'' open it relative to the parent @file's dir.
			var location = tkGetLocation(x)
			if location.source->is_file then
				filename = pathAddDiv(pathOnly(*location.source->name)) + filename
			end if

			'' Load the file content behind the @file token
			hLoadArgsFile(x + 1, filename, location)

			'' Remove the @file token (now that its location is no
			'' longer referenced), so it doesn't get in the way of
			'' hParseArgs().
			tkRemove(x, x)

			'' Re-check this position in case a new @file token was inserted right here
			x -= 1
		end select

		x += 1
	loop
end sub

private sub hExpectId(byval x as integer)
	tkExpect(x, TK_ID, "(valid symbol name)")
end sub

private function hIsStringOrId(byval x as integer) as integer
	function = (tkGet(x) = TK_STRING) or (tkGet(x) = TK_ID)
end function

private sub hExpectStringOrId(byval x as integer, byval paramdescription as zstring ptr)
	if hIsStringOrId(x) = FALSE then
		tkOopsExpected(x, paramdescription)
	end if
end sub

private function hPathRelativeToArgsFile(byval x as integer) as string
	var path = *tkGetText(x)

	'' If the file/dir argument isn't an absolute path, and it came from an
	'' @file, open it relative to the @file's dir.
	if pathIsAbsolute(path) = FALSE then
		var location = tkGetLocation(x)
		if location.source->is_file then
			path = pathAddDiv(pathOnly(*location.source->name)) + path
		end if
	end if

	function = path
end function

declare sub hParseArgs(byref x as integer)

private sub hParseSelectCompound(byref x as integer, byval selectclass as integer)
	'' -selecttarget|-selectversion|-selectdefine
	var xblockbegin = x
	x += 1

	astAppend(frog.script, astNew(selectclass))

	'' -case
	if tkGet(x) <> OPT_CASE then
		tkOopsExpected(x, "-case after the -select")
	end if

	do
		hParseArgs(x)

		select case tkGet(x)
		case TK_EOF
			tkOops(xblockbegin, "missing -endselect for this")

		case OPT_CASE
			if frog.script->tail->class = ASTCLASS_CASEELSE then
				tkOops(x, "-case behind -caseelse")
			end if
			xblockbegin = x
			x += 1

			select case selectclass
			case ASTCLASS_SELECTVERSION : hExpectStringOrId(x, "<version number> argument")
			case ASTCLASS_SELECTTARGET  : hExpectStringOrId(x, "<target> argument")
			case else                   : hExpectId(x)
			end select
			var n = astNew(ASTCLASS_CASE, tkGetText(x))
			n->location = tkGetLocation(x)
			astAppend(frog.script, n)
			x += 1

		case OPT_CASEELSE
			if frog.script->tail->class = ASTCLASS_CASEELSE then
				tkOops(x, "-caseelse behind -caseelse")
			end if
			astAppend(frog.script, astNew(ASTCLASS_CASEELSE))
			xblockbegin = x
			x += 1

		case OPT_ENDSELECT
			astAppend(frog.script, astNew(ASTCLASS_ENDSELECT))
			x += 1
			exit do

		case else
			tkOopsExpected(x, "-case or -endselect")
		end select
	loop
end sub

private sub hParseIfCompound(byref x as integer)
	'' -ifdef|-iftarget
	var xblockbegin = x
	var is_target = (tkGet(x) = OPT_IFTARGET)
	x += 1

	if is_target then
		'' <target>
		hExpectStringOrId(x, "<target> argument")

		'' -iftarget <target>  =>  -selecttarget -case <target>
		astAppend(frog.script, astNew(ASTCLASS_SELECTTARGET))
		astAppend(frog.script, astNew(ASTCLASS_CASE, tkGetText(x)))
	else
		'' <symbol>
		hExpectId(x)

		'' -ifdef <symbol>  =>  -select -case <symbol>
		astAppend(frog.script, astNew(ASTCLASS_SELECTDEFINE))
		astAppend(frog.script, astNew(ASTCLASS_CASE, tkGetText(x)))
	end if
	x += 1

	do
		hParseArgs(x)

		select case tkGet(x)
		case TK_EOF
			tkOops(xblockbegin, "missing -endif for this")

		case OPT_ELSE
			if frog.script->tail->class = ASTCLASS_CASEELSE then
				tkOops(x, "-else behind -else")
			end if
			astAppend(frog.script, astNew(ASTCLASS_CASEELSE))
			xblockbegin = x
			x += 1

		case OPT_ENDIF
			astAppend(frog.script, astNew(ASTCLASS_ENDSELECT))
			x += 1
			exit do

		case else
			tkOopsExpected(x, iif(tkGet(xblockbegin) = OPT_ELSE, _
					@"-endif", @"-else or -endif"))
		end select
	loop
end sub

private sub hParseDestinationBiFile(byref x as integer)
	'' [<destination .bi file>]
	if hIsStringOrId(x) then
		assert(frog.script->tail->class = ASTCLASS_OPTION)
		astSetAlias(frog.script->tail, tkGetText(x))
		x += 1
	end if
end sub

private sub hParseParam(byref x as integer, byref description as zstring)
	hExpectStringOrId(x, description)
	x += 1
end sub

private sub hParseOption1Param(byref x as integer, byval opt as integer, byref param1 as zstring)
	x += 1
	hParseParam(x, param1 + " parameter for " + tkInfoPretty(opt))
	astAppend(frog.script, astNewOPTION(opt, tkGetText(x - 1)))
end sub

private sub hParseOption2Params(byref x as integer, byval opt as integer, byref param1 as zstring, byref param2 as zstring)
	x += 1
	hParseParam(x, param1)
	hParseParam(x, param2)
	astAppend(frog.script, astNewOPTION(opt, tkGetText(x - 2), tkGetText(x - 1)))
end sub

private sub hParseArgs(byref x as integer)
	static nestinglevel as integer
	static seentarget as integer

	nestinglevel += 1

	while tkGet(x) <> TK_EOF
		var opt = tkGet(x)
		select case as const opt
		case OPT_V
			frog.verbose += 1
			x += 1

		case OPT_O
			x += 1
			hExpectStringOrId(x, "<path/file>")
			frog.outname = hPathRelativeToArgsFile(x)
			x += 1

		'' -emit <filename-pattern> <file>
		case OPT_EMIT
			x += 1

			hExpectStringOrId(x, "<filename-pattern> argument")
			var pattern = *tkGetText(x)
			x += 1

			hExpectStringOrId(x, "<file> argument")
			var filename = *tkGetText(x)
			x += 1

			frogAddBi(filename, pattern)

		'' -dontemit <filename-pattern>
		case OPT_DONTEMIT
			x += 1

			hExpectStringOrId(x, "<filename-pattern> argument")
			frogAddPattern(*tkGetText(x), -1)
			x += 1

		'' (-target <target>)+
		case OPT_TARGET
			x += 1
			'' All OSes and archs default to enabled; disable all the first time
			'' we see -target so that multiple targets can be selected.
			'' (However, '-target linux-x86 -target win64' also enables win32 and linux-x86_64)
			if seentarget = FALSE then
				frogSetTargets(FALSE)
				seentarget = TRUE
			end if

			hExpectStringOrId(x, "<target> argument")
			var s = *tkGetText(x)
			select case s
			case "nodos"
				frogSetTargets(TRUE)
				frog.os(OS_DOS) = FALSE
			case "noarm"
				frogSetTargets(TRUE)
				frog.arch(ARCH_ARM) = FALSE
				frog.arch(ARCH_AARCH64) = FALSE
			case else
				'' Check for <os>-<arch>
				dim archstr as string
				var dash = instr(s, "-")
				if dash then
					archstr = mid(s, dash + 1)
					s = mid(s, 1, dash - 1)
				end if

				'' Check for <os>; set frog.os()
				var os = osParse(s)
				if os < 0 then
					'' Assume it's just an arch
					archstr = s
					frogSetOSes(TRUE)
				else
					frog.os(os) = TRUE
				end if

				'' Check for <arch>; set frog.arch()
				if len(archstr) then
					var arch = archParse(archstr)
					if arch < 0 then
						tkOops(x, "unknown -target argument")
					end if
					frog.arch(arch) = TRUE
				else
					frogSetArchs(TRUE)
				end if
			end select
			x += 1

		'' -title
		case OPT_TITLE
			var begin = x
			x += 1

			'' <package + version>
			hParseParam(x, "<text> parameter for -title")
			var title = tkGetText(x - 1)

			'' original-license.txt
			hParseParam(x, "original-license.txt parameter for -title")
			var licensefile = filebuffersAdd(tkGetText(x - 1), tkGetLocation(x - 1))

			'' translators.txt
			hParseParam(x, "translators.txt parameter for -title")
			var translatorsfile = filebuffersAdd(tkGetText(x - 1), tkGetLocation(x - 1))

			'' [<destination .bi file>]
			dim header as HeaderInfo ptr
			if hIsStringOrId(x) then
				'' .bi-specific -title option
				var bi = frogLookupBiFromBi(*tkGetText(x))
				if bi < 0 then
					tkOops(x, "unknown destination .bi")
				end if
				header = @frog.bis[bi].header
				x += 1
			else
				'' global -title option
				header = @frog.header
			end if

			if len(header->title) > 0 then
				tkOops(begin, "duplicate -title option")
			end if
			header->title = *title
			header->licensefile = licensefile
			header->translatorsfile = translatorsfile

		'' -declareversions <symbol> (<string>)+
		case OPT_DECLAREVERSIONS
			if frog.have_declareversions then
				tkOops(x, "multiple -declareversions options, only 1 is allowed")
			end if
			frog.have_declareversions = TRUE
			x += 1

			'' <symbol>
			hExpectId(x)
			frog.versiondefine = *tkGetText(x)
			x += 1

			'' The version numbers must be given in ascending order,
			'' allowing us to optimize things like:
			''      (ID = a) or (ID = b) or (ID = c)
			'' to:
			''      ID <= c

			'' (<string>)+
			if tkGet(x) <> TK_STRING then
				tkOopsExpected(x, "<version number> argument")
			end if
			var xfirst = x
			do
				var verstr = *tkGetText(x)

				'' Verify that new version number is >= the previous one (unless this is the first one)
				if xfirst < x then
					var prev = *tkGetText(x - 1)
					if valint(prev) >= valint(verstr) then
						tkOops(x, "version '" + prev + "' >= '" + verstr + "', but should be < to maintain order")
					end if
				end if

				frogAddVernum(verstr)

				x += 1
			loop while tkGet(x) = TK_STRING

			astAppend(frog.script, astNew(ASTCLASS_DECLAREVERSIONS))

		'' -declarebool <symbol>
		case OPT_DECLAREBOOL
			x += 1

			'' <symbol>
			hExpectId(x)
			astAppend(frog.script, astNew(ASTCLASS_DECLAREBOOL, tkGetText(x)))
			x += 1

		case OPT_SELECTTARGET  : hParseSelectCompound(x, ASTCLASS_SELECTTARGET)
		case OPT_SELECTVERSION : hParseSelectCompound(x, ASTCLASS_SELECTVERSION)
		case OPT_SELECTDEFINE  : hParseSelectCompound(x, ASTCLASS_SELECTDEFINE)

		case OPT_IFTARGET, OPT_IFDEF
			hParseIfCompound(x)

		case OPT_CASE, OPT_CASEELSE, OPT_ENDSELECT, OPT_ELSE, OPT_ENDIF
			if nestinglevel <= 1 then
				select case tkGet(x)
				case OPT_CASE      : tkOops(x, "-case without -select")
				case OPT_CASEELSE  : tkOops(x, "-caseelse without -select")
				case OPT_ENDSELECT : tkOops(x, "-endselect without -select")
				case OPT_ELSE      : tkOops(x, "-else without -ifdef")
				case else          : tkOops(x, "-endif without -ifdef")
				end select
			end if
			exit while

		'' -define <id> [<body>]
		case OPT_DEFINE
			x += 1

			hExpectId(x)
			var id = tkGetText(x)
			x += 1

			dim body as zstring ptr
			if hIsStringOrId(x) then
				body = tkGetText(x)
				x += 1
			end if

			astAppend(frog.script, astNewOPTION(opt, id, body))

		case OPT_INCLUDE
			hParseOption1Param(x, opt, "<file>")

		case OPT_FBFROGINCLUDE
			hParseOption1Param(x, opt, "<file>")

		case OPT_INCDIR
			x += 1
			hExpectStringOrId(x, "<path>")
			astAppend(frog.script, astNewOPTION(opt, hPathRelativeToArgsFile(x)))
			x += 1

		case OPT_WINDOWSMS, OPT_CLONG32, OPT_FIXUNSIZEDARRAYS, _
		     OPT_NOFUNCTIONBODIES, OPT_DROPMACROBODYSCOPES, OPT_REMOVEEMPTYRESERVEDDEFINES
			x += 1
			astAppend(frog.script, astNewOPTION(opt))

		case OPT_RENAMETYPEDEF, OPT_RENAMETAG, OPT_RENAMEPROC, OPT_RENAMEDEFINE, _
		     OPT_RENAMEMACROPARAM, OPT_RENAME
			hParseOption2Params(x, opt, "<oldid>", "<newid>")

		case OPT_RENAME_, OPT_REMOVE, OPT_REMOVEDEFINE, OPT_REMOVEPROC, OPT_REMOVEVAR, OPT_REMOVE1ST, OPT_REMOVE2ND, _
		     OPT_DROPPROCBODY, OPT_TYPEDEFHINT, OPT_ADDFORWARDDECL, OPT_UNDEFBEFOREDECL, OPT_IFNDEFDECL, _
		     OPT_CONVBODYTOKENS, OPT_FORCEFUNCTION2MACRO, OPT_EXPANDINDEFINE, OPT_NOEXPAND
			hParseOption1Param(x, opt, "<id>")

		case OPT_EXPAND
			hParseOption1Param(x, opt, "<id-pattern>")

		case OPT_NOSTRING, OPT_STRING
			hParseOption1Param(x, opt, "<decl-pattern>")

		case OPT_REMOVEINCLUDE
			hParseOption1Param(x, opt, "<filename>")

		case OPT_SETARRAYSIZE
			hParseOption2Params(x, opt, "<id>", "<size>")

		case OPT_MOVEABOVE
			hParseOption2Params(x, opt, "<id>", "<ref>")

		case OPT_REPLACEMENTS
			hParseOption1Param(x, opt, "<file>")

		case OPT_INCLIB
			hParseOption1Param(x, opt, "<name>")
			hParseDestinationBiFile(x)

		case OPT_UNDEF
			hParseOption1Param(x, opt, "<id>")
			hParseDestinationBiFile(x)

		case OPT_ADDINCLUDE
			hParseOption1Param(x, opt, "<.bi file>")
			hParseDestinationBiFile(x)

		case else
			'' *.fbfrog file given (without @)? Treat as @file too
			var filename = *tkGetText(x)
			if pathExtOnly(filename) = "fbfrog" then
				hLoadArgsFile(x + 1, filename, tkGetLocation(x))
				tkRemove(x, x)

				'' Must expand @files again in case the loaded file contained any
				hExpandArgsFiles()
			else
				'' Input file
				var filename = hPathRelativeToArgsFile(x)
				astAppend(frog.script, astNewOPTION(OPT_I, filename))

				'' The first .h file name seen will be used as default name for the ouput .bi,
				'' in case no explicit .bi file names are given via -emit or -o
				if len((frog.defaultoutname)) = 0 then
					frog.defaultoutname = pathStripExt(filename) + ".bi"
				end if

				x += 1
			end if
		end select
	wend

	nestinglevel -= 1
end sub

private function hSkipToEndOfBlock(byval i as ASTNODE ptr) as ASTNODE ptr
	var level = 0

	do
		select case i->class
		case ASTCLASS_SELECTTARGET, ASTCLASS_SELECTVERSION, ASTCLASS_SELECTDEFINE
			level += 1

		case ASTCLASS_CASE, ASTCLASS_CASEELSE
			if level = 0 then
				exit do
			end if

		case ASTCLASS_ENDSELECT
			if level = 0 then
				exit do
			end if
			level -= 1
		end select

		i = i->next
	loop

	function = i
end function

private sub frogAddApi(byval verand as ASTNODE ptr, byval script as ASTNODE ptr, byval target as TargetInfo)
	assert(astIsVERAND(verand))
	var i = frog.apicount
	frog.apicount += 1
	frog.apis = reallocate(frog.apis, frog.apicount * sizeof(*frog.apis))
	with frog.apis[i]
		.constructor()
		.verand = verand
		.script = script
		.target = target
	end with
end sub

'' Pattern matching for the -selecttarget/-iftarget options
'' Example patterns:
''    64bit      =>  matches all 64bit targets
''    windows    =>  matches all Windows targets
''    dos        =>  matches dos only
''    linux-x86  =>  matches linux-x86 only
private function hTargetPatternMatchesTarget(byref pattern as string, byval target as TargetInfo) as integer
	var targetos = *osinfo(target.os).id
	var targetarch = *archinfo(target.arch).id

	select case pattern
	case "64bit" : return     archinfo(target.arch).is_64bit
	case "32bit" : return not archinfo(target.arch).is_64bit
	case "win32" : return (target.os = OS_WINDOWS) and (target.arch = ARCH_X86   )
	case "win64" : return (target.os = OS_WINDOWS) and (target.arch = ARCH_X86_64)
	case "unix"  : return osinfo(target.os).is_unix
	case targetos : return TRUE
	case targetarch : return TRUE
	end select

	'' <os>-<arch>?
	dim as string os, arch
	strSplit(pattern, "-", os, arch)
	return (targetos = os) and (targetarch = arch)
end function

''
'' The script is a linear list of the command line options, for example:
'' (each line is a sibling AST node)
''    select __LIBFOO_VERSION
''    case 1
''    #define VERSION 1
''    case 2
''    #define VERSION 2
''    endselect
''    ifdef UNICODE
''    #define UNICODE
''    else
''    #define ANSI
''    endif
''    #define COMMON
'' We want to follow each possible code path to determine which versions fbfrog
'' should work with and what their options are. The possible code paths for this
'' example are:
''    <conditions>                                <options>
''    __LIBFOO_VERSION=1, defined(UNICODE)     => #define VERSION 1, #define UNICODE, #define COMMON
''    __LIBFOO_VERSION=2, defined(UNICODE)     => #define VERSION 2, #define UNICODE, #define COMMON
''    __LIBFOO_VERSION=1, not defined(UNICODE) => #define VERSION 1, #define ANSI, #define COMMON
''    __LIBFOO_VERSION=2, not defined(UNICODE) => #define VERSION 2, #define ANSI, #define COMMON
''
'' All the evaluation code assumes that the script is valid, especially that
'' if/else/endif and select/case/endselect nodes are properly used.
''
'' In order to evaluate multiple code paths, we start walking at the beginning,
'' and start a recursive call at every condition. The if path of an -ifdef is
'' evaluated by a recursive call, and we then go on to evaluate the else path.
'' Similar for -select's, except that there can be 1..n possible code paths
'' instead of always 2. Each -case code path except the last is evaluated by a
'' recursive call, and then we go on to evaluate the last -case code path.
''
'' Evaluating the first (couple) code path(s) first, and the last code path
'' last, means that they'll be evaluated in the order they appear in the script,
'' and the results will be in the pretty order expected by the user.
''
'' While evaluating, we keep track of the conditions and visited options of each
'' code path. Recursive calls are given the conditions/options so far seen by
'' the parent. This way, common conditions/options from before the last
'' conditional branch are passed into each code path resulting from the
'' conditional branch.
''
private sub frogEvaluateScript _
	( _
		byval start as ASTNODE ptr, _
		byval conditions as ASTNODE ptr, _
		byval options as ASTNODE ptr, _
		byval target as TargetInfo _
	)

	var i = start
	while i

		select case i->class
		case ASTCLASS_DECLAREVERSIONS
			i = i->next

			var lastvernum = ubound(frog.vernums)

			'' Evaluate a separate code path for each version,
			'' branching for every version except the last
			for vernum as integer = 0 to lastvernum - 1
				'' <symbol> = <versionnumber>
				frogEvaluateScript(i, _
					astNewGROUP(astClone(conditions), astNewVERNUMCHECK(vernum)), _
					astClone(options), _
					target)
			next

			'' Now continue without recursing and evaluate the code path for the last version
			astAppend(conditions, astNewVERNUMCHECK(lastvernum))

		case ASTCLASS_DECLAREBOOL
			var symbol = i->text
			i = i->next

			var completeveror = astNew(ASTCLASS_VEROR)

			'' Branch for the true code path
			'' defined(<symbol>)
			var condition = astNewDEFINED(symbol)
			astAppend(completeveror, astClone(condition))
			frogEvaluateScript(i, _
				astNewGROUP(astClone(conditions), astClone(condition)), _
				astClone(options), _
				target)

			'' And follow the false code path here
			'' (not defined(<symbol>))
			condition = astNew(ASTCLASS_NOT, condition)
			astAppend(completeveror, astClone(condition))
			astAppend(conditions, condition)

			astAppend(frog.completeverors, completeveror)

		case ASTCLASS_SELECTTARGET, ASTCLASS_SELECTVERSION, ASTCLASS_SELECTDEFINE
			var sel = i
			i = i->next

			do
				'' -case
				assert(i->class = ASTCLASS_CASE)

				'' Evaluate -case condition
				dim is_true as integer

				if sel->class = ASTCLASS_SELECTTARGET then
					is_true = hTargetPatternMatchesTarget(*i->text, target)
				else
					dim condition as ASTNODE ptr
					if sel->class = ASTCLASS_SELECTVERSION then
						'' <versionnumber>
						var vernum = frogLookupVernum(*i->text)
						if vernum < 0 then
							oopsLocation(i->location, "unknown version number; it didn't appear in the previous -declareversions option")
						end if
						condition = astNewVERNUMCHECK(vernum)
					else
						'' defined(<symbol>)
						condition = astNewDEFINED(i->text)
					end if
					is_true = astGroupContains(conditions, condition)
					astDelete(condition)
				end if

				i = i->next

				'' Evaluate the first -case block whose condition is true
				if is_true then
					exit do
				end if

				'' Condition was false, skip over the -case's body
				var eob = hSkipToEndOfBlock(i)
				select case eob->class
				case ASTCLASS_CASEELSE, ASTCLASS_ENDSELECT
					'' Reached -caseelse/-endselect
					i = eob->next
					exit do
				end select

				'' Go to next -case
				i = eob
			loop

		case ASTCLASS_CASE, ASTCLASS_CASEELSE
			'' When reaching a case/else block instead of the corresponding
			'' select, that means we're evaluating the code path of the
			'' previous case code path, and must now step over the
			'' block(s) of the alternate code path(s).
			i = hSkipToEndOfBlock(i->next)
			assert((i->class = ASTCLASS_CASE) or _
				(i->class = ASTCLASS_CASEELSE) or _
				(i->class = ASTCLASS_ENDSELECT))

		case ASTCLASS_ENDSELECT
			'' Ignore - nothing to do
			i = i->next

		case else
			astAppend(options, astClone(i))
			i = i->next
		end select
	wend

	assert(conditions->class = ASTCLASS_GROUP)
	conditions->class = ASTCLASS_VERAND
	frogAddApi(conditions, options, target)
end sub

private sub maybeEvalForTarget(byval os as integer, byval arch as integer)
	if frog.arch(arch) then
		frogEvaluateScript(frog.script->head, astNewGROUP(), astNewGROUP(), type<TargetInfo>(os, arch))
	end if
end sub

private sub maybeEvalForOs(byval os as integer)
	if frog.os(os) = FALSE then exit sub

	maybeEvalForTarget(os, ARCH_X86)
	if osinfo(os).has_64bit then
		maybeEvalForTarget(os, ARCH_X86_64)
	end if

	if osinfo(os).has_arm then
		maybeEvalForTarget(os, ARCH_ARM)
		if osinfo(os).has_64bit then
			maybeEvalForTarget(os, ARCH_AARCH64)
		end if
	end if
end sub

private function frogParse(byref api as ApiInfo) as ASTNODE ptr
	tkInit()

	'' C preprocessing
	cppInit(api)

	'' Add fbfrog's CPP pre-#defines for preprocessing of default.h
	cppAddTargetPredefines(api.target)

	scope
		'' Pre-#defines are simply inserted at the top of the token
		'' buffer, so that cppMain() parses them like any other #define.

		var i = api.script->head
		while i

			assert(i->class = ASTCLASS_OPTION)
			select case i->opt
			case OPT_DEFINE
				cppAddPredefine(i->text, i->alias)
			case OPT_INCDIR
				cppAddIncDir(i->text)
			end select

			i = i->next
		wend
	end scope

	'' Insert the code from fbfrog pre-#includes (default.h etc.)
	'' * behind command line pre-#defines so that default.h can use them
	'' * marked for removal so the code won't be preserved
	scope
		var i = api.script->head
		while i
			assert(i->class = ASTCLASS_OPTION)
			if i->opt = OPT_FBFROGINCLUDE then
				var filename = hFindResource(*i->text)
				var x = tkGetCount()
				var file = filebuffersAdd(filename, type(NULL, 0))
				lexLoadC(x, file->buffer, file->source)
				tkSetRemove(x, tkGetCount() - 1)
			end if
			i = i->next
		wend
	end scope

	'' Add #includes for pre-#includes
	scope
		var i = api.script->head
		while i
			assert(i->class = ASTCLASS_OPTION)
			if i->opt = OPT_INCLUDE then
				cppAppendIncludeDirective(i->text, TKFLAG_PREINCLUDE)
			end if
			i = i->next
		wend
	end scope

	''
	'' Add #include statements for the toplevel file(s) behind current
	'' tokens, but marked with TKFLAG_ROOTFILE to let the CPP know that no
	'' #include search should be done.
	''
	'' This way we can re-use the #include handling code to load the
	'' toplevel files. (especially interesting for include guard optimization)
	''
	'' Note: pre-#defines should appear before tokens from root files, such
	'' that the order of -define vs *.h command line arguments doesn't
	'' matter.
	''
	scope
		var i = api.script->head
		while i
			assert(i->class = ASTCLASS_OPTION)
			if i->opt = OPT_I then
				cppAppendIncludeDirective(i->text, TKFLAG_ROOTFILE)
			end if
			i = i->next
		wend
	end scope

	cppMain()

	cppEnd()

	'' Remove CPP directives and EOLs (tokens marked for removal by
	'' cppMain()). Doing this as a separate step allows
	'' * error reports during cppMain() to view the complete input
	'' * cppMain() to reference #define directives based on token position
	''   (to retrieve the bodies for macro expansion) as opposed to having
	''   to load them into AST
	tkApplyRemoves()

	hMoveDirectivesOutOfConstructs()

	if api.replacementcount > 0 then
		hApplyReplacements()
	end if

	tkTurnCPPTokensIntoCIds()

	'' C parsing
	cInit(api)
	var ast = cMain()
	cEnd()

	tkEnd()

	hlGlobal(ast, api)

	function = ast
end function

private function hMakeProgressString(byval position as integer, byval total as integer) as string
	var sposition = str(position), stotal = str(total)
	sposition = string(len(stotal) - len(sposition), " ") + sposition
	function = "[" + sposition + "/" + stotal + "]"
end function

private function hMakeCountMessage(byval count as integer, byref noun as string) as string
	if count = 1 then
		function = "1 " + noun
	else
		function = count & " " + noun + "s"
	end if
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	if  __FB_ARGC__ <= 1 then
		hPrintHelpAndExit()
	end if

	frogSetTargets(TRUE)

	tkInit()

	'' Load all command line arguments into the tk buffer
	lexLoadArgs(0, hTurnArgsIntoString(__FB_ARGC__, __FB_ARGV__), _
		sourceinfoForZstring("<command line>"))

	'' Load content of @files too
	hExpandArgsFiles()

	'' Parse the command line arguments, skipping argv[0]. Global options
	'' are added to various frog.* fields, version-specific options are
	'' added to the frog.script list in their original order.
	frog.script = astNewGROUP()
	hParseArgs(1)

	tkEnd()

	'' Add the implicit default.h pre-#include
	astPrepend(frog.script, astNewOPTION(OPT_FBFROGINCLUDE, "default.h"))

	'' Determine the APIs and their individual options
	'' - The API-specific command line options were stored in the "script",
	''   this must be evaluated now, following each possible code path.
	'' - The built-in targets representing the "base" set of APIs are
	''   hard-coded here in form of loops
	'' - Any -declare* options in the script trigger recursive evaluation
	frog.completeverors = astNewGROUP()
	frog.enabledoscount = 0
	for os as integer = 0 to OS__COUNT - 1
		if frog.os(os) then frog.enabledoscount += 1
	next
	scope
		'' Fill in frog.completeverors for the built-in targets

		if frog.enabledoscount > 1 then
			'' 1. one VEROR covering all enabled OS conditions
			var osveror = astNewVEROR()
			for os as integer = 0 to OS__COUNT - 1
				if frog.os(os) then
					astAppend(osveror, astNewDEFINEDfbos(os))
				end if
			next
			astAppend(frog.completeverors, osveror)
		end if

		'' 2. one VEROR covering the __FB_64BIT__ boolean (defined + not defined)
		'' This assumes that 32bit + 64bit architectures are always enabled.
		'' TODO: support cases like 32bit-only?
		astAppend(frog.completeverors, _
			astNewVEROR(astNewDEFINEDfb64(FALSE), _
			            astNewDEFINEDfb64(TRUE)))

		'' same for __FB_ARM__
		astAppend(frog.completeverors, _
			astNewVEROR(astNewDEFINEDfbarm(FALSE), _
			            astNewDEFINEDfbarm(TRUE)))
	end scope
	for os as integer = 0 to OS__COUNT - 1
		maybeEvalForOs(os)
	next
	assert(frog.apicount > 0)

	if frog.apicount > ApiBits.MaxApis then
		oops(frog.apicount & " APIs -- that's too many, max. is " & ApiBits.MaxApis & ", sorry")
	else
		for i as integer = 0 to frog.apicount - 1
			frog.fullapis.set(i)
		next
	end if

	'' If no output .bi files were given via -emit options on the command line,
	'' we default to emitting one output .bi, much like: -emit '*' default.bi
	if frog.bicount = 0 then
		if len((frog.defaultoutname)) = 0 then
			frog.defaultoutname = "unknown.bi"
		end if
		if len((frog.outname)) = 0 then
			frog.outname = frog.defaultoutname
		elseif pathIsDir(frog.outname) then
			frog.outname = pathAddDiv(frog.outname) + pathStrip(frog.defaultoutname)
		end if
		frogAddBi(frog.outname, "*")
	end if

	'' For each version, parse the input into an AST, using the options for
	'' that version, and then merge the AST with the previous one, so that
	'' finally we get a single AST representing all versions.
	''
	'' Doing the merging here step-by-step vs. collecting all ASTs and then
	'' merging them afterwards: Merging here immediately saves memory, and
	'' also means that the slow merging process for a version happens after
	'' parsing that version. Instead of one single big delay at the end,
	'' there is a small delay at each version.
	dim as ASTNODE ptr final
	for api as integer = 0 to frog.apicount - 1
		print hMakeProgressString(api + 1, frog.apicount) + " " + frog.apis[api].prettyId()

		'' Prepare the API options for the following cpp/c/highlevel steps (load them into hash tables etc.)
		'' This writes into frog.bis to fill each .bi's ApiSpecificBiOptions.
		frog.apis[api].loadOptions()

		'' Parse code for this API into an AST
		var ast = frogParse(frog.apis[api])

		'' Prepare "incoming" trees
		for bi as integer = 0 to frog.bicount - 1
			assert(frog.bis[bi].incoming = NULL)
			frog.bis[bi].incoming = astNewGROUP()
		next

		'' Split the big tree into separate "incoming" trees on each .bi file
		scope
			dim bi as integer
			dim prevsource as SourceInfo ptr

			var i = ast->head
			while i
				var nxt = i->next

				assert(i->location.source)
				assert(i->location.source->is_file)

				'' Find out into which .bi file this declaration should be put.
				'' If this declaration has the same source as the previous one,
				'' then re-use the previously calculated .bi file, instead of
				'' redoing the lookup. Otherwise, do the lookup and cache the result.
				if prevsource <> i->location.source then
					bi = frogLookupBiFromH(i->location.source->name)
					prevsource = i->location.source
				end if

				if bi >= 0 then
					'' Add the declaration to the "incoming" AST for that .bi file
					astUnlink(ast, i)
					astAppend(frog.bis[bi].incoming, i)
				end if

				i = nxt
			wend
		end scope

		'' Forget the remaining AST. Anything that wasn't moved into the
		'' .bi files won't be emitted.
		astDelete(ast)

		dim apibit as ApiBits
		apibit.set(api)

		for bi as integer = 0 to frog.bicount - 1
			with frog.bis[bi]
				'' Do file-specific AST work (e.g. add Extern block)
				hlFile(.incoming, frog.apis[api], .options)

				'' Merge the "incoming" tree into the "final" tree
				astMergeNext(apibit, .final, .incoming)

				assert(.incoming = NULL)
				assert(.options.inclibs = NULL)
				assert(.options.undefs = NULL)
				assert(.options.addincludes = NULL)
			end with
		next

		astMergeNext(apibit, frog.mergedlog, frog.apis[api].log)
		assert(frog.apis[api].log = NULL)

		frog.fullveror = astNewVEROR(frog.fullveror, astClone(frog.apis[api].verand))
	next

	'' Print the merged list of #included files
	'' This should be useful because it allows the user to see which input
	'' files were used, found, not found, which additional files were
	'' #included, etc.
	astProcessVerblocks(frog.mergedlog)
	emitFbStdout(frog.mergedlog, 1)

	for bi as integer = 0 to frog.bicount - 1
		with frog.bis[bi]
			'' Turn VERBLOCKs into #ifs etc.
			astProcessVerblocks(.final)

			'' Prepend #pragma once
			'' It's always needed, except if the binding is empty: C headers
			'' typically have #include guards, but we don't preserve those.
			if .final->head then
				astPrepend(.final, astNew(ASTCLASS_PRAGMAONCE))
			end if

			hlAutoAddDividers(.final)

			'' Use .bi-specific header, if any; fallback to global header, if any
			dim header as HeaderInfo ptr
			if len(.header.title) > 0 then
				header = @.header
			elseif len((frog.header.title)) > 0 then
				header = @frog.header
			end if

			'' Write out the .bi file.
			var bifilename = *.filename
			print "emitting: " + bifilename + " (" + _
				hMakeCountMessage(hlCountDecls(.final), "declaration") + ", " + _
				hMakeCountMessage(hlCountTodos(.final), "TODO"       ) + ")"
			emitFbFile(bifilename, header, .final)
		end with
	next

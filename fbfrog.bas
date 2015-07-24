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
#include once "file.bi"

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
	dim shared as integer verbose, nodefaultscript
	dim shared as string outname, defaultoutname, prefix
	dim shared header as HeaderInfo  '' global titles etc. - will be added to all generated .bi files

	dim shared as ASTNODE ptr script
	dim shared as ASTNODE ptr completeverors, fullveror
	dim shared as ApiInfo ptr apis
	dim shared as integer apicount

	'' *.bi output file names from the -emit options
	dim shared as BIFILE ptr bis
	dim shared as integer bicount
	dim shared as THASH ucasebihash
	dim shared as THASH bilookupcache

	'' *.h file name patterns from the -emit options, associated to the
	'' corresponding bis array index
	type HPATTERN
		pattern as string
		bi as integer
	end type
	dim shared as HPATTERN ptr patterns
	dim shared as integer patterncount
end namespace

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
	var item = hashLookup(@frog.ucasebihash, ucasefilename, ucasehash)
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
	var item = hashLookup(@frog.ucasebihash, ucasefilename, ucasehash)
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
		hashAdd(@frog.ucasebihash, item, ucasehash, ucasefilename, cptr(any ptr, bi))
	end if

	frogAddPattern(pattern, bi)
end sub

function frogLookupBiFromH(byval hfile as zstring ptr) as integer
	'' Check whether we've already cached the .bi for this .h file
	var hfilehash = hashHash(hfile)
	var item = hashLookup(@frog.bilookupcache, hfile, hfilehash)
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
	hashAdd(@frog.bilookupcache, item, hfilehash, hfile, cptr(any ptr, bi))
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
	print "fbfrog 1.9 (" + __DATE_ISO__ + "), FreeBASIC *.bi binding generator"
	print "usage: fbfrog foo.h [options]"
	print "global options:"
	print "  @<file> | <*.fbfrog>  Read more command line arguments from a file"
	print "  -o <path/file>        Set output .bi file name, or just the output directory"
	print "  -emit '*.h' foo.bi    Emit code from matching .h into specified .bi"
	print "  -dontemit '*.h'       Drop code from matching .h files"
	print "  -title <package + version> original-license.txt translators.txt [<destination .bi file>]"
	print "     Add text at the top of .bi file(s): package name + version, copyright, license"
	print "  -v                    Show verbose/debugging info"
	print "  -nodefaultscript      Don't use default.fbfrog implicitly"
	print "API script logic:"
	print "  -declaredefines (<symbol>)+               Exclusive #defines"
	print "  -declareversions <symbol> (<number>)+     Version numbers"
	print "  -declarebool <symbol>                     Single on/off #define"
	print "  -select          (-case <symbol> ...)+ [-caseelse ...] -endselect"
	print "  -select <symbol> (-case <number> ...)+ [-caseelse ...] -endselect"
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
	print "  -disableconstants  Don't turn #defines into constants"
	print "  -nofunctionbodies  Don't preserve function bodies"
	print "  -dropmacrobodyscopes  Drop scope blocks with only 1 statement in macro bodies"
	print "  -replacements <file>  Load patterns for search/replace"
	print "  options for renaming symbols (-rename* <oldid> <newid>):"
	print "    -renametypedef, -renametag (struct/union/enum),"
	print "    -renameproc (procedures)"
	print "    -renamedefine, -renamemacroparam"
	print "    -rename (any matching symbol)"
	print "  options for removing declarations (-remove* <id>):"
	print "    -removedefine, -removeproc, -removevar, -remove1st, -remove2nd"
	print "  -dropprocbody <id>  Don't preserve a certain procedure's body"
	print "  -typedefhint <id>   Mark <id> as typedef, to help parsing of type casts"
	print "  -addforwarddecl <id>  Force a forward declaration to be added for the given type"
	print "  -undefbeforedecl <id>  Insert an #undef above a declaration"
	print "  -nostring <id>      Prevent a symbol from being turned into a zstring"
	print "  -string <id>        Force a [U]Byte [Ptr] symbol to be turned into a ZString [Ptr]"
	print "  -convbodytokens <id>  Translate a #define's body only by converting the tokens, no parsing"
	print "  -expandindefine <id>  Expand macro in #define body"
	print "  -noexpand <id>      Disable expansion of certain #define"
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
		hashInit(@renameopt(i), 3, FALSE)
	next
	for i as integer = lbound(idopt) to ubound(idopt)
		hashInit(@idopt(i), 3, FALSE)
	next
	hashInit(@removeinclude, 3, FALSE)
	hashInit(@setarraysizeoptions, 3, FALSE)
end constructor

destructor ApiInfo()
	for i as integer = lbound(renameopt) to ubound(renameopt)
		hashEnd(@renameopt(i))
	next
	for i as integer = lbound(idopt) to ubound(idopt)
		hashEnd(@idopt(i))
	next
	hashEnd(@removeinclude)
	hashEnd(@setarraysizeoptions)

	for i as integer = 0 to replacementcount - 1
		deallocate(replacements[i].fromcode)
		deallocate(replacements[i].tocode)
	next
	deallocate(replacements)
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
	case OPT_DISABLECONSTANTS : disableconstants = TRUE
	case OPT_NOFUNCTIONBODIES : nofunctionbodies = TRUE
	case OPT_DROPMACROBODYSCOPES : dropmacrobodyscopes = TRUE

	case OPT_RENAMETYPEDEF, OPT_RENAMETAG, OPT_RENAMEPROC, _
	     OPT_RENAMEDEFINE, OPT_RENAMEMACROPARAM, OPT_RENAME
		hashAddOverwrite(@renameopt(opt), param1, param2)
		have_renames = TRUE

	case OPT_REMOVE, OPT_REMOVEDEFINE, OPT_REMOVEPROC, OPT_REMOVEVAR, OPT_REMOVE1ST, OPT_REMOVE2ND, _
	     OPT_DROPPROCBODY, OPT_TYPEDEFHINT, OPT_ADDFORWARDDECL, OPT_UNDEFBEFOREDECL, _
	     OPT_NOSTRING, OPT_STRING, OPT_CONVBODYTOKENS, OPT_EXPANDINDEFINE, OPT_NOEXPAND
		hashAddOverwrite(@idopt(opt), param1, NULL)

	case OPT_REMOVEINCLUDE
		hashAddOverwrite(@removeinclude, param1, NULL)

	case OPT_SETARRAYSIZE
		hashAddOverwrite(@setarraysizeoptions, param1, param2)

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

private sub hParseSelectCompound(byref x as integer)
	'' -select
	astAppend(frog.script, astNew(ASTCLASS_SELECT))
	var xblockbegin = x
	x += 1

	'' [<symbol>]
	dim as zstring ptr selectsymbol
	if tkGet(x) = TK_ID then
		selectsymbol = tkGetText(x)
		x += 1
	end if

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

			dim as ASTNODE ptr condition
			if selectsymbol then
				'' <version number>
				hExpectStringOrId(x, "<version number> argument")

				'' <symbol> = <versionnumber>
				condition = astNew(ASTCLASS_EQ, astNewTEXT(selectsymbol), astNewTEXT(tkGetText(x)))
			else
				'' <symbol>
				hExpectId(x)

				'' defined(<symbol>)
				condition = astNewDEFINED(tkGetText(x))
			end if
			var n = astNew(ASTCLASS_CASE)
			n->expr = condition
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

private sub hParseIfDefCompound(byref x as integer)
	'' -ifdef
	var xblockbegin = x
	x += 1

	'' <symbol>
	hExpectId(x)
	'' -ifdef <symbol>  =>  -select -case <symbol>
	astAppend(frog.script, astNew(ASTCLASS_SELECT))
	scope
		var n = astNew(ASTCLASS_CASE)
		n->expr = astNewDEFINED(tkGetText(x))
		astAppend(frog.script, n)
	end scope
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

	nestinglevel += 1

	while tkGet(x) <> TK_EOF
		var opt = tkGet(x)
		select case as const opt
		case OPT_NODEFAULTSCRIPT  : frog.nodefaultscript  = TRUE : x += 1
		case OPT_V                : frog.verbose          = TRUE : x += 1

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

		'' -declaredefines (<symbol>)+
		case OPT_DECLAREDEFINES
			x += 1

			'' (<symbol>)+
			var n = astNew(ASTCLASS_DECLAREDEFINES)
			hExpectId(x)
			do
				astAppend(n, astNewTEXT(tkGetText(x)))
				x += 1
			loop while tkGet(x) = TK_ID

			astAppend(frog.script, n)

		'' -declareversions <symbol> (<string>)+
		case OPT_DECLAREVERSIONS
			x += 1

			'' <symbol>
			hExpectId(x)
			var n = astNew(ASTCLASS_DECLAREVERSIONS, tkGetText(x))
			x += 1

			'' (<string>)+
			if tkGet(x) <> TK_STRING then
				tkOopsExpected(x, "<version number> argument")
			end if
			do
				astAppend(n, astNewTEXT(tkGetText(x)))
				x += 1
			loop while tkGet(x) = TK_STRING

			astAppend(frog.script, n)

		'' -declarebool <symbol>
		case OPT_DECLAREBOOL
			x += 1

			'' <symbol>
			hExpectId(x)
			astAppend(frog.script, astNew(ASTCLASS_DECLAREBOOL, tkGetText(x)))
			x += 1

		case OPT_SELECT
			hParseSelectCompound(x)

		case OPT_IFDEF
			hParseIfDefCompound(x)

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

		case OPT_WINDOWSMS, OPT_CLONG32, OPT_FIXUNSIZEDARRAYS, OPT_DISABLECONSTANTS, _
		     OPT_NOFUNCTIONBODIES, OPT_DROPMACROBODYSCOPES
			x += 1
			astAppend(frog.script, astNewOPTION(opt))

		case OPT_RENAMETYPEDEF, OPT_RENAMETAG, OPT_RENAMEPROC, OPT_RENAMEDEFINE, _
		     OPT_RENAMEMACROPARAM, OPT_RENAME
			hParseOption2Params(x, opt, "<oldid>", "<newid>")

		case OPT_REMOVE, OPT_REMOVEDEFINE, OPT_REMOVEPROC, OPT_REMOVEVAR, OPT_REMOVE1ST, OPT_REMOVE2ND, _
		     OPT_DROPPROCBODY, OPT_TYPEDEFHINT, OPT_ADDFORWARDDECL, OPT_UNDEFBEFOREDECL, _
		     OPT_NOSTRING, OPT_STRING, OPT_CONVBODYTOKENS, OPT_EXPANDINDEFINE, OPT_NOEXPAND
			hParseOption1Param(x, opt, "<id>")

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
		case ASTCLASS_SELECT
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

private sub frogAddApi(byval verand as ASTNODE ptr, byval script as ASTNODE ptr)
	assert(astIsVERAND(verand))
	var i = frog.apicount
	frog.apicount += 1
	frog.apis = reallocate(frog.apis, frog.apicount * sizeof(*frog.apis))
	with frog.apis[i]
		.constructor()
		.verand = verand
		.script = script
	end with
end sub

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
		byval options as ASTNODE ptr _
	)

	var i = start
	while i

		select case i->class
		case ASTCLASS_DECLAREDEFINES, ASTCLASS_DECLAREVERSIONS
			var decl = i
			i = i->next

			var completeveror = astNew(ASTCLASS_VEROR)

			'' Evaluate a separate code path for each #define/version
			var k = decl->head
			do
				dim as ASTNODE ptr condition
				if decl->class = ASTCLASS_DECLAREDEFINES then
					'' defined(<symbol>)
					condition = astNewDEFINED(k->text)
				else
					'' <symbol> = <versionnumber>
					condition = astNew(ASTCLASS_EQ, astNewTEXT(decl->text), astClone(k))
				end if
				astAppend(completeveror, astClone(condition))

				k = k->next
				if k = NULL then
					'' This is the last #define/version, so don't branch
					astAppend(conditions, condition)
					exit do
				end if

				'' Branch for this #define/version
				frogEvaluateScript(i, _
					astNewGROUP(astClone(conditions), condition), _
					astClone(options))
			loop

			astAppend(frog.completeverors, completeveror)

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
				astClone(options))

			'' And follow the false code path here
			'' (not defined(<symbol>))
			condition = astNew(ASTCLASS_NOT, condition)
			astAppend(completeveror, astClone(condition))
			astAppend(conditions, condition)

			astAppend(frog.completeverors, completeveror)

		case ASTCLASS_SELECT
			var selectnode = i
			i = i->next

			do
				'' -case
				assert(i->class = ASTCLASS_CASE)
				var condition = i->expr
				i = i->next

				'' Evaluate the first -case whose condition is true
				if astGroupContains(conditions, condition) then
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
	frogAddApi(conditions, options)
end sub

private function frogParse(byref api as ApiInfo) as ASTNODE ptr
	tkInit()

	'' C preprocessing
	cppInit(api)

	scope
		'' Pre-#defines are simply inserted at the top of the token
		'' buffer, so that cppMain() parses them like any other #define.

		var i = api.script->head
		while i

			assert(i->class = ASTCLASS_OPTION)
			select case i->opt
			case OPT_DEFINE
				var s = "#define " + *i->text
				if i->alias then
					s += " " + *i->alias
				end if
				s += !"\n"

				var x = tkGetCount()
				lexLoadC(x, s, sourceinfoForZstring("pre-#define"))
				tkSetRemove(x, tkGetCount() - 1)

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

sub frogPrint(byref s as string)
	print frog.prefix + s
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	if  __FB_ARGC__ <= 1 then
		hPrintHelpAndExit()
	end if

	filebuffersInit()
	fbcrtheadersInit()
	extradatatypesInit()
	fbkeywordsInit()
	lexInit()

	hashInit(@frog.ucasebihash, 6, TRUE)
	hashInit(@frog.bilookupcache, 6, TRUE)

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

	if frog.nodefaultscript = FALSE then
		'' Parse default.fbfrog and prepend the options from it to the
		'' script from the command line.
		var userscript = frog.script
		frog.script = astNewGROUP()
		tkInit()
		hLoadArgsFile(0, hFindResource("default.fbfrog"), type(NULL, 0))
		hParseArgs(0)
		tkEnd()
		astAppend(frog.script, userscript)
	end if

	'' Parse the version-specific options ("script"), following each
	'' possible code path, and determine how many and which versions there
	'' are.
	frog.completeverors = astNewGROUP()
	frogEvaluateScript(frog.script->head, astNewGROUP(), astNewGROUP())
	assert(frog.apicount > 0)

	frog.prefix = space((len(str(frog.apicount)) * 2) + 4)

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
		print hMakeProgressString(api + 1, frog.apicount) + " " + astDumpPrettyVersion(frog.apis[api].verand)

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

		for bi as integer = 0 to frog.bicount - 1
			with frog.bis[bi]
				'' Do file-specific AST work (e.g. add Extern block)
				hlFile(.incoming, frog.apis[api], .options)

				'' Merge the "incoming" tree into the "final" tree
				astMergeNext(astNewVEROR(astClone(frog.apis[api].verand)), .final, .incoming)

				assert(.incoming = NULL)
				assert(.options.inclibs = NULL)
				assert(.options.undefs = NULL)
				assert(.options.addincludes = NULL)
			end with
		next

		frog.fullveror = astNewVEROR(frog.fullveror, frog.apis[api].verand)
		frog.apis[api].verand = NULL
	next

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
			emitFile(bifilename, header, .final)
		end with
	next

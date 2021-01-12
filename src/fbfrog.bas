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

#include once "ast-merge.bi"
#include once "c-lex.bi"
#include once "c-parser.bi"
#include once "c-pp.bi"
#include once "emit.bi"
#include once "fbfrog-args-lex.bi"
#include once "util-path.bi"

#include once "file.bi"

using tktokens

namespace frog
	dim shared as integer verbose
	dim shared as string outname, defaultoutname
	dim shared header as HeaderInfo  '' global titles etc. - will be added to all generated .bi files

	dim shared as integer have_declareversions

	dim shared os(0 to OS__COUNT-1) as byte
	dim shared arch(0 to ARCH__COUNT-1) as byte
	dim shared as integer enabledoscount

	dim shared as AstNode ptr script
	dim shared as AstNode ptr completeverors, fullveror
	dim shared as ApiInfo ptr apis
	dim shared as integer apicount
	dim shared as ApiBits fullapis

	dim shared as AstNode ptr mergedlog

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

	dim shared sourcectx as SourceContext
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

function frogLookupBiFromBi(byref filename as string) as integer
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
	print "fbfrog 1.14 (built on " + __DATE_ISO__ + ")"
	print "Usage: fbfrog foo.h [options]"
	print "Common options:"
	print "  -o <path/file>     Set output directory and/or file name"
	print "  -v                 Show verbose/debugging info; pass -v -v for more"
	print "  -define id [body]  Add a pre-#define for C preprocessor"
	print "  -incdir <path>     Add #include search directory for C preprocessor"
	print "  foo.fbfrog         Read more options from a file named foo.fbfrog"
	print "Please check the documentation for the full list of options."
	end 1
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
		byref tk as TokenBuffer, _
		byval x as integer, _
		byref filename as string, _
		byval location as TkLocation _
	)

	const MAX_FILES = 1024  '' Arbitrary limit to detect recursion
	static filecount as integer

	if filecount > MAX_FILES then
		tk.showErrorAndAbort(x, "suspiciously many @file expansions, recursion? (limit=" & MAX_FILES & ")")
	end if

	filename = hFindResource(filename)

	'' Load the file content at the specified position
	lexLoadArgs(frog.sourcectx, tk, x, frog.sourcectx.addFileSource(filename, location))
	filecount += 1

end sub

'' Expand @file arguments in the tk buffer
private sub hExpandArgsFiles(byref tk as TokenBuffer)
	var x = 0
	do
		select case tk.get(x)
		case TK_EOF
			exit do

		case TK_ARGSFILE
			var filename = *tk.getText(x)

			'' Complain if argument was only '@'
			if len(filename) = 0 then
				tk.oopsExpected(x, "file name directly behind @ (no spaces in between)")
			end if

			'' If the @file argument comes from an @file,
			'' open it relative to the parent @file's dir.
			var location = tk.getLocation(x)
			var decodedloc = frog.sourcectx.decode(location)
			if decodedloc.source->is_file then
				filename = pathAddDiv(pathOnly(*decodedloc.source->name)) + filename
			end if

			'' Load the file content behind the @file token
			hLoadArgsFile(tk, x + 1, filename, location)

			'' Remove the @file token (now that its location is no
			'' longer referenced), so it doesn't get in the way of
			'' hParseArgs().
			tk.remove(x, x)

			'' Re-check this position in case a new @file token was inserted right here
			x -= 1
		end select

		x += 1
	loop
end sub

private sub hExpectId(byref tk as TokenBuffer, byval x as integer)
	tk.expect(x, TK_ID, "(valid symbol name)")
end sub

private function hIsStringOrId(byref tk as TokenBuffer, byval x as integer) as integer
	function = (tk.get(x) = TK_STRING) or (tk.get(x) = TK_ID)
end function

private sub hExpectStringOrId(byref tk as TokenBuffer, byval x as integer, byval paramdescription as zstring ptr)
	if hIsStringOrId(tk, x) = FALSE then
		tk.oopsExpected(x, paramdescription)
	end if
end sub

private function hPathRelativeToArgsFile(byref tk as TokenBuffer, byval x as integer) as string
	var path = *tk.getText(x)

	'' If the file/dir argument isn't an absolute path, and it came from an
	'' @file, open it relative to the @file's dir.
	if pathIsAbsolute(path) = FALSE then
		var decodedloc = frog.sourcectx.decode(tk.getLocation(x))
		if decodedloc.source->is_file then
			path = pathAddDiv(pathOnly(*decodedloc.source->name)) + path
		end if
	end if

	function = path
end function

declare sub hParseArgs(byref tk as TokenBuffer, byref x as integer)

private sub hParseSelectCompound(byref tk as TokenBuffer, byref x as integer, byval selectkind as integer)
	'' -selecttarget|-selectversion|-selectdefine
	var xblockbegin = x
	x += 1

	frog.script->append(astNew(selectkind))

	'' -case
	if tk.get(x) <> OPT_CASE then
		tk.oopsExpected(x, "-case after the -select")
	end if

	do
		hParseArgs(tk, x)

		select case tk.get(x)
		case TK_EOF
			tk.showErrorAndAbort(xblockbegin, "missing -endselect for this")

		case OPT_CASE
			if frog.script->tail->kind = ASTKIND_CASEELSE then
				tk.showErrorAndAbort(x, "-case behind -caseelse")
			end if
			xblockbegin = x
			x += 1

			select case selectkind
			case ASTKIND_SELECTVERSION : hExpectStringOrId(tk, x, "<version number> argument")
			case ASTKIND_SELECTTARGET  : hExpectStringOrId(tk, x, "<target> argument")
			case else                  : hExpectId(tk, x)
			end select
			var n = astNew(ASTKIND_CASE, tk.getText(x))
			n->location = tk.getLocation(x)
			frog.script->append(n)
			x += 1

		case OPT_CASEELSE
			if frog.script->tail->kind = ASTKIND_CASEELSE then
				tk.showErrorAndAbort(x, "-caseelse behind -caseelse")
			end if
			frog.script->append(astNew(ASTKIND_CASEELSE))
			xblockbegin = x
			x += 1

		case OPT_ENDSELECT
			frog.script->append(astNew(ASTKIND_ENDSELECT))
			x += 1
			exit do

		case else
			tk.oopsExpected(x, "-case or -endselect")
		end select
	loop
end sub

private sub hParseIfCompound(byref tk as TokenBuffer, byref x as integer)
	'' -ifdef|-iftarget
	var xblockbegin = x
	var is_target = (tk.get(x) = OPT_IFTARGET)
	x += 1

	if is_target then
		'' <target>
		hExpectStringOrId(tk, x, "<target> argument")

		'' -iftarget <target>  =>  -selecttarget -case <target>
		frog.script->append(astNew(ASTKIND_SELECTTARGET))
		frog.script->append(astNew(ASTKIND_CASE, tk.getText(x)))
	else
		'' <symbol>
		hExpectId(tk, x)

		'' -ifdef <symbol>  =>  -select -case <symbol>
		frog.script->append(astNew(ASTKIND_SELECTDEFINE))
		frog.script->append(astNew(ASTKIND_CASE, tk.getText(x)))
	end if
	x += 1

	do
		hParseArgs(tk, x)

		select case tk.get(x)
		case TK_EOF
			tk.showErrorAndAbort(xblockbegin, "missing -endif for this")

		case OPT_ELSE
			if frog.script->tail->kind = ASTKIND_CASEELSE then
				tk.showErrorAndAbort(x, "-else behind -else")
			end if
			frog.script->append(astNew(ASTKIND_CASEELSE))
			xblockbegin = x
			x += 1

		case OPT_ENDIF
			frog.script->append(astNew(ASTKIND_ENDSELECT))
			x += 1
			exit do

		case else
			tk.oopsExpected(x, iif(tk.get(xblockbegin) = OPT_ELSE, _
					@"-endif", @"-else or -endif"))
		end select
	loop
end sub

private sub hParseDestinationBiFile(byref tk as TokenBuffer, byref x as integer)
	'' [<destination .bi file>]
	if hIsStringOrId(tk, x) then
		assert(frog.script->tail->kind = ASTKIND_OPTION)
		frog.script->tail->setAlias(tk.getText(x))
		x += 1
	end if
end sub

private sub hParseParam(byref tk as TokenBuffer, byref x as integer, byref description as zstring)
	hExpectStringOrId(tk, x, description)
	x += 1
end sub

private sub hParseOption1Param(byref tk as TokenBuffer, byref x as integer, byval opt as integer, byref param1 as zstring)
	x += 1
	hParseParam(tk, x, param1 + " parameter for " + tkInfoPretty(opt))
	frog.script->append(astNewOPTION(opt, tk.getText(x - 1)))
end sub

private sub hParseOption2Params(byref tk as TokenBuffer, byref x as integer, byval opt as integer, byref param1 as zstring, byref param2 as zstring)
	x += 1
	hParseParam(tk, x, param1)
	hParseParam(tk, x, param2)
	frog.script->append(astNewOPTION(opt, tk.getText(x - 2), tk.getText(x - 1)))
end sub

private sub hParseArgs(byref tk as TokenBuffer, byref x as integer)
	static nestinglevel as integer
	static seentarget as integer

	nestinglevel += 1

	while tk.get(x) <> TK_EOF
		var opt = tk.get(x)
		select case as const opt
		case OPT_V
			frog.verbose += 1
			x += 1

		case OPT_O
			x += 1
			hExpectStringOrId(tk, x, "<path/file>")
			frog.outname = hPathRelativeToArgsFile(tk, x)
			x += 1

		'' -emit <filename-pattern> <file>
		case OPT_EMIT
			x += 1

			hExpectStringOrId(tk, x, "<filename-pattern> argument")
			var pattern = *tk.getText(x)
			x += 1

			hExpectStringOrId(tk, x, "<file> argument")
			var filename = *tk.getText(x)
			x += 1

			frogAddBi(filename, pattern)

		'' -dontemit <filename-pattern>
		case OPT_DONTEMIT
			x += 1

			hExpectStringOrId(tk, x, "<filename-pattern> argument")
			frogAddPattern(*tk.getText(x), -1)
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

			hExpectStringOrId(tk, x, "<target> argument")
			var s = *tk.getText(x)
			select case s
			case "nodos"
				frogSetTargets(TRUE)
				frog.os(OS_DOS) = FALSE
			case "noarm"
				frogSetTargets(TRUE)
				frog.arch(ARCH_ARM) = FALSE
				frog.arch(ARCH_AARCH64) = FALSE
			case "win32"
				frog.os(OS_WINDOWS) = TRUE
				frog.arch(ARCH_X86) = TRUE
			case "win64"
				frog.os(OS_WINDOWS) = TRUE
				frog.arch(ARCH_X86_64) = TRUE
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
						tk.showErrorAndAbort(x, "unknown -target argument")
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
			hParseParam(tk, x, "<text> parameter for -title")
			var title = tk.getText(x - 1)

			'' original-license.txt
			hParseParam(tk, x, "original-license.txt parameter for -title")
			var licensefile = frog.sourcectx.addFileSource(tk.getText(x - 1), tk.getLocation(x - 1))

			'' translators.txt
			hParseParam(tk, x, "translators.txt parameter for -title")
			var translatorsfile = frog.sourcectx.addFileSource(tk.getText(x - 1), tk.getLocation(x - 1))

			'' [<destination .bi file>]
			dim header as HeaderInfo ptr
			if hIsStringOrId(tk, x) then
				'' .bi-specific -title option
				var bi = frogLookupBiFromBi(*tk.getText(x))
				if bi < 0 then
					tk.showErrorAndAbort(x, "unknown destination .bi")
				end if
				header = @frog.bis[bi].header
				x += 1
			else
				'' global -title option
				header = @frog.header
			end if

			if len(header->title) > 0 then
				tk.showErrorAndAbort(begin, "duplicate -title option")
			end if
			header->title = *title
			header->licensefile = licensefile
			header->translatorsfile = translatorsfile

		'' -declareversions <symbol> (<string>)+
		case OPT_DECLAREVERSIONS
			if frog.have_declareversions then
				tk.showErrorAndAbort(x, "multiple -declareversions options, only 1 is allowed")
			end if
			frog.have_declareversions = TRUE
			x += 1

			'' <symbol>
			hExpectId(tk, x)
			frog.versiondefine = *tk.getText(x)
			x += 1

			'' The version numbers must be given in ascending order,
			'' allowing us to optimize things like:
			''      (ID = a) or (ID = b) or (ID = c)
			'' to:
			''      ID <= c

			'' (<string>)+
			if tk.get(x) <> TK_STRING then
				tk.oopsExpected(x, "<version number> argument")
			end if
			var xfirst = x
			do
				var verstr = *tk.getText(x)

				'' Verify that new version number is >= the previous one (unless this is the first one)
				if xfirst < x then
					var prev = *tk.getText(x - 1)
					if valint(prev) >= valint(verstr) then
						tk.showErrorAndAbort(x, "version '" + prev + "' >= '" + verstr + "', but should be < to maintain order")
					end if
				end if

				frogAddVernum(verstr)

				x += 1
			loop while tk.get(x) = TK_STRING

			frog.script->append(astNew(ASTKIND_DECLAREVERSIONS))

		'' -declarebool <symbol>
		case OPT_DECLAREBOOL
			x += 1

			'' <symbol>
			hExpectId(tk, x)
			frog.script->append(astNew(ASTKIND_DECLAREBOOL, tk.getText(x)))
			x += 1

		case OPT_SELECTTARGET  : hParseSelectCompound(tk, x, ASTKIND_SELECTTARGET)
		case OPT_SELECTVERSION : hParseSelectCompound(tk, x, ASTKIND_SELECTVERSION)
		case OPT_SELECTDEFINE  : hParseSelectCompound(tk, x, ASTKIND_SELECTDEFINE)

		case OPT_IFTARGET, OPT_IFDEF
			hParseIfCompound(tk, x)

		case OPT_CASE, OPT_CASEELSE, OPT_ENDSELECT, OPT_ELSE, OPT_ENDIF
			if nestinglevel <= 1 then
				select case tk.get(x)
				case OPT_CASE      : tk.showErrorAndAbort(x, "-case without -select")
				case OPT_CASEELSE  : tk.showErrorAndAbort(x, "-caseelse without -select")
				case OPT_ENDSELECT : tk.showErrorAndAbort(x, "-endselect without -select")
				case OPT_ELSE      : tk.showErrorAndAbort(x, "-else without -ifdef")
				case else          : tk.showErrorAndAbort(x, "-endif without -ifdef")
				end select
			end if
			exit while

		'' -define <id> [<body>]
		case OPT_DEFINE
			x += 1

			hExpectId(tk, x)
			var id = tk.getText(x)
			x += 1

			dim body as zstring ptr
			if hIsStringOrId(tk, x) then
				body = tk.getText(x)
				x += 1
			end if

			frog.script->append(astNewOPTION(opt, id, body))

		case OPT_INCLUDE
			hParseOption1Param(tk, x, opt, "<file>")

		case OPT_FBFROGINCLUDE
			hParseOption1Param(tk, x, opt, "<file>")

		case OPT_INCDIR
			x += 1
			hExpectStringOrId(tk, x, "<path>")
			frog.script->append(astNewOPTION(opt, hPathRelativeToArgsFile(tk, x)))
			x += 1

		case OPT_WINDOWSMS, OPT_CLONG32, OPT_FIXUNSIZEDARRAYS, _
		     OPT_NOFUNCTIONBODIES, OPT_DROPMACROBODYSCOPES, OPT_REMOVEEMPTYRESERVEDDEFINES
			x += 1
			frog.script->append(astNewOPTION(opt))

		case OPT_RENAMETYPEDEF, OPT_RENAMETAG, OPT_RENAMEPROC, OPT_RENAMEDEFINE, _
		     OPT_RENAMEMACROPARAM, OPT_RENAME
			hParseOption2Params(tk, x, opt, "<oldid>", "<newid>")

		case OPT_RENAME_, OPT_REMOVE, OPT_REMOVEDEFINE, OPT_REMOVEPROC, OPT_REMOVEVAR, OPT_REMOVE1ST, OPT_REMOVE2ND, _
		     OPT_DROPPROCBODY, OPT_TYPEDEFHINT, OPT_ADDFORWARDDECL, OPT_UNDEFBEFOREDECL, OPT_IFNDEFDECL, _
		     OPT_CONVBODYTOKENS, OPT_FORCEFUNCTION2MACRO, OPT_EXPANDINDEFINE, OPT_NOEXPAND
			hParseOption1Param(tk, x, opt, "<id>")

		case OPT_EXPAND
			hParseOption1Param(tk, x, opt, "<id-pattern>")

		case OPT_NOSTRING, OPT_STRING
			hParseOption1Param(tk, x, opt, "<decl-pattern>")

		case OPT_REMOVEINCLUDE
			hParseOption1Param(tk, x, opt, "<filename>")

		case OPT_SETARRAYSIZE
			hParseOption2Params(tk, x, opt, "<id>", "<size>")

		case OPT_MOVEABOVE
			hParseOption2Params(tk, x, opt, "<id>", "<ref>")

		case OPT_REPLACEMENTS
			hParseOption1Param(tk, x, opt, "<file>")

		case OPT_INCLIB
			hParseOption1Param(tk, x, opt, "<name>")
			hParseDestinationBiFile(tk, x)

		case OPT_UNDEF
			hParseOption1Param(tk, x, opt, "<id>")
			hParseDestinationBiFile(tk, x)

		case OPT_ADDINCLUDE
			hParseOption1Param(tk, x, opt, "<.bi file>")
			hParseDestinationBiFile(tk, x)

		case else
			'' *.fbfrog file given (without @)? Treat as @file too
			var filename = *tk.getText(x)
			if pathExtOnly(filename) = "fbfrog" then
				hLoadArgsFile(tk, x + 1, filename, tk.getLocation(x))
				tk.remove(x, x)

				'' Must expand @files again in case the loaded file contained any
				hExpandArgsFiles(tk)
			else
				'' Input file
				var filename = hPathRelativeToArgsFile(tk, x)
				frog.script->append(astNewOPTION(OPT_I, filename))

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

private function hSkipToEndOfBlock(byval i as AstNode ptr) as AstNode ptr
	var level = 0

	do
		select case i->kind
		case ASTKIND_SELECTTARGET, ASTKIND_SELECTVERSION, ASTKIND_SELECTDEFINE
			level += 1

		case ASTKIND_CASE, ASTKIND_CASEELSE
			if level = 0 then
				exit do
			end if

		case ASTKIND_ENDSELECT
			if level = 0 then
				exit do
			end if
			level -= 1
		end select

		i = i->nxt
	loop

	function = i
end function

private sub frogAddApi(byval verand as AstNode ptr, byval script as AstNode ptr, byval target as TargetInfo)
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
		byval start as AstNode ptr, _
		byval conditions as AstNode ptr, _
		byval options as AstNode ptr, _
		byval target as TargetInfo _
	)

	var i = start
	while i

		select case i->kind
		case ASTKIND_DECLAREVERSIONS
			i = i->nxt

			var lastvernum = ubound(frog.vernums)

			'' Evaluate a separate code path for each version,
			'' branching for every version except the last
			for vernum as integer = 0 to lastvernum - 1
				'' <symbol> = <versionnumber>
				frogEvaluateScript(i, _
					astNewGROUP(conditions->clone(), astNewVERNUMCHECK(vernum)), _
					options->clone(), _
					target)
			next

			'' Now continue without recursing and evaluate the code path for the last version
			conditions->append(astNewVERNUMCHECK(lastvernum))

		case ASTKIND_DECLAREBOOL
			var symbol = i->text
			i = i->nxt

			var completeveror = astNew(ASTKIND_VEROR)

			'' Branch for the true code path
			'' defined(<symbol>)
			var condition = astNewDEFINED(symbol)
			completeveror->append(condition->clone())
			frogEvaluateScript(i, _
				astNewGROUP(conditions->clone(), condition->clone()), _
				options->clone(), _
				target)

			'' And follow the false code path here
			'' (not defined(<symbol>))
			condition = astNew(ASTKIND_NOT, condition)
			completeveror->append(condition->clone())
			conditions->append(condition)

			frog.completeverors->append(completeveror)

		case ASTKIND_SELECTTARGET, ASTKIND_SELECTVERSION, ASTKIND_SELECTDEFINE
			var sel = i
			i = i->nxt

			do
				'' -case
				assert(i->kind = ASTKIND_CASE)

				'' Evaluate -case condition
				dim is_true as integer

				if sel->kind = ASTKIND_SELECTTARGET then
					is_true = hTargetPatternMatchesTarget(*i->text, target)
				else
					dim condition as AstNode ptr
					if sel->kind = ASTKIND_SELECTVERSION then
						'' <versionnumber>
						var vernum = frogLookupVernum(*i->text)
						if vernum < 0 then
							oopsLocation(frog.sourcectx.decode(i->location), "unknown version number; it didn't appear in the previous -declareversions option")
						end if
						condition = astNewVERNUMCHECK(vernum)
					else
						'' defined(<symbol>)
						condition = astNewDEFINED(i->text)
					end if
					is_true = conditions->groupContains(condition)
					delete condition
				end if

				i = i->nxt

				'' Evaluate the first -case block whose condition is true
				if is_true then
					exit do
				end if

				'' Condition was false, skip over the -case's body
				var eob = hSkipToEndOfBlock(i)
				select case eob->kind
				case ASTKIND_CASEELSE, ASTKIND_ENDSELECT
					'' Reached -caseelse/-endselect
					i = eob->nxt
					exit do
				end select

				'' Go to next -case
				i = eob
			loop

		case ASTKIND_CASE, ASTKIND_CASEELSE
			'' When reaching a case/else block instead of the corresponding
			'' select, that means we're evaluating the code path of the
			'' previous case code path, and must now step over the
			'' block(s) of the alternate code path(s).
			i = hSkipToEndOfBlock(i->nxt)
			assert((i->kind = ASTKIND_CASE) or _
				(i->kind = ASTKIND_CASEELSE) or _
				(i->kind = ASTKIND_ENDSELECT))

		case ASTKIND_ENDSELECT
			'' Ignore - nothing to do
			i = i->nxt

		case else
			options->append(i->clone())
			i = i->nxt
		end select
	wend

	assert(conditions->kind = ASTKIND_GROUP)
	conditions->kind = ASTKIND_VERAND
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

private function frogParse(byref api as ApiInfo) as AstNode ptr
	dim ast as AstNode ptr

	scope
		dim tk as TokenBuffer = TokenBuffer(@frog.sourcectx)

		scope
			'' C preprocessing
			dim cpp as CppContext = CppContext(@frog.sourcectx, @tk, api)

			'' Add fbfrog's CPP pre-#defines for preprocessing of default.h
			cpp.addTargetPredefines(api.target)

			scope
				'' Pre-#defines are simply inserted at the top of the token
				'' buffer, so that cppMain() parses them like any other #define.

				var i = api.script->head
				while i

					assert(i->kind = ASTKIND_OPTION)
					select case i->opt
					case OPT_DEFINE
						cpp.addPredefine(i->text, i->alias_)
					case OPT_INCDIR
						cpp.addIncDir(i->text)
					end select

					i = i->nxt
				wend
			end scope

			'' Insert the code from fbfrog pre-#includes (default.h etc.)
			'' * behind command line pre-#defines so that default.h can use them
			'' * marked for removal so the code won't be preserved
			scope
				var i = api.script->head
				while i
					assert(i->kind = ASTKIND_OPTION)
					if i->opt = OPT_FBFROGINCLUDE then
						var filename = hFindResource(*i->text)
						var x = tk.count()
						lexLoadC(frog.sourcectx, tk, x, frog.sourcectx.addFileSource(filename, i->location))
						tk.setRemove(x, tk.count() - 1)
					end if
					i = i->nxt
				wend
			end scope

			'' Add #includes for pre-#includes
			scope
				var i = api.script->head
				while i
					assert(i->kind = ASTKIND_OPTION)
					if i->opt = OPT_INCLUDE then
						cpp.appendIncludeDirective(i->text, TKFLAG_PREINCLUDE)
					end if
					i = i->nxt
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
					assert(i->kind = ASTKIND_OPTION)
					if i->opt = OPT_I then
						cpp.appendIncludeDirective(i->text, TKFLAG_ROOTFILE)
					end if
					i = i->nxt
				wend
			end scope

			cpp.parseToplevel()
		end scope

		'' Remove CPP directives and EOLs (tokens marked for removal by
		'' cppMain()). Doing this as a separate step allows
		'' * error reports during cppMain() to view the complete input
		'' * cppMain() to reference #define directives based on token position
		''   (to retrieve the bodies for macro expansion) as opposed to having
		''   to load them into AST
		tk.applyRemoves()

		hMoveDirectivesOutOfConstructs(tk)

		if api.replacementcount > 0 then
			hApplyReplacements(frog.sourcectx, tk, api)
		end if

		tk.turnCPPTokensIntoCIds()

		'' C parsing
		scope
			dim parser as CParser = CParser(tk, api)
			ast = parser.parseToplevel()
		end scope
	end scope

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

	scope
		dim tk as TokenBuffer = TokenBuffer(@frog.sourcectx)

		'' Load all command line arguments into the tk buffer
		lexLoadArgs(frog.sourcectx, tk, 0, frog.sourcectx.addInternalSource("command line", hTurnArgsIntoString(__FB_ARGC__, __FB_ARGV__)))

		'' Load content of @files too
		hExpandArgsFiles(tk)

		'' Parse the command line arguments, skipping argv[0]. Global options
		'' are added to various frog.* fields, version-specific options are
		'' added to the frog.script list in their original order.
		frog.script = astNewGROUP()
		hParseArgs(tk, 1)
	end scope

	'' Add the implicit default.h pre-#include
	frog.script->prepend(astNewOPTION(OPT_FBFROGINCLUDE, "default.h"))

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
					osveror->append(astNewDEFINEDfbos(os))
				end if
			next
			frog.completeverors->append(osveror)
		end if

		'' 2. one VEROR covering the __FB_64BIT__ boolean (defined + not defined)
		'' This assumes that 32bit + 64bit architectures are always enabled.
		'' TODO: support cases like 32bit-only?
		frog.completeverors->append( _
			astNewVEROR(astNewDEFINEDfb64(FALSE), _
			            astNewDEFINEDfb64(TRUE)))

		'' same for __FB_ARM__
		frog.completeverors->append( _
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
	dim as AstNode ptr final
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
			dim prevsource as const SourceInfo ptr

			var i = ast->head
			while i
				var nxt = i->nxt

				var decodedloc = frog.sourcectx.decode(i->location)
				assert(decodedloc.source)
				assert(decodedloc.source->is_file)

				'' Find out into which .bi file this declaration should be put into.
				'' If this declaration has the same source as the previous one,
				'' then re-use the previously calculated .bi file, instead of
				'' redoing the lookup. Otherwise, do the lookup and cache the result.
				if prevsource <> decodedloc.source then
					bi = frogLookupBiFromH(decodedloc.source->name)
					prevsource = decodedloc.source
				end if

				if bi >= 0 then
					'' Add the declaration to the "incoming" AST for that .bi file
					ast->unlink(i)
					frog.bis[bi].incoming->append(i)
				end if

				i = nxt
			wend
		end scope

		'' Forget the remaining AST. Anything that wasn't moved into the
		'' .bi files won't be emitted.
		delete ast

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

		frog.fullveror = astNewVEROR(frog.fullveror, frog.apis[api].verand->clone())
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
				.final->prepend(astNew(ASTKIND_PRAGMAONCE))
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

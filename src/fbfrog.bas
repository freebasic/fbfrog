''
'' Main module, command line interface
''

#include once "fbfrog.bi"

#include once "clang-parser.bi"
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

	'' Load the file content at the specified position
	var file = filebuffersAdd(frog.sourcectx, filename, location)
	lexLoadArgs(tk, x, file->buffer, file->source)
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
			if location.source->is_file then
				filename = pathAddDiv(pathOnly(location.source->name)) + filename
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
		var location = tk.getLocation(x)
		if location.source->is_file then
			path = pathAddDiv(pathOnly(location.source->name)) + path
		end if
	end if

	function = path
end function

declare sub hParseArgs(byref tk as TokenBuffer, byref x as integer)

private sub hParseParam(byref tk as TokenBuffer, byref x as integer, byref description as zstring)
	hExpectStringOrId(tk, x, description)
	x += 1
end sub

private sub hParseOption1Param(byref tk as TokenBuffer, byref x as integer, byval opt as integer, byref param1 as zstring)
	x += 1
	hParseParam(tk, x, param1 + " parameter for " + tkInfoPretty(opt))
	astAppend(frog.script, astNewOPTION(opt, tk.getText(x - 1)))
end sub

private sub hParseOption2Params(byref tk as TokenBuffer, byref x as integer, byval opt as integer, byref param1 as zstring, byref param2 as zstring)
	x += 1
	hParseParam(tk, x, param1)
	hParseParam(tk, x, param2)
	astAppend(frog.script, astNewOPTION(opt, tk.getText(x - 2), tk.getText(x - 1)))
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

			astAppend(frog.script, astNewOPTION(opt, id, body))

		case OPT_INCLUDE
			hParseOption1Param(tk, x, opt, "<file>")

		case OPT_INCDIR
			x += 1
			hExpectStringOrId(tk, x, "<path>")
			astAppend(frog.script, astNewOPTION(opt, hPathRelativeToArgsFile(tk, x)))
			x += 1

		case OPT_WINDOWSMS, OPT_CLONG32, OPT_FIXUNSIZEDARRAYS, _
		     OPT_NOFUNCTIONBODIES, OPT_DROPMACROBODYSCOPES, OPT_REMOVEEMPTYRESERVEDDEFINES
			x += 1
			astAppend(frog.script, astNewOPTION(opt))

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

private sub frogAddApi(byval script as AstNode ptr, byval target as TargetInfo)
	var i = frog.apicount
	frog.apicount += 1
	frog.apis = reallocate(frog.apis, frog.apicount * sizeof(*frog.apis))
	with frog.apis[i]
		.constructor()
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

private sub maybeEvalForTarget(byval os as integer, byval arch as integer)
	if frog.arch(arch) then
		frogAddApi(frog.script->head, type<TargetInfo>(os, arch))
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

private function getTargetClangInvokeCommand(byref api as ApiInfo) as string
	return "clang -target " + api.target.clang()
end function

private sub parseIncludeDirsFromGccVerbose(byref gccverbose as const string, byref includedirs as DynamicArray(string))
	enum
		StateNotYet
		StatePart1
		StatePart2
		StateFinished
	end enum
	var state = StateNotYet
	var bol = 0

	while bol < len(gccverbose)
		var eol = instr(bol + 1, gccverbose, !"\n") - 1
		var ln = mid(gccverbose, bol + 1, eol - bol)
		bol = eol + 1

		select case ln
		case "#include ""..."" search starts here:"
			if state = StateNotYet then
				state = StatePart1
			end if

		case "#include <...> search starts here:"
			if state = StatePart1 then
				state = StatePart2
			end if

		case "End of search list."
			state = StateFinished

		case else
			select case state
			case StatePart1, StatePart2
				if len(ln) > 1 andalso ln[0] = asc(" ") then
					ln = right(ln, len(ln) - 1)
					includedirs.append(ln)
				end if
			end select

		end select
	wend
end sub

private function getCommandOutput(byref cmd as const string) as string
	var f = freefile()
	if open pipe(cmd, for input, as f) then
		oops("Failed to run command: " + cmd)
	end if
	dim as string ln, all
	while not eof(f)
		line input #f, ln
		all += ln + !"\n"
	wend
	close f
	return all
end function

private sub queryGccIncludeDirs(byref api as ApiInfo, byref includedirs as DynamicArray(string))
	var cmd = "echo | " + getTargetClangInvokeCommand(api) + " -E -v - 2>&1"
	var gccverbose = getCommandOutput(cmd)
	parseIncludeDirsFromGccVerbose(gccverbose, includedirs)
end sub

private function frogParse(byref api as ApiInfo) as AstNode ptr
	dim ast as AstNode ptr

#if 0
	scope
		dim tk as TokenBuffer

		scope
			'' C preprocessing
			dim cpp as CppContext = CppContext(frog.sourcectx, tk, api)

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
#endif

	scope
		dim parser as ClangContext = ClangContext(frog.sourcectx, api)

		parser.addArg("-target")
		parser.addArg(api.target.clang())

		scope
			dim includedirs as DynamicArray(string)
			queryGccIncludeDirs(api, includedirs)
			for i as integer = 0 to includedirs.count - 1
				parser.addArg("-isystem")
				parser.addArg(includedirs.p[i])
			next
		end scope

		'' Pre-#defines and #include search dirs
		scope
			var i = api.script->head
			while i
				assert(i->kind = ASTKIND_OPTION)
				select case i->opt
				case OPT_DEFINE
					parser.addArg("-D" & *i->text & "=" & *i->alias_)
				case OPT_INCDIR
					parser.addArg("-I" & *i->text)
				end select
				i = i->nxt
			wend
		end scope

		'' Pre-#includes
		scope
			var i = api.script->head
			while i
				assert(i->kind = ASTKIND_OPTION)
				if i->opt = OPT_INCLUDE then
					parser.addArg("-include")
					parser.addArg(*i->text)
				end if
				i = i->nxt
			wend
		end scope

		'' Main input files
		scope
			var i = api.script->head
			while i
				assert(i->kind = ASTKIND_OPTION)
				if i->opt = OPT_I then
					parser.addArg(*i->text)
				end if
				i = i->nxt
			wend
		end scope

		if frog.verbose then
			parser.dumpArgs()
		end if

		parser.parseTranslationUnit()

		ast = parser.parseAst()

		parser.parseInclusions()
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
		dim tk as TokenBuffer

		'' Load all command line arguments into the tk buffer
		lexLoadArgs(tk, 0, hTurnArgsIntoString(__FB_ARGC__, __FB_ARGV__), _
			frog.sourcectx.lookupOrMakeSourceInfo("<command line>", FALSE))

		'' Load content of @files too
		hExpandArgsFiles(tk)

		'' Parse the command line arguments, skipping argv[0]. Global options
		'' are added to various frog.* fields, version-specific options are
		'' added to the frog.script list in their original order.
		frog.script = astNewGROUP()
		hParseArgs(tk, 1)
	end scope

	'' Determine the APIs and their individual options
	'' - The API-specific command line options were stored in the "script",
	''   this must be evaluated now, following each possible code path.
	'' - The built-in targets representing the "base" set of APIs are
	''   hard-coded here in form of loops
	'' - Any -declare* options in the script trigger recursive evaluation
	frog.enabledoscount = 0
	for os as integer = 0 to OS__COUNT - 1
		if frog.os(os) then frog.enabledoscount += 1
	next
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
				hlFile(.incoming, frog.apis[api])

				'' Merge the "incoming" tree into the "final" tree
				assert(.final = NULL)
				.final = .incoming
				.incoming = NULL
			end with
		next

		assert(frog.mergedlog = NULL)
		frog.mergedlog = frog.apis[api].log
		frog.apis[api].log = NULL
	next

	'' Print the merged list of #included files
	'' This should be useful because it allows the user to see which input
	'' files were used, found, not found, which additional files were
	'' #included, etc.
	emitFbStdout(frog.mergedlog, 1)

	for bi as integer = 0 to frog.bicount - 1
		with frog.bis[bi]
			'' Prepend #pragma once
			'' It's always needed, except if the binding is empty: C headers
			'' typically have #include guards, but we don't preserve those.
			if .final->head then
				astPrepend(.final, astNew(ASTKIND_PRAGMAONCE))
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

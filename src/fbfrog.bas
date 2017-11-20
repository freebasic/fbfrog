''
'' Main module, command line interface
''

#include once "fbfrog.bi"

#include once "api.bi"
#include once "ast.bi"
#include once "clang-parser.bi"
#include once "c-lex.bi"
#include once "c-parser.bi"
#include once "c-pp.bi"
#include once "emit.bi"
#include once "options.bi"
#include once "fbfrog-args-lex.bi"
#include once "highlevel.bi"
#include once "util-path.bi"

#include once "file.bi"

using tktokens

namespace frog
	dim shared as integer verbose
	dim shared as string outname

	dim shared os(0 to OS__COUNT-1) as byte
	dim shared arch(0 to ARCH__COUNT-1) as byte

	dim shared as AstNode ptr script

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

				x += 1
			end if
		end select
	wend

	nestinglevel -= 1
end sub

private function getTargetClangInvokeCommand(byref options as BindingOptions) as string
	return "clang -target " + options.target.clang()
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

private sub queryGccIncludeDirs(byref options as BindingOptions, byref includedirs as DynamicArray(string))
	var cmd = "echo | " + getTargetClangInvokeCommand(options) + " -E -v - 2>&1"
	var gccverbose = getCommandOutput(cmd)
	parseIncludeDirsFromGccVerbose(gccverbose, includedirs)
end sub

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

	dim options as BindingOptions
	options.script = frog.script
	frog.script = NULL
	options.loadOptions()

	dim ast as AstNode ptr

	scope
		dim parser as ClangContext = ClangContext(frog.sourcectx, options)

		parser.addArg("-target")
		parser.addArg(options.target.clang())

		scope
			dim includedirs as DynamicArray(string)
			queryGccIncludeDirs(options, includedirs)
			for i as integer = 0 to includedirs.count - 1
				parser.addArg("-isystem")
				parser.addArg(includedirs.p[i])
			next
		end scope

		'' Pre-#defines and #include search dirs
		scope
			var i = options.script->head
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
			var i = options.script->head
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
			var i = options.script->head
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

	hlGlobal(ast, options)
	hlFile(ast, options)

	if frog.outname = "" then
		emitFbStdout(ast, 0)
	else
		print "emitting: " + frog.outname + " (" + _
			hMakeCountMessage(hlCountDecls(ast), "declaration") + ", " + _
			hMakeCountMessage(hlCountTodos(ast), "TODO"       ) + ")"
		emitFbFile(frog.outname, ast)
	end if

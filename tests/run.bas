''
'' This helper program searches the tests/ directory for *.h files, and runs
'' fbfrog on each one. Test cases which are intended to PASS will produce a .bi
'' file and a .txt file containing fbfrog's console output. Test cases which are
'' intended to FAIL only produce the .txt file.
''
'' This way we have the .h, the resulting .bi (if any), and any console output
'' including error messages captured and can easily detect changes via Git.
''
'' The first line of each .h file is checked for the following strings:
''
''  @fbfrog <...>  Extra fbfrog command line options for this test.
''                 Any occurences of "<dir>" are replaced with the path to the
''                 directory containing the .h file.
''
''  @fail        Used to mark error (expected-failure) tests. This can be used
''               in front of @fbfrog: @fail @fbfrog ...
''
''  @ignore      Skip this .h file completely, allowing it to be #included
''               into others etc. without being seen as test cases itself.
''

#include "../src/util-path.bi"
#include "../src/util-str.bi"
#include "dir.bi"

#if defined(__FB_WIN32__) or defined(__FB_CYGWIN__) or defined(__FB_DOS__)
	const EXEEXT = ".exe"
#else
	const EXEEXT = ""
#endif

type TestRunner
	as string exe_path, cur_dir, fbfrog
end type

dim shared runner as TestRunner

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function strStripPrefix(byref s as string, byref prefix as string) as string
	if left(s, len(prefix)) = prefix then
		function = right(s, len(s) - len(prefix))
	else
		function = s
	end if
end function

function pathStripLastComponent(byref path as string) as string
	function = pathOnly(left(path, len(path) - 1))
end function

function hExtractLine1(byref filename as string) as string
	var f = freefile()
	if open(filename, for input, as #f) then
		print "couldn't open file '" + filename + "'"
		end 1
	end if

	dim as string line1
	line input #f, line1

	close #f

	function = line1
end function

function hShell(byref ln as string) as integer
	var result = shell(ln)
	select case result
	case -1
		print "command not found: '" + ln + "'"
		end 1
	case 0, 1
	case else
		print "command terminated with unusual error code " & result & ": " + ln
		end 1
	end select
	function = (result = 0)
end function

sub hTest(byref hfile as string)
	var line1 = hExtractLine1(hfile)

	'' @ignore?
	if instr(line1, "@ignore") >= 1 then
		exit sub
	end if

	'' @fail?
	var is_failure_test = (instr(line1, "@fail") >= 1)

	'' @fbfrog <...> extra command line options?
	dim as string extraoptions
	scope
		const TOKEN = "@fbfrog "
		var begin = instr(line1, TOKEN)
		if begin >= 1 then
			begin += len(TOKEN)
			extraoptions = right(line1, len(line1) - begin + 1)
			extraoptions = strReplace(extraoptions, "<dir>", pathAddDiv(pathOnly(hfile)))
		end if
	end scope

	var txtfile = pathStripExt(hfile) + ".txt"

	'' ./fbfrog *.h <extraoptions> > txtfile 2>&1
	var ln = runner.fbfrog + " " + hfile + " " + extraoptions
	hShell(ln + " > " + txtfile + " 2>&1")

	print "expect " + iif(is_failure_test, "fail", "pass") + ": " + hfile
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' Directory tree search

const MAXFILES = 1 shl 10

namespace files
	dim shared as string list(0 to MAXFILES-1)
	dim shared as integer count
end namespace

type DIRNODE
	next		as DIRNODE ptr
	path		as string
end type

type DIRQUEUE
	head		as DIRNODE ptr
	tail		as DIRNODE ptr
end type

dim shared as DIRQUEUE dirs

private sub dirsAppend(byref path as string)
	dim as DIRNODE ptr node = callocate(sizeof(DIRNODE))
	node->path = pathAddDiv(path)
	if dirs.tail then
		dirs.tail->next = node
	end if
	dirs.tail = node
	if dirs.head = NULL then
		dirs.head = node
	end if
end sub

private sub dirsDropHead()
	if dirs.head then
		var node = dirs.head
		dirs.head = node->next
		if dirs.head = NULL then
			dirs.tail = NULL
		end if
		node->path = ""
		deallocate(node)
	end if
end sub

private sub hScanParent(byref parent as string, byref filepattern as string)
	'' Scan for files
	var found = dir(parent + filepattern, fbNormal)
	while len(found) > 0

		if files.count >= MAXFILES then
			print "MAXFILE is too small"
			end 1
		end if
		files.list(files.count) = strStripPrefix(parent + found, runner.cur_dir)
		files.count += 1

		found = dir()
	wend

	'' Scan for subdirectories
	found = dir(parent + "*", fbDirectory or fbReadOnly)
	while len(found) > 0
		select case found
		case ".", ".."
			'' Ignore these special subdirectories

		case else
			'' Remember the subdirectory for further scanning
			dirsAppend(parent + found)
		end select

		found = dir()
	wend
end sub

sub hScanDirectory(byref rootdir as string, byref filepattern as string)
	dirsAppend(rootdir)

	'' Work off the queue -- each subdir scan can append new subdirs
	while dirs.head
		hScanParent(dirs.head->path, filepattern)
		dirsDropHead()
	wend
end sub

private function hPartition _
	( _
		byval l as integer, _
		byval m as integer, _
		byval r as integer _
	) as integer

	dim as string pivot = files.list(m)
	dim as integer store = l

	swap files.list(m), files.list(r)

	for i as integer = l to r - 1
		if lcase(files.list(i)) <= lcase(pivot) then
			swap files.list(i), files.list(store)
			store += 1
		end if
	next

	swap files.list(store), files.list(r)

	function = store
end function

private sub hQuickSort(byval l as integer, byval r as integer)
	if l < r then
		dim as integer m = l + ((r - l) \ 2)
		m = hPartition(l, m, r)
		hQuickSort(l, m - 1)
		hQuickSort(m + 1, r)
	end if
end sub

sub hSortFiles()
	hQuickSort(0, files.count - 1)
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

runner.exe_path = pathAddDiv(exepath())
runner.cur_dir = pathAddDiv(curdir())
runner.fbfrog = pathStripLastComponent(runner.exe_path) + "fbfrog" + EXEEXT
if (runner.cur_dir + "tests" + PATHDIV) = runner.exe_path then
	runner.fbfrog = "./fbfrog" + EXEEXT
end if

var clean_only = FALSE
for i as integer = 1 to __FB_ARGC__-1
	select case *__FB_ARGV__[i]
	case "-clean"
		clean_only = TRUE
	case else
		print "unknown option: " + *__FB_ARGV__[i]
		end 1
	end select
next

'' Clean test directories: Delete existing *.txt and *.bi files
hScanDirectory(runner.exe_path, "*.txt")
hScanDirectory(runner.exe_path, "*.bi")
for i as integer = 0 to files.count-1
	var dummy = kill(files.list(i))
next
files.count = 0

if clean_only then
	end 0
end if

'' Test each *.h file
hScanDirectory(runner.exe_path, "*.h")
hSortFiles()
for i as integer = 0 to files.count-1
	hTest(files.list(i))
next
print "done: " & files.count & " tests"
files.count = 0

if hShell("git diff --quiet") = 0 then
	print !"result: ok (\"git diff\" indicates no unexpected changes)"
else
	print !"result: \"git diff\" indicates changes; please check for problems in the .bi and .txt files in tests/"
end if

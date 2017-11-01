'' Path/file name handling functions

#include once "util-path.bi"
#include once "dir.bi"

'' Searches backwards for the last '.' while still behind '/' or '\'.
private function hFindExtBegin(byref path as const string) as integer
	for i as integer = len(path)-1 to 0 step -1
		select case path[i]
		case asc(".")
			return i
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
		case asc($"\"), asc("/")
#else
		case asc("/")
#endif
			exit for
		end select
	next
	function = len(path)
end function

function pathStripExt(byref path as const string) as string
	function = left(path, hFindExtBegin(path))
end function

function pathExtOnly(byref path as const string) as string
	'' -1 to strip the '.' in front of the file extension
	function = right(path, len(path) - hFindExtBegin(path) - 1)
end function

private function hFindFileName(byref path as const string) as integer
	for i as integer = len(path)-1 to 0 step -1
		select case path[i]
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
		case asc($"\"), asc("/")
#else
		case asc("/")
#endif
			return i + 1
		end select
	next
end function

function pathOnly(byref path as const string) as string
	function = left(path, hFindFileName(path))
end function

function pathStrip(byref path as const string) as string
	function = right(path, len(path) - hFindFileName(path))
end function

function pathAddDiv(byref path as const string) as string
	dim as string s = path
	var length = len(s)

	if length > 0 then
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
		select case s[length-1]
		case asc($"\"), asc("/")

		case else
			s += $"\"
		end select
#else
		if s[length-1] <> asc("/") then
			s += "/"
		end if
#endif
	end if

	function = s
end function

private function pathGetRootLength(byref s as const string) as integer
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
	if len(s) >= 3 then
		'' x:\...
		if s[1] = asc(":") then
			select case s[2]
			case asc("/"), asc($"\")
				return 3
			end select
		end if
	end if
#ifdef __FB_WIN32__
	'' UNC paths
	if len(s) >= 2 then
		'' \\...
		if s[0] = asc($"\") then
			if s[1] = asc($"\") then
				return 2
			end if
		end if
	end if
#endif
#else
	if len(s) >= 1 then
		'' /...
		if s[0] = asc("/") then
			return 1
		end if
	end if
#endif
end function

function pathIsAbsolute(byref s as const string) as integer
	function = (pathGetRootLength(s) > 0)
end function

'' Turns a relative path into an absolute path
function pathMakeAbsolute(byref path as const string) as string
	if pathIsAbsolute(path) then
		function = path
	else
		function = hCurdir() + path
	end if
end function

function hExepath() as string
	function = pathAddDiv(exepath())
end function

function hCurdir() as string
	function = pathAddDiv(curdir())
end function

function pathStripCurdir(byref path as const string) as string
	var pwd = hCurdir()
	if left(path, len(pwd)) = pwd then
		function = right(path, len(path) - len(pwd))
	else
		function = path
	end if
end function

private function pathEndsWithDiv(byref s as const string) as integer
	var length = len(s)
	if length > 0 then
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
		select case s[length-1]
		case asc($"\"), asc("/")
			function = TRUE
		end select
#else
		function = (s[length-1] = asc("/"))
#endif
	end if
end function

function hReadableDirExists(byref path as const string) as integer
	dim as string fixed = path
	if right(fixed, len(PATHDIV)) = PATHDIV then
		fixed = left(fixed, len(fixed) - len(PATHDIV))
	end if
	function = (len(dir(fixed, fbDirectory or fbReadOnly or fbHidden)) > 0)
end function

function pathIsDir(byref s as const string) as integer
	function = hReadableDirExists(s) or pathEndsWithDiv(s)
end function

'' Component stack for the path solver
type PathSolverStack
	const MAXSOLVERSTACK = 128
	stack(0 to MAXSOLVERSTACK-1) as integer
	top as integer = -1
	declare sub push(byval w as integer)
	declare function pop() as integer
end type

sub PathSolverStack.push(byval w as integer)
	top += 1
	if top >= MAXSOLVERSTACK then
		print "error: path solver stack too small, MAXSOLVERSTACK=" & MAXSOLVERSTACK
		end 1
	end if
	stack(top) = w
end sub

function PathSolverStack.pop() as integer
	if top > 0 then
		top -= 1
	end if
	function = stack(top)
end function

'' Resolves .'s and ..'s in the path,
'' normalizes path separators to the host standard.
function pathNormalize(byref path as const string) as string
	var rootlen = pathGetRootLength(path)
	if rootlen = 0 then
		return path
	end if

	'' r: read position, w: write position
	'' r reads ahead, while w slowly writes out the result.
	'' First r and w stay together, but as soon as r finds a .., w is set
	'' back a bit, right in front of the path component it wrote last, so
	'' that the component is dropped (it will be overwritten by following
	'' components).
	'' The stack is needed to be able to skip back over multiple components
	'' in succession, for example in 'aa/bb/../..'.

	'' r and w start out behind the root path (/ or C:\ etc.) so that it's
	'' not touched. The begin of the first component after the root path
	'' must be on the stack to be able to skip back to it (although the
	'' begin of the root path itself, 0, is not on the stack, so it can't
	'' be removed with a '..').
	dim stack as PathSolverStack
	stack.push(rootlen)

	dim as string s = path
	var dots = 0 '' Number of .'s in the current component
	var chars = 0 '' Number of chars in the current component
	var w = rootlen

	for r as integer = rootlen to len(s) - 1
		select case s[r]
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
		case asc($"\"), asc("/")
#else
		case asc("/")
#endif
			'' Component closed: check whether it was /./ or /../
			select case dots
			case 1    '' /./
				'' Ignore: don't write this /, and remove the .
				w -= 1

			case 2    '' /../
				'' Go back to the begin of the component this
				'' '..' refers to
				w = stack.pop()

			case else
				if chars = 0 then
					'' // (Ignore: don't write this /)
				else
					'' Write this /. For Win32/DOS this also normalizes / to \.
					s[w] = asc(PATHDIV)
					'' New component starts behind this /
					w += 1
					'' Remember this begin position so
					'' w can be reset to it during '..'.
					stack.push(w)
				end if

			end select

			dots = 0
			chars = 0

		case asc(".")
			dots += 1
			chars += 1
			s[w] = s[r]
			w += 1

		case else
			dots = 0
			chars += 1
			s[w] = s[r]
			w += 1

		end select
	next

	function = left(s, w)
end function

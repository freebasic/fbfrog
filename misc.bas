#include once "fbfrog.bi"
#include once "crt.bi"
#include once "dir.bi"

sub oops_bug _
	( _
		byval test as zstring ptr, _
		byval funcname as zstring ptr, _
		byval linenum as integer _
	)
	print "bug: failure at " & lcase(*funcname) & _
			"(" & linenum & "): " & *test
	end 1
end sub

sub oops(byref message as string)
	print "oops, " & message
	end 1
end sub

private sub oops_mem(byval size as ulong)
	oops("memory allocation failed (asked for " & size & " bytes)")
end sub

function xallocate(byval size as ulong) as any ptr
	dim as any ptr p = allocate(size)
	if (p = NULL) then
		oops_mem(size)
	end if
	return p
end function

function xcallocate(byval size as ulong) as any ptr
	dim as any ptr p = callocate(size)
	if (p = NULL) then
		oops_mem(size)
	end if
	return p
end function

function xreallocate(byval old as any ptr, byval size as ulong) as any ptr
	dim as any ptr p = reallocate(old, size)
	if (p = NULL) then
		oops_mem(size)
	end if
	return p
end function

function str_duplicate _
	( _
		byval s as ubyte ptr, _
		byval length as integer _
	) as zstring ptr

	dim as zstring ptr p = xallocate(length + 1)

	if (length > 0) then
		memcpy(p, s, length)
	end if
	p[length] = 0

	return p
end function

'' Searches backwards for the last '.' while still behind '/' or '\'.
private function find_ext_begin(byref path as string) as integer
	for i as integer = (len(path)-1) to 0 step -1
		dim as integer ch = path[i]
		if (ch = asc(".")) then
			return i
		elseif ((ch = asc("/")) or (ch = asc("\"))) then
			exit for
		end if
	next
	return len(path)
end function

function path_strip_ext(byref path as string) as string
	return left(path, find_ext_begin(path))
end function

function path_ext_only(byref path as string) as string
	return right(path, len(path) - (find_ext_begin(path) + 1))
end function

private function find_file_name(byref path as string) as integer
	for i as integer = (len(path) - 1) to 0 step -1
		select case (path[i])
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
		case asc("\"), asc("/")
#else
		case asc("/")
#endif
			return i + 1
		end select
	next
	return 0
end function

function path_only(byref path as string) as string
	return left(path, find_file_name(path))
end function

function path_add_div(byref path as string) as string
	dim as string s = path
	dim as integer length = len(s)

	if (length > 0) then
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
		select case (s[length - 1])
		case asc("\"), asc("/")

		case else
			s &= "\"
		end select
#else
		if (s[length - 1] <> asc("/")) then
			s &= "/"
		end if
#endif
	end if

	return s
end function

private function path_get_root_length(byref s as string) as integer
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
	if (len(s) >= 3) then
		'' x:\...
		if (s[1] = asc(":")) then
			select case (s[2])
			case asc("/"), asc("\")
				return 3
			end select
		end if
	end if
#ifdef __FB_WIN32__
	'' UNC paths
	if (len(s) >= 2) then
		'' \\...
		if (s[0] = asc("\")) then
			if (s[1] = asc("\")) then
				return 2
			end if
		end if
	end if
#endif
#else
	if (len(s) >= 1) then
		'' /...
		if (s[0] = asc("/")) then
			return 1
		end if
	end if
#endif
	return 0
end function

function path_strip_last_component(byref path as string) as string
	return path_only(left(path, len(path) - 1))
end function

function path_find_common_base _
	( _
		byref aorig as string, _
		byref borig as string _
	) as string

	dim as string a = aorig
	dim as string b = borig
	dim as integer aroot = path_get_root_length(a)
	dim as integer broot = path_get_root_length(b)

	do
		dim as integer alen = len(a)
		dim as integer blen = len(b)

		if ((alen < aroot) or (blen < broot)) then
			exit do
		end if

		if (alen = blen) then
			if (a = b) then
				return a
			end if
		end if

		if (alen > blen) then
			a = path_strip_last_component(a)
		else
			b = path_strip_last_component(b)
		end if
	loop

	return ""
end function

function path_strip_common_base _
	( _
		byref a as string, _
		byref b as string _
	) as string
	return right(a, len(a) - len(path_find_common_base(a, b)))
end function

'' Component stack for the path solver
type PathSolver
	as integer ptr p
	as integer room
	as integer top
end type

dim shared as PathSolver solver

private sub solver_init()
	solver.p = NULL
	solver.room = 0
	solver.top = -1
end sub

private sub solver_end()
	deallocate(solver.p)
end sub

private sub solver_push(byval w as integer)
	solver.top += 1

	if (solver.top >= solver.room) then
		solver.room += 128
		solver.p = xreallocate(solver.p, sizeof(integer) * solver.room)
	end if

	solver.p[solver.top] = w
end sub

private function solver_pop() as integer
	if (solver.top > 0) then
		solver.top -= 1
	end if
	return solver.p[solver.top]
end function

'' Turns a relative path into an absolute path
function path_make_absolute(byref path as string) as string
	if (path_get_root_length(path) = 0) then
		return path_add_div(curdir()) + path
	end if
	return path
end function

'' Resolves .'s and ..'s in the path,
'' normalizes path separators to the host standard.
function path_normalize(byref path as string) as string
	dim as integer rootlen = path_get_root_length(path)
	if (rootlen = 0) then
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
	solver_init()
	solver_push(rootlen)

	dim as string s = path
	dim as integer dots = 0 '' Number of .'s in the current component
	dim as integer chars = 0 '' Number of chars in the current component
	dim as integer w = rootlen

	for r as integer = rootlen to (len(s) - 1)
		select case (s[r])
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
		case asc("\"), asc("/")
#else
		case asc("/")
#endif
			'' Component closed: check whether it was /./ or /../

			select case (dots)
			case 1    '' /./
				'' Ignore: don't write this /, and remove the .
				w -= 1

			case 2    '' /../
				'' Go back to the begin of the component this
				'' '..' refers to
				w = solver_pop()

			case else
				if (chars = 0) then
					'' // (Ignore: don't write this /)
				else
					'' Write this /
#if defined(__FB_WIN32__) or defined(__FB_DOS__)
					s[w] = asc("\") '' This also normalizes / to \
#else
					s[w] = asc("/")
#endif
					'' New component starts behind this /
					w += 1
					'' Remember this begin position so
					'' w can be reset to it during '..'.
					solver_push(w)
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

	solver_end()

	s = left(s, w)

	return s
end function

function file_exists(byref file as string) as integer
	dim as integer f = freefile()
	if (open(file, for binary, access read, as #f) = 0) then
		close #f
		return TRUE
	end if
	return FALSE
end function

type DirNode
	as DirNode ptr next
	as string path
end type

type DirQueue
	as DirNode ptr head
	as DirNode ptr tail

	as integer dircount
	as integer filecount
end type

dim shared as DirQueue dirs

private sub dirs_append(byref path as string)
	dim as DirNode ptr node = callocate(sizeof(DirNode))
	node->path = path

	if (dirs.tail) then
		dirs.tail->next = node
	end if
	dirs.tail = node
	if (dirs.head = NULL) then
		dirs.head = node
	end if
end sub

private sub dirs_drop_head()
	if (dirs.head) then
		dim as DirNode ptr node = dirs.head
		dirs.head = node->next
		if (dirs.head = NULL) then
			dirs.tail = NULL
		end if
		node->path = ""
		deallocate(node)
	end if
end sub

private sub scan_current_head_and_append()
	const PATTERN = "*.h"

	''if (frog.verbose) then
	''	print " dir: " & dirs.head->path
	''end if

	'' Find files and subdirs (first level children only)
	''
	'' Dirs must be appended to a list for later,
	'' since dir() can't be used recursively due to
	'' its context sensitivity.
	'' Alternative: one threadcreate() per dir
	''
	'' Files on the other hand can be handled immediately.

	dim as integer attrib = 0
	dim as string found = dir(dirs.head->path & PATTERN, _
	                          fbDirectory or fbNormal, @attrib)

	while (len(found) > 0)
		dim as integer is_dir = ((attrib and fbDirectory) <> 0)

		if (is_dir) then
			'' Directory
			select case (found)
			case ".", ".."

			case else
				found = dirs.head->path + found
				dirs.dircount += 1
				dirs_append(path_add_div(found))
			end select
		else
			'' File
			found = dirs.head->path + found
			dirs.filecount += 1

			''if (frog.verbose) then
			''	print "file: " & found
			''end if

			frog_add_file(found, FALSE, FALSE)
		end if

		found = dir(@attrib)
	wend
end sub

sub scan_directory_for_h(byref rootdir as string)
	dirs.filecount = 0
	dirs.dircount = 1

	dim as string root = path_add_div(rootdir)

	print "scanning directory for *.h files: '" & root & "'"

	dirs_append(root)

	'' Work off the queue -- each subdir scan can append new subdirs
	while (dirs.head)
		scan_current_head_and_append()
		dirs_drop_head()
	wend

	if (frog.verbose) then
		dim as string files = "file"
		if (dirs.filecount <> 1) then
			files += "s"
		end if

		dim as string directories = "director"
		if (dirs.dircount = 1) then
			directories += "y"
		else
			directories += "ies"
		end if

		print using "  scanner: found & " & files & _
				" in & " & directories; _
				dirs.filecount, dirs.dircount
	end if
end sub

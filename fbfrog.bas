#include once "fbfrog.bi"

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

private sub print_help()
	print _
	!"usage: fbfrog [--options] *.h\n" & _
	!"For every given C header (*.h) an FB header (*.bi) will be generated.\n" & _
	!"The resulting .bi files may need further editing; watch out for TODOs."

	end 0
end sub

private sub print_version()
	print "0.1"
	end 0
end sub

private sub handle_arg(byref arg as string)
	select case (arg)
	case "--help"
		print_help()

	case "--version"
		print_version()

	case else
		if (len(arg) = 0) then
			return
		end if

		if (arg[0] = asc("-")) then
			oops("unknown option: '" & arg & "', try --help")
		end if

		frog_add_file(arg, path_strip_ext(arg) & ".bi")

	end select
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	frog_init()

	for i as integer = 1 to (__FB_ARGC__ - 1)
		handle_arg(*__FB_ARGV__[i])
	next

	frog_work()

	frog_end()
	end 0

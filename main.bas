#include once "lex.bi"







private sub parse_header(byref file as string)
	lex_select_file(file)
	do
		lex_skip()

		if (lex_tk() = TK_EOF) then
			exit do
		end if
	loop
end sub

private sub print_help_and_exit()
	print "usage: frog [ *.h ] [ --help ]"
	print "For every given C header (*.h) an FB header (*.bi) will be generated."
	print "With some luck and not too complex headers the translation should be fine."
	print "If something is wrong or requires a human eye, you will be notified via"
	print "errors and warnings, and there may be notes emitted into the .bi files."
	end 1
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	if (__FB_ARGC__ = 1) then
		print_help_and_exit()
	end if

	dim as string arg

	'' Check for command line options
	for i as integer = 1 to (__FB_ARGC__ - 1)
		arg = *__FB_ARGV__[i]

		if (arg[0] <> asc("-")) then
			continue for
		end if

		select case (arg)
		case "--help"
			print_help_and_exit()

		case "--"
			cptr(ubyte ptr, __FB_ARGV__[i])[0] = 0
			exit for

		case else
			oops("unknown command line option: '" & arg & "'")
			print_help_and_exit()

		end select
	next

	lex_global_init()

	'' Parse all files specified on the command line
	for i as integer = 1 to (__FB_ARGC__ - 1)
		arg = *__FB_ARGV__[i]

		'' Ignore args that we emptied when processing the options
		if (len(arg) > 0) then
			parse_header(arg)
		end if
	next

	print "Done."
	end 0

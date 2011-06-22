#include once "lex.bi"
#include once "emit.bi"

private sub parse_pp_directive()
	'' #
	lex_skip()

	select case as const (lex_tk())
	case TK_IF

	case TK_IFDEF

	case TK_IFNDEF

	case TK_ELIF

	case TK_ENDIF

	case TK_PRAGMA
		lex_skip()

		'' Handle GCC's '#pragma ms_struct on | off | reset'
		if (lex_match_text("ms_struct")) then
			select case (*lex_text())
			case "on"
			case "off", "reset"
			end select
		end if

	case else
		lex_xoops("unexpected PP directive: '" & *lex_text() & "'")
	end select
end sub

private sub parse_header(byref file as string)
	lex_open(file)
	emit_open(path_strip_ext(file) & ".bi")

	do
		lex_skip()

		select case as const (lex_tk())
		case TK_HASH
			parse_pp_directive()

		case TK_EOF
			exit do

		case TK_EOL
			lex_skip()

		end select
	loop

	emit_close()
	lex_close()
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
	emit_global_init()

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

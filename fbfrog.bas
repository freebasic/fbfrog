#include once "common.bi"
#include once "tk.bi"

#define FROG_VERSION "0.1"
#define FROG_HELP _
	!"usage: fbfrog *.h\n" & _
	!"For every given C header (*.h) an FB header (*.bi) will be generated.\n" & _
	!"With some luck and not too complex headers the translation should be fine.\n" & _
	!"If something is wrong or requires a human eye, there will be TODOs written\n" & _
	!"into the .bi files."

sub _xassertfail _
	( _
		byval test as zstring ptr, _
		byval filename as zstring ptr, _
		byval funcname as zstring ptr, _
		byval linenum as integer _
	)
	print *filename & "(" & linenum & "):" & *funcname & "(): assertion failed: " & *test
	xoops("internal problem, please report this bug!")
end sub

sub xoops(byref message as string)
	print "oops, " & message
	end 1
end sub

private sub xoops_mem(byval size as ulong)
	xoops("oops, memory allocation failed (asked for " & size & " bytes)")
end sub

function xallocate(byval size as ulong) as any ptr
	dim as any ptr p = allocate(size)
	if (p = NULL) then
		xoops_mem(size)
	end if
	return p
end function

function xcallocate(byval size as ulong) as any ptr
	dim as any ptr p = callocate(size)
	if (p = NULL) then
		xoops_mem(size)
	end if
	return p
end function

function xreallocate(byval old as any ptr, byval size as ulong) as any ptr
	dim as any ptr p = reallocate(old, size)
	if (p = NULL) then
		xoops_mem(size)
	end if
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

private function is_whitespace(byval x as integer) as integer
	dim as integer id = tk_get(x)
	return ((id = TK_EOL) or (id = TK_SPACE) or _
	        (id = TK_COMMENT) or (id = TK_LINECOMMENT))
end function

private function skip_whitespace(byval x as integer) as integer
	while (is_whitespace(x))
		x += 1
	wend
	return x
end function

'' Skips the token and any following whitespace
private function skip_soft(byval x as integer) as integer
	return skip_whitespace(x + 1)
end function

private function parse_pp_directive(byval x as integer) as integer
	'' (Assuming all '#' are indicating a PP directive)
	if (tk_get(x) <> TK_HASH) then
		return x
	end if

	dim as integer begin = x

	'' Skip until EOL, but also handle PP line continuation
	do
		x += 1

		select case (tk_get(x))
		case TK_EOL
			if (tk_get(x - 1) <> TK_BACKSLASH) then
				exit do
			end if

		case TK_EOF
			exit do

		end select
	loop

	tk_mark_stmt(STMT_PP, begin, x)

	return x
end function

private function find_parentheses_backwards(byval x as integer) as integer
	dim as integer opening_tk = any
	dim as integer closing_tk = tk_get(x)

	select case (closing_tk)
	case TK_RPAREN
		opening_tk = TK_LPAREN
	case TK_RBRACE
		opening_tk = TK_LBRACE
	case TK_RBRACKET
		opening_tk = TK_LBRACKET
	case else
		return x
	end select

	dim as integer level = 0
	dim as integer old = x
	do
		x -= 1

		select case (tk_get(x))
		case opening_tk
			if (level = 0) then
				'' Found it
				exit do
			end if
			level -= 1

		case closing_tk
			level += 1

		case TK_EOF
			'' Not in this file anyways
			return old

		end select
	loop

	return x
end function

'' EXTERN string '{'
private function parse_extern(byval x as integer) as integer
	dim as integer begin = x

	if (tk_get(x) <> KW_EXTERN) then
		return x
	end if
	x = skip_soft(x)

	if (tk_get(x) <> TK_STRING) then
		return begin
	end if
	x = skip_soft(x)

	'' Opening '{'
	if (tk_get(x) <> TK_LBRACE) then
		return begin
	end if

	tk_mark_stmt(STMT_EXTERN, begin, x + 1)

	return skip_soft(x)
end function

private function parse_closing_braces(byval x as integer) as integer
	if (tk_get(x) <> TK_RBRACE) then
		return x
	end if

	dim as integer opening = find_parentheses_backwards(x)
	if (opening = x) then
		return x
	end if

	dim as integer stmt = 0

	select case (tk_stmt(opening))
	case STMT_EXTERN
		stmt = STMT_END_EXTERN
	end select

	tk_mark_stmt(stmt, x, x + 1)

	return skip_soft(x)
end function

private sub parse_toplevel()
	dim as integer x = 0
	do
		x = parse_pp_directive(x)
		x = parse_extern(x)
		x = parse_closing_braces(x)

		select case (tk_get(x))
		case TK_BYTE, TK_EOL, TK_SPACE, TK_COMMENT, TK_LINECOMMENT
			x += 1

		case TK_EOF
			exit do

		case else
			x += 1

		end select
	loop
end sub

private sub filter_semicolons()
	dim as integer lastsemi = -1
	dim as integer x = 0
	do
		select case (tk_get(x))
		case TK_SEMI
			'' All ';' can just be removed
			tk_remove(x)
			lastsemi = x

		case TK_EOL
			lastsemi = -1
			x += 1

		case TK_SPACE, TK_COMMENT, TK_LINECOMMENT
			'' Allowed between semi-colon and EOL
			x += 1

		case TK_EOF
			exit do

		case else
			'' However if there is no EOL coming,
			'' add an ':' (FB's statement separator) instead.
			if (lastsemi >= 0) then
				tk_insert(lastsemi, TK_COLON, NULL)
				lastsemi = -1
				x += 1
			end if

			x += 1

		end select
	loop
end sub

private sub filter_eol_and_comments_in_statements()
	dim as integer x = 0
	do
		select case (tk_get(x))
		case TK_EOF
			exit do

		case TK_EOL, TK_COMMENT, TK_LINECOMMENT
			if (tk_stmt(x) <> STMT_TOPLEVEL) then
				tk_remove(x)
				'' Removing EOL or comment might result in
				'' a lack of a space token, fix that up.
				if ((tk_get(x - 1) <> TK_SPACE) and _
				    (tk_get(x    ) <> TK_SPACE)) then
					tk_insert_space(x)
				end if
			end if

		end select

		x += 1
	loop
end sub

private sub translate_toplevel()
	dim as integer x = 0
	do
		select case (tk_stmt(x))
		case STMT_EXTERN
			'' EXTERN "C" '{' -> EXTERN "C"
			'' Just jump to the '{' and remove it
			x += 3
			while (tk_get(x) <> TK_LBRACE)
				x += 1
			wend
			tk_remove(x)

		case STMT_END_EXTERN
			'' '}' -> END EXTERN
			tk_remove(x)
			tk_insert(x, KW_END, NULL)
			x += 1
			tk_insert_space(x)
			x += 1
			tk_insert(x, KW_EXTERN, NULL)

		end select

		select case (tk_get(x))
		case TK_EOF
			exit do

		case else
			x += 1

		end select
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	if (__FB_ARGC__ = 1) then
		print FROG_HELP
		end 0
	end if

	'' Check for command line options
	dim as integer filecount = 0
	for i as integer = 1 to (__FB_ARGC__ - 1)
		dim as zstring ptr arg = __FB_ARGV__[i]
		if (*arg = "--version") then
			print "fbfrog " & FROG_VERSION
			end 0
		elseif (*arg = "--help") then
			print FROG_HELP
			end 0
		elseif (cptr(ubyte ptr, arg)[0] = asc("-")) then
			xoops("unknown option: '" & *arg & "', try --help")
		else
			filecount += 1
		end if
	next
	if (filecount = 0) then
		xoops("no input files")
	end if

	tk_init()

	'' Parse the files specified on the command line
	dim as string hfile, bifile
	for i as integer = 1 to (__FB_ARGC__ - 1)
		dim as zstring ptr arg = __FB_ARGV__[i]
		if (cptr(ubyte ptr, arg)[0] <> asc("-")) then
			hfile = *arg

			print "loading '" & hfile & "'..."
			tk_in_file(hfile)

			print "parsing..."
			parse_toplevel()
			print "filtering semi-colons..."
			filter_semicolons()
			print "filtering eol/comments inside constructs..."
			filter_eol_and_comments_in_statements()
			print "main translation..."
			translate_toplevel()

			bifile = path_strip_ext(hfile) & ".bi"
			print "emitting '" & bifile & "'..."
			tk_emit_file(bifile)
		end if
	next

	tk_end()
	end 0

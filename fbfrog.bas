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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function is_whitespace(byval x as integer) as integer
	dim as integer id = tk_get(x)
	return ((id = TK_EOL) or (id = TK_SPACE) or _
	        (id = TK_COMMENT) or (id = TK_LINECOMMENT))
end function

'' Skips the token and any following whitespace
private function skip(byval x as integer) as integer
	do
		x += 1
	loop while (is_whitespace(x))
	return x
end function

'' Same, but backwards
private function skiprev(byval x as integer) as integer
	do
		x -= 1
	loop while (is_whitespace(x))
	return x
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

'' EXTERN/STRUCT/ENUM blocks
private function parse_compound_opening(byval x as integer) as integer
	dim as integer stmt = any

	select case (tk_get(x))
	case KW_EXTERN
		stmt = STMT_EXTERN
	case KW_STRUCT
		stmt = STMT_STRUCT
	case KW_ENUM
		stmt = STMT_ENUM
	case else
		return x
	end select

	dim as integer begin = x
	x = skip(x)

	if (stmt = STMT_EXTERN) then
		'' EXTERN requires a following string
		if (tk_get(x) <> TK_STRING) then
			return begin
		end if
		x = skip(x)
	else
		'' STRUCT/ENUM can have an optional id
		if (tk_get(x) = TK_ID) then
			x = skip(x)
		end if
	end if

	'' Opening '{'?
	if (tk_get(x) <> TK_LBRACE) then
		return begin
	end if

	'' Mark the '{' too, but not the whitespace/comments behind it,
	'' since that's usually indentation belonging to something else.
	x += 1
	tk_mark_stmt(stmt, begin, x)

	return x
end function

private function parse_compound_closing(byval x as integer) as integer
	if (tk_get(x) <> TK_RBRACE) then
		return x
	end if

	dim as integer opening = find_parentheses_backwards(x)
	if (opening = x) then
		return x
	end if

	x += 1

	dim as integer stmt = tk_stmt(opening)

	select case (stmt)
	case STMT_EXTERN
		stmt = STMT_END_EXTERN
	case STMT_STRUCT
		stmt = STMT_END_STRUCT
	case STMT_ENUM
		stmt = STMT_END_ENUM
	end select

	tk_mark_stmt(stmt, x - 1, x)

	return x
end function


end function

private sub parse_toplevel()
	dim as integer x = 0
	do
		dim as integer old = x

		x = parse_pp_directive(x)
		x = parse_compound_opening(x)
		x = parse_compound_closing(x)

		if (x = old) then
			'' Token/construct couldn't be identified, so make
			'' sure the parsing advances somehow...
			x = skip(x)
		end if
	loop while (tk_get(x) <> TK_EOF)
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

'' EOL fixup -- in C it's possible to have constructs split over multiple
'' lines, which requires a '_' line continuation char in FB. Also, the CPP
'' \<EOL> line continuation needs to be converted to FB.
private sub fixup_eols()
	dim as integer x = 0
	do
		select case (tk_get(x))
		case TK_EOF
			exit do

		case TK_EOL
			select case (tk_stmt(x))
			case STMT_TOPLEVEL
				'' (EOLs at toplevel are supposed to stay)

			case STMT_PP
				'' EOLs can only be part of PP directives if
				'' the newline char is escaped with '\'.
				'' For FB that needs to be replaced with a '_'.
				'' That might require an extra space too,
				'' because '_' can be part of identifiers,
				'' unlike '\'...
				x -= 2
				if (tk_get(x) <> TK_SPACE) then
					tk_insert_space(x)
					x += 1
				end if

				'' Back to '\'
				x += 1

				'' Replace the '\' by '_'
				xassert(tk_get(x) = TK_BACKSLASH)
				tk_remove(x)
				tk_insert(x, TK_UNDERSCORE, NULL)

				'' Back to EOL
				x += 1

			case else
				'' For EOLs inside constructs, '_'s need to
				'' be added so it works in FB. It should be
				'' inserted in front of any line comment or
				'' space that aligns the line comment.
				dim as integer y = x - 1
				if (tk_get(y) = TK_LINECOMMENT) then
					y -= 1
				end if
				if (tk_get(y) = TK_SPACE) then
					y -= 1
				end if
				y += 1
				tk_insert_space(y)
				tk_insert(y + 1, TK_UNDERSCORE, NULL)
				x += 2

			end select

		end select

		x += 1
	loop
end sub

private function remove_following_lbrace(byval x as integer) as integer
	'' EXTERN "C" '{' -> EXTERN "C"
	'' Just jump to the '{' and remove it
	while (tk_get(x) <> TK_LBRACE)
		x += 1
	wend
	tk_remove(x)
	return x
end function

private function translate_compound_end _
	( _
		byval x as integer, _
		byval compound_kw as integer _
	) as integer

	'' '}' -> END EXTERN
	tk_remove(x)

	tk_insert(x, KW_END, NULL)
	x += 1
	tk_insert_space(x)
	x += 1
	tk_insert(x, compound_kw, NULL)

	return x
end function

private sub translate_toplevel()
	dim as integer x = 0
	do
		select case (tk_stmt(x))
		case STMT_EXTERN, STMT_ENUM
			'' Just remove the '{'
			x = remove_following_lbrace(x)

		case STMT_STRUCT
			'' STRUCT -> TYPE
			tk_remove(x)
			tk_insert(x, KW_TYPE, NULL)
			'' And also remove the '{'
			x = remove_following_lbrace(x)

		case STMT_END_EXTERN
			x = translate_compound_end(x, KW_EXTERN)

		case STMT_END_STRUCT
			x = translate_compound_end(x, KW_TYPE)

		case STMT_END_ENUM
			x = translate_compound_end(x, KW_ENUM)

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

			print "translating..."
			filter_semicolons()
			fixup_eols()
			translate_toplevel()

			bifile = path_strip_ext(hfile) & ".bi"
			print "emitting '" & bifile & "'..."
			tk_emit_file(bifile)
		end if
	next

	tk_end()
	end 0

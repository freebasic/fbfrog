#include once "frog.bi"

'' Skips the token and any following whitespace
private function skip_(byval x as integer, byval delta as integer) as integer
	do
		x += delta

		select case (tk_get(x))
		case TK_EOL, TK_SPACE, TK_COMMENT, TK_LINECOMMENT, TK_TODO

		case else
			exit do
		end select
	loop
	return x
end function

function skip(byval x as integer) as integer
	return skip_(x, 1)
end function

function skiprev(byval x as integer) as integer
	return skip_(x, -1)
end function

private function skip_unless_eol_ _
	( _
		byval x as integer, _
		byval delta as integer _
	) as integer

	do
		x += delta

		select case (tk_get(x))
		case TK_SPACE, TK_COMMENT, TK_LINECOMMENT, TK_TODO

		case else
			exit do
		end select
	loop

	return x
end function

function skip_unless_eol(byval x as integer) as integer
	return skip_unless_eol_(x, 1)
end function

function skiprev_unless_eol(byval x as integer) as integer
	return skip_unless_eol_(x, -1)
end function

function is_whitespace_until_eol(byval x as integer) as integer
	do
		select case (tk_get(x))
		case TK_EOL, TK_EOF
			exit do

		case TK_SPACE, TK_COMMENT, TK_LINECOMMENT, TK_TODO

		case else
			return FALSE

		end select

		x += 1
	loop
	return TRUE
end function

function skip_optional _
	( _
		byval x as integer, _
		byval tk as integer _
	) as integer

	if (tk_get(x) = tk) then
		x = skip(x)
	end if

	return x
end function

function insert_spaced_token _
	( _
		byval x as integer, _
		byval tk as integer, _
		byval text as zstring ptr _
	) as integer

	select case (tk_get(x - 1))
	case TK_SPACE, TK_EOL, TK_EOF

	case else
		tk_insert_space(x)
		x += 1
	end select

	tk_insert(x, tk, text)
	x += 1

	select case (tk_get(x))
	case TK_SPACE, TK_EOL, TK_EOF

	case else
		tk_insert_space(x)
		x += 1
	end select

	return x
end function

function insert_statement_separator(byval x as integer) as integer
	'' Only if there's not already another ':'
	if ((tk_get(x) = TK_COLON) or (tk_get(skiprev(x)) = TK_COLON)) then
		return x
	end if
	return insert_spaced_token(x, TK_COLON, NULL)
end function

private function insert_todo _
	( _
		byval x as integer, _
		byval text as zstring ptr _
	) as integer

	dim as integer first = skiprev_unless_eol(x)

	select case (tk_get(first))
	case TK_EOL, TK_EOF
		'' Ok, there's only indentation in front of us.
		'' Insert the TODO right here, then an EOL,
		'' then duplicate the indentation to re-indent
		'' the next construct. That way, the TODO
		'' will appear nicely aligned above the
		'' construct it refers to.
		'' (Often there won't be any indentation,
		'' then nothing will be copied)
		first += 1 '' (not the EOL at the front)
		dim as integer last = x - 1

		tk_insert(x, TK_TODO, text)
		x += 1

		tk_insert(x, TK_EOL, NULL)
		x += 1

		if (first <= last) then
			tk_copy(x, first, last)
			x += last - first + 1
		end if
	case else
		'' Insert in the middle of the line
		x = insert_spaced_token(x, TK_TODO, text)
	end select

	return x
end function

sub remove_this_and_space(byval x as integer)
	tk_remove(x, skip(x) - 1)
end sub

function find_token(byval x as integer, byval tk as integer) as integer
	dim as integer begin = x

	do
		select case (tk_get(x))
		case tk
			exit do
		case TK_EOF
			x = begin
			exit do
		end select

		x += 1
	loop

	return x
end function

function find_parentheses _
	( _
		byval x as integer, _
		byval backwards as integer _
	) as integer

	dim as integer old = x

	dim as integer a = tk_get(x)
	dim as integer b = -1

	if (backwards) then
		select case (a)
		case TK_RPAREN
			b = TK_LPAREN
		case TK_RBRACE
			b = TK_LBRACE
		case TK_RBRACKET
			b = TK_LBRACKET
		end select
	else
		select case (a)
		case TK_LPAREN
			b = TK_RPAREN
		case TK_LBRACE
			b = TK_RBRACE
		case TK_LBRACKET
			b = TK_RBRACKET
		end select
	end if

	if (b < 0) then
		return old
	end if

	dim as integer level = 0
	do
		if (backwards) then
			x = skiprev(x)
		else
			x = skip(x)
		end if

		select case (tk_get(x))
		case b
			if (level = 0) then
				'' Found it
				exit do
			end if
			level -= 1

		case a
			level += 1

		case TK_EOF
			'' Not in this file anyways
			return old

		end select
	loop

	return x
end function

private function skip_statement(byval x as integer) as integer
	dim as integer level = 0

	do
		select case (tk_get(x))
		case TK_SEMI
			if (level = 0) then
				exit do
			end if

		case TK_LBRACE
			level += 1

		case TK_RBRACE
			level -= 1

		case TK_HASH
			return x

		case TK_EOF
			exit do

		end select

		x = skip(x)
	loop

	return skip(x)
end function

function parse_unknown(byval x as integer) as integer
	dim as integer begin = x

	'' The current token/construct couldn't be identified,
	'' so try to skip over the whole statement. Parsing
	'' needs to advance somehow, and everything here should
	'' be re-marked as MARK_UNKNOWN, so it can be marked
	'' with a /single/ TODO.

	'' Find the next '#' or ';' while skipping over ';'s inside
	'' '{...}'.
	x = skip_statement(x)
	tk_set_mark(MARK_UNKNOWN, begin, skiprev(x))

	return x
end function

private function skip_pp(byval x as integer) as integer
	'' Skip over current token like skip(), but stop at EOL, unless it's
	'' escaped (CPP line continuation).

	do
		x = skip_unless_eol(x)

		if (tk_get(x) <> TK_EOL) then
			exit do
		end if

		if (tk_get(x - 1) <> TK_BACKSLASH) then
			exit do
		end if
	loop

	return x
end function

private function skip_pp_directive(byval x as integer) as integer
	do
		x = skip_pp(x)

		select case (tk_get(x))
		case TK_EOL, TK_EOF
			exit do
		end select
	loop

	return x
end function

private function indent_comment _
	( _
		byref oldtext as string, _
		byref prefix as string _
	) as string

	''
	'' Indents a comments body. Since the whole comment is a single token,
	'' the indendation must be done in the comment token's text.
	''
	'' Used to change this:
	''
	''			/* Foo
	''	   bar baz buz */
	''
	'' To this:
	''
	''			/* Foo
	''			   bar baz buz */
	''

	dim as string newtext
	dim as integer behindeol = FALSE

	for i as integer = 0 to (len(oldtext) - 1)
		if (behindeol) then
			newtext += prefix
		end if

		'' Check the current char, if's an EOL, we'll insert whitespace
		'' behind it later...
		select case (oldtext[i])
		case &h0A '' LF
			behindeol = TRUE
		case &h0D '' CR
			if ((i + 1) < len(oldtext)) then
				behindeol = (oldtext[i + 1] <> &h0A) '' CRLF
			else
				behindeol = TRUE
			end if
		case else
			behindeol = FALSE
		end select

		'' Copy current char
		newtext += chr(oldtext[i])
	next

	if (behindeol) then
		newtext += prefix
	end if

	return newtext
end function

private function handle_include _
	( _
		byval begin as integer, _
		byval x as integer, _
		byref filename as string, _
		byval is_preparse as integer _
	) as integer

	if ((is_preparse = FALSE) and (frog.merge = FALSE)) then
		return x
	end if

	dim as FrogFile ptr f = _
		frog_add_file(filename, is_preparse, TRUE)

	'' File not found?
	if (f = NULL) then
		return x
	end if

	'' Preparse: Lookup/add the file and increase the refcount
	if (is_preparse) then
		f->refcount += 1
		return x
	end if

	'' Otherwise: Just lookup the file entry to check its refcount,
	'' and merge the file in if this is the only place #including it.
	if (f->refcount <> 1) then
		return x
	end if

	print "merging in: " & *f->softname

	'' Remove the #include (but not the EOL behind it)
	ASSUMING(tk_get(begin) = TK_HASH)
	tk_remove(begin, x - 1)

	'' and insert the file content
	x = lex_insert_file(begin, *f->hardname)

	'' If the #include was indented, indent the whole inserted block too.
	dim as integer first = skiprev_unless_eol(begin)
	select case (tk_get(first))
	case TK_EOL, TK_EOF
		'' Ok, there's only indentation in front of the #include.
		'' Copy it in front of every line of the inserted block.

		first += 1 '' (not the EOL in front of the #include)
		dim as integer last = begin - 1

		'' Often there won't be any indentation, then there's nothing
		'' to do.
		if (first <= last) then
			dim as integer y = begin
			while (y < x)
				select case (tk_get(y))
				case TK_EOL
					y += 1
					tk_copy(y, first, last)
					y += last - first + 1
					x += last - first + 1

				case TK_COMMENT
					'' Reindent multiline comments too

					'' 1) Collect the whitespace into
					''    a string
					dim as string text
					for i as integer = first to last
						text += *tk_text(i)
					next

					'' 2) Prefix it to every line in the
					''    comment body
					text = indent_comment(*tk_text(y), text)

					'' 3) Insert the new comment
					tk_insert(y, TK_COMMENT, text)
					y += 1

					'' 4) Remove the old one
					tk_remove(y, y)

				case else
					y += 1
				end select
			wend
		end if
	end select

	return x
end function

private function parse_pp_define_attribute _
	( _
		byval x as integer, _
		byval pflags as uinteger ptr _
	) as integer

	dim as integer begin = x

	'' __attribute__
	if (tk_get(x) <> TK_ID) then
		return begin
	end if

	if (*tk_text(x) <> "__attribute__") then
		return begin
	end if
	x = skip_pp(x)

	'' '(' '('
	for i as integer = 0 to 1
		if (tk_get(x) <> TK_LPAREN) then
			return begin
		end if
		x = skip_pp(x)
	next

	'' id
	if (tk_get(x) <> TK_ID) then
		return begin
	end if

	select case (*tk_text(x))
	case "cdecl", "stdcall", "__stdcall__"
		*pflags or= DEFINE_CALL

	case "dllexport", "dllimport"
		*pflags or= DEFINE_EMPTY

	case else
		return begin
	end select

	x = skip_pp(x)

	return x
end function

private function parse_pp_define_declspec _
	( _
		byval x as integer, _
		byval pflags as uinteger ptr _
	) as integer

	dim as integer begin = x

	'' __declspec
	if (tk_get(x) <> TK_ID) then
		return begin
	end if

	if (*tk_text(x) <> "__declspec") then
		return begin
	end if
	x = skip_pp(x)

	'' '('
	if (tk_get(x) <> TK_LPAREN) then
		return begin
	end if
	x = skip_pp(x)

	'' id
	if (tk_get(x) <> TK_ID) then
		return begin
	end if

	select case (*tk_text(x))
	case "dllexport", "dllimport"
		*pflags or= DEFINE_EMPTY

	case else
		return begin
	end select

	x = skip_pp(x)

	return x
end function

private function determine_pp_define_flags(byval x as integer) as uinteger
	dim as uinteger flags = 0

	'' Empty?
	if (tk_get(x) = TK_EOL) then
		flags or= DEFINE_EMPTY
	end if

	do
		x = parse_pp_define_attribute(x, @flags)
		x = parse_pp_define_declspec(x, @flags)

		select case (tk_get(x))
		case TK_EOL
			exit do

		case TK_ID
			select case (*tk_text(x))
			case "__stdcall"
				flags or= DEFINE_CALL

			end select

		end select

		x = skip_pp(x)
	loop

	return flags
end function

function parse_pp_directive _
	( _
		byval x as integer, _
		byval is_preparse as integer _
	) as integer

	'' (Assuming all '#' are indicating a PP directive)
	if (tk_get(x) <> TK_HASH) then
		return x
	end if

	dim as string filename
	dim as integer begin = x
	x = skip_pp(x)

	'' Mark the expression parts of #if (but not #if itself) specially
	dim as integer mark = MARK_PP
	select case (tk_get(x))
	case KW_IF, KW_IFDEF, KW_IFNDEF, KW_ELIF
		mark = MARK_PPEXPR

	case KW_INCLUDE
		dim as integer y = skip_pp(x)

		'' Turn <...> into a string literal
		'' TODO: This should be handled by the lexer, but it depends
		'' on context, so it could be even harder to do in the lexer
		'' than here. This sort of breaks the strict separation of
		'' parsing/translating, but oh well...

		if (tk_get(y) = TK_LT) then
			dim as integer lt = y
			do
				y += 1

				select case (tk_get(y))
				case TK_GT
					tk_remove(lt, y)
					tk_insert(lt, TK_STRING, filename)
					y = lt
					exit do

				case TK_EOL, TK_EOF
					filename = ""
					exit do

				end select

				dim as zstring ptr text = tk_text(y)
				if (text = NULL) then
					text = token_text(tk_get(y))
				end if
				filename += *text
			loop
		end if

		if (tk_get(y) = TK_STRING) then
			if (tk_get(skip_pp(y)) = TK_EOL) then
				filename = *tk_text(y)
			end if
		end if

	case KW_DEFINE
		dim as integer y = skip_pp(x)
		if (tk_get(y) = TK_ID) then
			dim as uinteger flags = _
					determine_pp_define_flags(skip_pp(y))
			if (flags) then
				frog_add_define(tk_text(y), flags)
			end if
		end if

	end select

	tk_set_mark(MARK_PP, begin, x)

	dim as integer rest = x + 1
	x = skip_pp_directive(x)

	'' The last EOL is not part of the #directive, otherwise the EOL
	'' fixup would replace it with line continuation...
	tk_set_mark(mark, rest, x - 1)

	if (len(filename) > 0) then
		x = handle_include(begin, x, filename, is_preparse)
	end if

	'' In case of merge: skip whitespace at BOF
	'' Otherwise: skip EOL + other following whitespace
	x = skip(x - 1)

	return x
end function

sub preparse_toplevel()
	dim as integer x = skip(-1)
	while (tk_get(x) <> TK_EOF)
		dim as integer old = x
		x = parse_pp_directive(x, TRUE)
		if (x = old) then
			x = skip(x)
		end if
	wend
end sub

'' Normally this will only be called once, but when concatenating, it's called
'' repeatedly to parse the appended files, so found #includes can be searched
'' for based on their parent, the current file.
sub parse_toplevel(byval begin as integer)
	'' Skip space at begin-of-file
	dim as integer x = skip(begin - 1)

	do
		dim as integer old = x

		x = parse_pp_directive(x, FALSE)
		x = parse_topdecl_or_typedef(x)
		x = parse_struct(x)
		x = parse_extern_begin(x)
		x = parse_extern_end(x)

		select case (tk_get(x))
		case TK_SEMI
			'' Random toplevel semicolon
			x = skip(x)

		case TK_EOF
			exit do

		end select

		if (x = old) then
			x = parse_unknown(x)
		end if
	loop
end sub

'' EOL fixup -- in C it's possible to have constructs split over multiple
'' lines, which requires a '_' line continuation char in FB. Also, the CPP
'' \<EOL> line continuation needs to be converted to FB.
private function fixup_eol(byval x as integer) as integer
	select case (tk_mark(x))
	case MARK_TOPLEVEL, MARK_UNKNOWN
		'' (EOLs at toplevel are supposed to stay)

	case MARK_PP, MARK_PPEXPR
		'' CPP uses <'\' EOL> for line continuation; translate that to
		'' <'_' EOL>, and add a space in front of '_' if needed,
		'' so it doesn't become part of some identifier/keyword.
		x -= 2
		if (tk_get(x) <> TK_SPACE) then
			tk_insert_space(x)
			x += 1
		end if

		'' Back to '\'
		x += 1

		'' Replace the '\' by '_'
		ASSUMING(tk_get(x) = TK_BACKSLASH)
		tk_remove(x, x)
		tk_insert(x, TK_UNDERSCORE, NULL)

		'' Back to EOL
		x += 1

	case else
		'' For EOLs inside constructs, add '_' so the line wrapping
		'' is preserved. Alternatively we could just remove the EOLs.

		dim as integer y = x
		y -= 1

		'' If there is a line comment, then the '_' needs to be added
		'' in front of it.
		if (tk_get(y) = TK_LINECOMMENT) then
			y -= 1
		end if

		'' For beauty, we add it even in front of the space that
		'' (possibly) aligns the line comment.
		if (tk_get(y) = TK_SPACE) then
			y -= 1
		end if

		y += 1

		tk_insert_space(y)
		y += 1

		tk_insert(y, TK_UNDERSCORE, NULL)

		x += 2

	end select

	return x
end function

private sub fixup_eols()
	dim as integer x = 0
	do
		select case (tk_get(x))
		case TK_EOF
			exit do

		case TK_EOL
			x = fixup_eol(x)

		end select

		x += 1
	loop
end sub

private sub fixup_comments()
	'' Remove /' and '/ inside comments, since those have meaning in FB
	dim as integer x = 0
	do
		select case (tk_get(x))
		case TK_COMMENT
			dim as zstring ptr s = tk_text(x)
			dim as ubyte ptr p = s
			dim as ubyte ptr limit = p + len(*s)

			while (p < limit)
				select case (p[0])
				case asc("'")
					if ((p + 1) < limit) then
						if (p[1] = asc("/")) then
							p[0] = asc("?")
						end if
					end if
				case asc("/")
					if ((p + 1) < limit) then
						if (p[1] = asc("'")) then
							p[0] = asc("?")
						end if
					else
						p[0] = asc("?")
					end if
				end select

				p += 1
			wend

		case TK_EOF
			exit do
		end select

		x += 1
	loop
end sub

private function fixup_logical_operator _
	( _
		byval x as integer, _
		byval kwforpp as integer, _
		byval kwnormal as integer _
	) as integer

	dim as integer mark = tk_mark(x)

	tk_remove(x, x)

	if (mark = MARK_PPEXPR) then
		x = insert_spaced_token(x, kwforpp, NULL)
	else
		x = insert_spaced_token(x, kwnormal, NULL)
	end if

	return x
end function

private function replace_operator _
	(_
		byval x as integer, _
		byval tk1 as integer, _
		byval tk2 as integer _
	) as integer

	tk_remove(x, x)

	'' Inserting a keyword? That might need spaces too...
	if (tk1 > TK_ID) then
		x = insert_spaced_token(x, tk1, NULL)

		'' Inserting a double-token self-op? (e.g. '%=' -> MOD '=')
		if (tk2 >= 0) then
			ASSUMING(tk_get(x - 1) = tk1)
			tk_insert(x, tk2, NULL)
			x = skip(x)
		else
			x = skip(x - 1)
		end if
	else
		tk_insert(x, tk1, NULL)
		x = skip(x)
	end if

	return x
end function

private function fixup_operator(byval x as integer) as integer
	select case as const (tk_get(x))
	case TK_EXCL '' ! -> NOT
		x = insert_spaced_token(x, TK_TODO, "unary NOT")
		x = replace_operator(x, KW_NOT, -1)

	case TK_EXCLEQ '' != -> <>
		x = replace_operator(x, TK_LTGT, -1)

	case TK_PERCENT '' % -> mod
		x = replace_operator(x, KW_MOD, -1)

	case TK_PERCENTEQ '' %= -> mod=
		x = replace_operator(x, KW_MOD, TK_EQ)

	case TK_AMP '' & -> AND | @
		select case (tk_get(skiprev(x)))
		case TK_ID, _                   '' abc & x
		     TK_DECNUM, TK_HEXNUM, _    '' 123 & x
		     TK_OCTNUM, _
		     TK_RPAREN, _               '' ...) & x
		     TK_RBRACKET                '' ...] & x
			'' This is likely to be AND
			x = replace_operator(x, KW_AND, -1)

		case else
			'' (&x
			'' , &x
			'' etc., this is likely to be @
			x = insert_spaced_token(x, TK_TODO, "was &")
			x = replace_operator(x, TK_AT, -1)

		end select

	case TK_AMPEQ '' &= -> and=
		x = replace_operator(x, KW_AND, TK_EQ)

	case TK_PLUSPLUS, TK_MINUSMINUS
		x = insert_spaced_token(x, TK_TODO, NULL)
		x = skip(x)

	case TK_LTLT '' << -> shl
		x = replace_operator(x, KW_SHL, -1)

	case TK_LTLTEQ '' <<= -> shl=
		x = replace_operator(x, KW_SHL, TK_EQ)

	case TK_EQEQ '' == -> =
		x = replace_operator(x, TK_EQ, -1)

	case TK_GTGT '' >> -> shr
		x = replace_operator(x, KW_SHR, -1)

	case TK_GTGTEQ '' >>= -> shr=
		x = replace_operator(x, KW_SHR, TK_EQ)

	case TK_QUEST '' ?
		'' TODO: should turn this into iif(), but that's not easy
		x = insert_spaced_token(x, TK_TODO, "iif()")
		x = skip(x)

	case TK_CIRCUMFLEX
		x = replace_operator(x, KW_XOR, -1)

	case TK_CIRCUMFLEXEQ
		x = replace_operator(x, KW_XOR, TK_EQ)

	case TK_PIPE '' | -> or
		x = replace_operator(x, KW_OR, -1)

	case TK_PIPEEQ '' |= -> or=
		x = replace_operator(x, KW_OR, TK_EQ)

	case TK_AMPAMP, TK_PIPEPIPE
		'' || -> orelse | or
		'' && -> andalso | and

		dim as integer is_pp = (tk_mark(x) = MARK_PPEXPR)
		dim as integer kw = -1

		select case (tk_get(x))
		case TK_AMPAMP
			kw = iif(is_pp, KW_AND, KW_ANDALSO)
		case TK_PIPEPIPE
			kw = iif(is_pp, KW_OR, KW_ORELSE)
		end select

		ASSUMING(kw >= 0)

		x = replace_operator(x, kw, -1)

	case else
		x = skip(x)

	end select

	return x
end function

private sub fixup_operators()
	dim as integer x = skip(-1)
	while (tk_get(x) <> TK_EOF)
		select case (tk_mark(x))
		case MARK_TOPLEVEL, MARK_UNKNOWN, MARK_UNKNOWNENUMCONST
			x = skip(x)
		case else
			x = fixup_operator(x)
		end select
	wend
end sub

sub translate_toplevel()
	dim as integer x = skip(-1)

	while (tk_get(x) <> TK_EOF)
		select case as const (tk_mark(x))
		case MARK_PP
			if (tk_get(skip_pp(x)) = KW_PRAGMA) then
				'' Add TODO for #pragmas
				x = insert_todo(x, "#pragma")
			end if

			ASSUMING(tk_get(x) = TK_HASH)
			x = skip_pp(x)

			if (tk_get(x) = KW_ELIF) then
				'' #elif -> #elseif
				tk_remove(x, x)
				tk_insert(x, KW_ELSEIF, NULL)
			end if

			x = skip(skip_pp_directive(x) - 1)

		case MARK_EXTERN
			ASSUMING(tk_get(x) = KW_EXTERN)
			x = skip(x)
			ASSUMING(tk_get(x) = TK_STRING)
			x = skip(x)
			ASSUMING(tk_get(x) = TK_LBRACE)
			tk_remove(x, x)

			if (is_whitespace_until_eol(skiprev(x) + 1) = FALSE) then
				x = insert_statement_separator(x)
			end if

		case MARK_ENDEXTERN
			x = translate_compound_end(x, KW_EXTERN)

		case MARK_STRUCT
			x = translate_struct(x)

		case MARK_ENDENUM
			x = translate_compound_end(x, KW_ENUM)

		case MARK_ENDSTRUCT
			x = translate_compound_end(x, KW_TYPE)

		case MARK_ENDUNION
			x = translate_compound_end(x, KW_UNION)

		case MARK_ENUMCONST
			x = translate_enumconst(x)

		case MARK_TYPEDEF
			fixup_multdecl(skip(x), x)
			x = translate_decl(x, DECL_TYPEDEF)

		case MARK_TOPDECL
			dim as integer typebegin = x

			select case (tk_get(typebegin))
			case TK_ID
				if (frog_add_define(tk_text(typebegin), 0)) then
					typebegin = skip(typebegin)
				end if
			case KW_EXTERN, KW_STATIC
				typebegin = skip(typebegin)
			end select

			'' Split up combined var/proc declarations into separate declarations,
			'' as needed for FB, and mark them as vardecl/procdecl to let those
			'' translators finish the translation.
			fixup_multdecl(typebegin, x)

			'' Note: x doesn't advanced here, so that the /whole/
			'' former topdecl will be handled as vardecl/procdecl.

		case MARK_PROCDECL
			x = translate_decl(x, DECL_PROC)

		case MARK_VARDECL
			x = translate_decl(x, DECL_VAR)

		case MARK_FIELDDECL
			fixup_multdecl(x, x)
			x = translate_decl(x, DECL_FIELD)

		case MARK_UNKNOWN, MARK_UNKNOWNENUMCONST
			'' Insert a TODO at the begin of this statement.
			ASSUMING(tk_get(x) <> TK_EOL)

			x = insert_todo(x, "unknown construct")

			if (tk_mark(x) = MARK_UNKNOWNENUMCONST) then
				x = parse_enumconst(x, TRUE)
			else
				x = skip_statement(x)
			end if

		case else
			x = skip(x)

		end select
	wend

	fixup_comments()
	fixup_operators()
	fixup_eols()
end sub

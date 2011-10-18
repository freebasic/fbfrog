#include once "frog.bi"

type FrogFile
	as string h
	as string bi
end type

type FrogStuff
	'' Stats
	as ulongint inputsize  '' Sum of input file sizes, just for stats
	as integer inputtokens '' Count of input tokens

	as LinkedList files    '' FrogFile
end type

dim shared as FrogStuff frog

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

	'' Find the next ';' while skipping over those inside
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

private function parse_pp_directive(byval x as integer) as integer
	'' (Assuming all '#' are indicating a PP directive)
	if (tk_get(x) <> TK_HASH) then
		return x
	end if

	dim as integer begin = x
	x = skip_pp(x)

	'' Mark the expression parts of #if (but not #if itself) specially
	dim as integer mark = MARK_PP
	select case (tk_get(x))
	case KW_IF, KW_IFDEF, KW_IFNDEF, KW_ELIF
		mark = MARK_PPEXPR
	end select

	tk_set_mark(MARK_PP, begin, x)

	begin = x + 1
	x = skip_pp_directive(x)

	'' The last EOL is not part of the #directive, otherwise the EOL
	'' fixup would replace it with line continuation...
	tk_set_mark(mark, begin, x - 1)

	'' Make sure to return at a proper non-space token though,
	'' to avoid confusing the other parsers.
	return skip(x)
end function


sub parse_toplevel()
	'' Skip space at begin-of-file
	dim as integer x = skip(-1)

	do
		dim as integer old = x

		x = parse_pp_directive(x)
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
end sub

'' EOL fixup -- in C it's possible to have constructs split over multiple
'' lines, which requires a '_' line continuation char in FB. Also, the CPP
'' \<EOL> line continuation needs to be converted to FB.
sub fixup_eols()
	dim as integer x = 0
	do
		select case (tk_get(x))
		case TK_EOF
			exit do

		case TK_EOL
			select case (tk_mark(x))
			case MARK_TOPLEVEL, MARK_UNKNOWN
				'' (EOLs at toplevel are supposed to stay)

			case MARK_PP, MARK_PPEXPR
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
				ASSUMING(tk_get(x) = TK_BACKSLASH)
				tk_remove(x, x)
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

sub fixup_comments()
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
		x = insert_spaced_token(x, TK_TODO, _
			"FB's NOT has different precedence")
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

sub fixup_operators()
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

sub frog_work()
	dim as FrogFile ptr f = list_head(@frog.files)
	while (f)
		print "working on '" & f->h & "'"
		tk_init()
		tk_insert_file(0, f->h)

		parse_toplevel()
		translate_toplevel()
		fixup_comments()
		fixup_operators()
		fixup_eols()

		tk_emit_file(f->bi)
		tk_end()

		f = list_next(f)
	wend
end sub

sub frog_add_file(byref h as string, byref bi as string)
	dim as FrogFile ptr f = list_append(@frog.files)
	f->h = h
	f->bi = bi
end sub

sub frog_init()
	list_init(@frog.files, sizeof(FrogFile))
end sub

sub frog_end()
	dim as FrogFile ptr f = list_head(@frog.files)
	while (f)
		f->h = ""
		f->bi = ""
		f = list_next(f)
	wend
	list_end(@frog.files)
end sub

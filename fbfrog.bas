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
	print "bug: assertion failed at " & *filename & "(" & linenum & "):" & lcase(*funcname) & ": " & *test
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

'' Skips the token and any following whitespace
private function skip(byval x as integer) as integer
	do
		x += 1

		select case (tk_get(x))
		case TK_EOL, TK_SPACE, TK_COMMENT, TK_LINECOMMENT

		case else
			exit do
		end select
	loop
	return x
end function

'' Same, but backwards
private function skiprev(byval x as integer) as integer
	do
		x -= 1

		select case (tk_get(x))
		case TK_EOL, TK_SPACE, TK_COMMENT, TK_LINECOMMENT

		case else
			exit do
		end select
	loop
	return x
end function

private function is_whitespace_until_eol(byval x as integer) as integer
	do
		select case (tk_get(x))
		case TK_EOL, TK_EOF
			exit do

		case TK_SPACE, TK_COMMENT, TK_LINECOMMENT

		case else
			return FALSE

		end select

		x += 1
	loop
	return TRUE
end function

private function is_whitespace_since_eol(byval x as integer) as integer
	do
		select case (tk_get(x))
		case TK_EOL, TK_EOF
			exit do

		case TK_SPACE, TK_COMMENT, TK_LINECOMMENT

		case else
			return FALSE

		end select

		x -= 1
	loop
	return TRUE
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

	'' The last EOL is not part of the #directive, otherwise the EOL
	'' fixup would replace it with line continuation...
	tk_mark_stmt(STMT_PP, begin, x - 1)

	'' Make sure to return at a proper non-space token though,
	'' to avoid confusing the other parsers.
	return skip(x)
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
		x = skiprev(x)

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

private function find_parentheses(byval x as integer) as integer
	dim as integer opening_tk = tk_get(x)
	dim as integer closing_tk = any

	select case (opening_tk)
	case TK_LPAREN
		closing_tk = TK_RPAREN
	case TK_LBRACE
		closing_tk = TK_RBRACE
	case TK_LBRACKET
		closing_tk = TK_RBRACKET
	case else
		return x
	end select

	dim as integer level = 0
	dim as integer old = x
	do
		x = skip(x)

		select case (tk_get(x))
		case closing_tk
			if (level = 0) then
				'' Found it
				exit do
			end if
			level -= 1

		case opening_tk
			level += 1

		case TK_EOF
			'' Not in this file anyways
			return old

		end select
	loop

	return x
end function

private function parse_base_type(byval x as integer) as integer
	dim as integer old = x

	select case (tk_get(x))
	case KW_ENUM, KW_STRUCT, KW_UNION
		'' {ENUM | STRUCT | UNION} id
		x = skip(x)
		if (tk_get(x) <> TK_ID) then
			return old
		end if
		return skip(x)

	case TK_ID
		'' Just a single id
		return skip(x)
	end select

	'' [SIGNED | UNSIGNED]
	select case (tk_get(x))
	case KW_SIGNED, KW_UNSIGNED
		x = skip(x)
	end select

	'' [ VOID | CHAR | FLOAT | DOUBLE | INT
	'' | SHORT [INT]
	'' | LONG [LONG] [INT]
	'' ]
	select case (tk_get(x))
	case KW_VOID, KW_CHAR, KW_FLOAT, KW_DOUBLE, KW_INT
		x = skip(x)

	case KW_SHORT
		x = skip(x)

		'' [INT]
		if (tk_get(x) = KW_INT) then
			x = skip(x)
		end if

	case KW_LONG
		x = skip(x)

		'' [LONG]
		if (tk_get(x) = KW_LONG) then
			x = skip(x)
		end if

		'' [INT]
		if (tk_get(x) = KW_INT) then
			x = skip(x)
		end if

	end select

	'' In case of no type keyword at all, x = old
	return x
end function

private function parse_enumconst(byval x as integer) as integer
	'' id
	if (tk_get(x) <> TK_ID) then
		return x
	end if

	dim as integer begin = x
	x = skip(x)

	'' ['=' expression]
	if (tk_get(x) = TK_ASSIGN) then
		dim as integer level = 0
		do
			x = skip(x)

			select case (tk_get(x))
			case TK_LPAREN
				level += 1

			case TK_RPAREN
				level -= 1

			case TK_COMMA
				if (level = 0) then
					exit do
				end if

			case TK_RBRACE, TK_EOF, TK_HASH
				'' Note: '#' (PP directives) not allowed in
				'' expressions in FB, this can't be translated.
				exit do

			end select
		loop
	end if

	select case (tk_get(x))
	case TK_COMMA
		'' Treat the comma as part of the constant declaration
		x = skip(x)

	case TK_RBRACE

	case else
		return begin
	end select

	'' Mark the constant declaration
	tk_mark_stmt(STMT_ENUMCONST, begin, skiprev(x))

	return x
end function

private function parse_ptrs(byval x as integer) as integer
	'' Pointers: ('*')*
	while (tk_get(x) = TK_MUL)
		x = skip(x)
	wend
	return x
end function

private function parse_field(byval x as integer) as integer
	dim as integer begin = x

	'' type
	x = parse_base_type(begin)
	if (x = begin) then
		return begin
	end if

	'' '*'* identifier (',' '*'* identifier)*
	do
		x = parse_ptrs(x)

		if (tk_get(x) <> TK_ID) then
			return begin
		end if
		x = skip(x)

		if (tk_get(x) <> TK_COMMA) then
			exit do
		end if
		x = skip(x)
	loop

	'' ';'
	if (tk_get(x) <> TK_SEMI) then
		return begin
	end if
	x = skip(x)

	tk_mark_stmt(STMT_FIELD, begin, skiprev(x))

	return x
end function

private function parse_nested_struct_begin(byval x as integer) as integer
	'' {STRUCT|UNION} '{'
	select case (tk_get(x))
	case KW_STRUCT, KW_UNION

	case else
		return x
	end select

	dim as integer begin = x
	x = skip(x)

	'' '{'
	if (tk_get(x) <> TK_LBRACE) then
		return begin
	end if

	tk_mark_stmt(STMT_STRUCT, begin, x)

	return skip(x)
end function

private function parse_nested_struct_end _
	( _
		byval x as integer, _
		byval toplevelopening as integer _
	) as integer

	'' '}'
	if (tk_get(x) <> TK_RBRACE) then
		return x
	end if

	dim as integer begin = x

	'' Find the opening '{', to determine whether this is a struct
	'' or a union. Bail out if there is no matching nested struct/union
	'' begin.
	dim as integer opening = find_parentheses_backwards(x)
	if ((opening = x) or (opening <= toplevelopening)) then
		return begin
	end if
	opening = skiprev(opening)
	xassert(tk_stmt(opening) = STMT_STRUCT)

	'' '}'
	x = skip(x)

	'' ';'
	if (tk_get(x) <> TK_SEMI) then
		return begin
	end if

	tk_mark_stmt(iif((tk_get(opening) = KW_STRUCT), _
				STMT_ENDSTRUCT, STMT_ENDUNION), begin, x)

	return skip(x)
end function

private function parse_struct(byval x as integer) as integer
	'' [TYPEDEF] {STRUCT|UNION|ENUM} [id] '{' ... '}' [id] ';'
	dim as integer begin = x

	'' [TYPEDEF]
	if (tk_get(x) = KW_TYPEDEF) then
		x = skip(x)
	end if

	'' {STRUCT|UNION|ENUM}
	dim as integer compoundkw = tk_get(x)
	select case (compoundkw)
	case KW_ENUM, KW_STRUCT, KW_UNION

	case else
		return begin

	end select

	x = skip(x)

	'' [id]
	if (tk_get(x) = TK_ID) then
		x = skip(x)
	end if

	'' '{'
	if (tk_get(x) <> TK_LBRACE) then
		return begin
	end if

	tk_mark_stmt(STMT_STRUCT, begin, x)

	dim as integer toplevelopening = x
	x = skip(x)

	'' Body: Struct fields/enum constants, nested structs/unions,
	'' possibly intermixed with PP directives.
	dim as integer level = 0
	do
		dim as integer old = x

		if (compoundkw = KW_ENUM) then
			x = parse_enumconst(x)
		else
			x = parse_nested_struct_begin(x)
			if (x > old) then
				level += 1
			end if

			x = parse_field(x)
		end if

		'' '}'?
		select case (tk_get(x))
		case TK_RBRACE
			if (level > 0) then
				x = parse_nested_struct_end(x, toplevelopening)
				level -= 1
			else
				exit do
			end if
		case TK_EOF
			return begin
		end select

		x = parse_pp_directive(x)

		if (x = old) then
			'' Ok, there's something weird inside this struct/enum
			'' body that the PP directive/enumconst/field parsers
			'' didn't recognize. Ignore this token and try to
			'' continue parsing other fields etc.
			''return begin
			x = skip(x)
		end if
	loop

	'' '}'
	dim as integer structend = x
	xassert(tk_get(x) = TK_RBRACE)
	x = skip(x)

	'' [id]
	if (tk_get(x) = TK_ID) then
		x = skip(x)
	end if

	'' ';'
	if (tk_get(x) <> TK_SEMI) then
		return begin
	end if

	if (compoundkw = KW_ENUM) then
		compoundkw = STMT_ENDENUM
	elseif (compoundkw = KW_STRUCT) then
		compoundkw = STMT_ENDSTRUCT
	else
		compoundkw = STMT_ENDUNION
	end if

	tk_mark_stmt(compoundkw, structend, x)

	return skip(x)
end function

private function parse_extern_begin(byval x as integer) as integer
	'' EXTERN "C" '{'

	if (tk_get(x) <> KW_EXTERN) then
		return x
	end if

	dim as integer begin = x
	x = skip(x)

	'' "C"
	if (tk_get(x) <> TK_STRING) then
		return begin
	end if
	x = skip(x)

	'' '{'
	if (tk_get(x) <> TK_LBRACE) then
		return begin
	end if

	tk_mark_stmt(STMT_EXTERN, begin, x)

	'' EXTERN parsing is done here, so the content is parsed from the
	'' toplevel loop.
	return skip(x)
end function

private function parse_extern_end(byval x as integer) as integer
	'' '}'
	if (tk_get(x) <> TK_RBRACE) then
		return x
	end if

	'' Check whether this '}' belongs to an 'extern "C" {'
	dim as integer opening = find_parentheses_backwards(x)
	if (opening = x) then
		return x
	end if

	dim as integer stmt = tk_stmt(opening)
	if (stmt <> STMT_EXTERN) then
		return x
	end if

	tk_mark_stmt(STMT_ENDEXTERN, x, x)

	return skip(x)
end function

private function parse_typedef(byval x as integer) as integer
	if (tk_get(x) <> KW_TYPEDEF) then
		return x
	end if

	'' TYPEDEF
	dim as integer begin = x
	x = skip(x)

	'' type
	dim as integer typebegin = x
	x = parse_base_type(x)
	if (x = typebegin) then
		return begin
	end if

	x = parse_ptrs(x)

	'' id
	if (tk_get(x) <> TK_ID) then
		return begin
	end if
	x = skip(x)

	'' ';'
	if (tk_get(x) <> TK_SEMI) then
		return begin
	end if

	tk_mark_stmt(STMT_TYPEDEF, begin, x)

	return skip(x)
end function

private function parse_procdecl(byval x as integer) as integer
	dim as integer begin = x

	'' type
	x = parse_base_type(x)
	if (x = begin) then
		return begin
	end if

	x = parse_ptrs(x)

	'' id
	if (tk_get(x) <> TK_ID) then
		return begin
	end if
	x = skip(x)

	'' '('
	if (tk_get(x) <> TK_LPAREN) then
		return begin
	end if
	x = skip(x)

	'' Parameter list
	'' [ type [id]  (',' type [id] )*  [ ',' '...' ] ]
	'' Note: The following isn't even doing that much syntax verification
	'' at all, but it's ok, it's not a compiler afterall.
	do
		select case (tk_get(x))
		case TK_RPAREN
			exit do

		case TK_EOF
			return begin

		case TK_COMMA, TK_ELLIPSIS
			'' Let ',' and '...' pass
			x = skip(x)

		case else
			'' type
			dim as integer parambegin = x
			x = parse_base_type(x)
			if (x = parambegin) then
				return begin
			end if

			x = parse_ptrs(x)

			'' [id]
			if (tk_get(x) = TK_ID) then
				x = skip(x)
			end if

		end select
	loop

	'' ')'
	xassert(tk_get(x) = TK_RPAREN)
	x = skip(x)

	'' ';'
	if (tk_get(x) <> TK_SEMI) then
		return begin
	end if

	'' The whole thing must be marked, so that any EOLs in between can
	'' be detected and removed...
	tk_mark_stmt(STMT_PROCDECL, begin, x)

	return skip(x)
end function

private sub parse_toplevel()
	dim as integer x = 0
	do
		dim as integer old = x

		x = parse_pp_directive(x)
		x = parse_struct(x)
		x = parse_extern_begin(x)
		x = parse_extern_end(x)
		x = parse_typedef(x)
		x = parse_procdecl(x)

		if (x = old) then
			'' Token/construct couldn't be identified, so make
			'' sure the parsing advances somehow, but mark as
			'' nothing to clean up failed parses.
			x = skip(x)
			tk_mark_stmt(STMT_TOPLEVEL, old, x - 1)
		end if
	loop while (tk_get(x) <> TK_EOF)
end sub

private function remove_following_lbrace(byval x as integer) as integer
	while (tk_get(x) <> TK_LBRACE)
		x += 1
	wend
	tk_remove(x)
	return x
end function

private function insert_statement_separator(byval x as integer) as integer
	'' Only if there's not already another ':'
	if ((tk_get(x) = TK_COLON) or (tk_get(skiprev(x)) = TK_COLON)) then
		return x
	end if

	if (tk_get(x - 1) <> TK_SPACE) then
		tk_insert_space(x)
		x += 1
	end if

	tk_insert(x, TK_COLON, NULL)
	x += 1

	if (tk_get(x) <> TK_SPACE) then
		tk_insert_space(x)
		x += 1
	end if

	return x
end function

private function translate_compound_end _
	( _
		byval x as integer, _
		byval compoundkw as integer _
	) as integer

	'' '}' [id] [';']    ->    END {EXTERN | TYPE | ENUM}
	'' The id can only appear because of <typedef struct { } id;> which
	'' the struct parser/translator accepts and handles. The id might be
	'' copied, but only here is it removed.
	'' Note: ';' behind an EXTERN block isn't treated as part of it.

	if (is_whitespace_since_eol(x - 1) = FALSE) then
		x = insert_statement_separator(x)
	end if

	tk_insert(x, KW_END, NULL)
	x += 1
	tk_insert_space(x)
	x += 1
	tk_insert(x, compoundkw, NULL)
	x += 1

	'' '}'
	dim as integer y = x
	xassert(tk_get(y) = TK_RBRACE)
	y = skip(y)

	if (compoundkw <> KW_EXTERN) then
		'' [id]
		if (tk_get(y) = TK_ID) then
			y = skip(y)
		end if

		'' [';']
		if (tk_get(y) = TK_SEMI) then
			y = skip(y)
		end if
	end if

	'' Remove the '}' and ';', including space and the optional id in
	'' between. If there is no EOL coming afterwards, insert a ':'
	'' statement separator.
	tk_remove_range(x, skiprev(y))
	if (is_whitespace_until_eol(x) = FALSE) then
		x = insert_statement_separator(x)
	end if

	return x
end function

private function translate_enumconst(byval x as integer) as integer
	'' identifer ['=' expression] [',']

	xassert(tk_get(x) = TK_ID)

	dim as integer ends_at_eol = FALSE
	dim as integer level = 0
	do
		x = skip(x)

		select case (tk_get(x))
		case TK_LPAREN
			level += 1

		case TK_RPAREN
			level -= 1

		case TK_COMMA
			if (level = 0) then
				'' Remove the comma, unless there are more
				'' constants coming in this line.
				if (is_whitespace_until_eol(x + 1)) then
					tk_remove(x)
					x -= 1
				end if
				x = skip(x)
				exit do
			end if

		case TK_RBRACE
			exit do

		end select

		xassert(tk_get(x) <> TK_EOF)
	loop

	return x
end function

private sub split_field_if_needed(byval x as integer)
	'' Split up a field if needed:
	'' Scan the whole field (from begin to ';'). If there are commas and
	'' pointers, then splits need to be made at the proper places, to
	'' bring the whole thing into a state allowing it to be translated to
	'' FB. The offsets of fields mustn't change of course.
	''    int *a, b, c, **d, **e, f;
	'' to:
	''    int *a; int b, c; int **d; int **e; int f;
	'' Declarations with pointers need to be split off, while non-pointer
	'' declarations and declarations with the same number of pointers can
	'' stay together (since they /can/ be translated to FB).

	dim as integer typebegin = x
	dim as integer typeend = parse_base_type(typebegin)
	x = typeend
	'' The type parser skips to the next non-type token,
	'' so to get the real typeend we need to skip back
	typeend = skiprev(typeend)


	'' Current ptrcount > 0 but <> next ptrcount?
	''  --> Replace following comma by semi, dup the type.
	'' As soon as a next decl is seen and it's different, split.

	'' Amount of pointers seen for the current sequence of declarations.
	'' As soon as a declaration with different ptrcount is seen,
	'' we know it's time to split.
	dim as integer ptrcount = 0     '' Active counter
	dim as integer declptrcount = 0 '' Amount for previous declaration(s)

	'' Begin of the current sequence of declarations with equal ptrcount,
	'' so we can go back and do the split there.
	dim as integer declbegin = typebegin

	do
		select case (tk_get(x))
		case TK_MUL
			ptrcount += 1

		case TK_COMMA, TK_SEMI
			'' Comma/semicolon terminate a declaration (not
			'' officially probably, hehe) and we need to decide
			'' whether to continue the current sequence (of decls
			'' with equal ptrcount) or split it off and start a new
			'' sequence, beginning with this last (just terminated)
			'' decl.
			if (ptrcount <> declptrcount) then
				'' Only split if this isn't the first decl,
				'' in which case there is no current sequence
				'' to split off...
				if (declbegin <> typebegin) then
					'' Replace ',' with ';'
					tk_replace(declbegin, TK_SEMI, NULL)

					'' Copy in the type
					tk_copy_range(declbegin + 1, typebegin, typeend)

					'' Take into account the added tokens
					x += typeend - typebegin + 1
				end if

				'' Continue with new sequence
				declptrcount = ptrcount
			end if

			if (tk_get(x) = TK_SEMI) then
				exit do
			end if

			'' Remember this for later -- it's where the split
			'' might need to be done.
			declbegin = x
			ptrcount = 0

		end select

		x = skip(x)
	loop
end sub

private sub remove_unnecessary_ptrs(byval x as integer)
	'' Assuming field declarations with multiple identifiers but different
	'' ptrcounts on some of them have been split up already, we will only
	'' encounter fields of these forms here:
	''    int a;
	''    int *a;
	''    int a, b, c;
	''    int *a, *b, *c;
	'' i.e. all identifiers with equal ptrcounts.
	'' This translation step turns this:
	''    int *a, *b, *c;
	'' into:
	''    int *a, b, c;
	'' which is needed to get to this:
	''    as integer ptr a, b, c

	x = parse_base_type(x)

	'' Just remove all ptrs behind commas. Also remove unnecessary space.
	dim as integer have_comma = FALSE
	do
		select case (tk_get(x))
		case TK_MUL
			if (have_comma) then
				tk_remove(x)
				x -= 1
			end if

		case TK_COMMA
			have_comma = TRUE

		case TK_SEMI
			exit do
		end select

		x += 1
	loop
end sub

private function translate_base_type(byval x as integer) as integer
	''    int             ->      as integer
	''    unsigned int    ->      as uinteger
	''    struct T        ->      as T
	'' etc.

	'' Insert the AS
	tk_insert(x, KW_AS, NULL)
	x += 1
	tk_insert_space(x)
	x += 1

	select case (tk_get(x))
	case KW_ENUM, KW_STRUCT, KW_UNION
		'' {ENUM | STRUCT | UNION} id
		tk_remove_range(x, skip(x) - 1)

		xassert(tk_get(x) = TK_ID)
		return skip(x)

	case TK_ID
		'' Just a single id
		return skip(x)
	end select

	'' [SIGNED | UNSIGNED]
	'' [ VOID | CHAR | FLOAT | DOUBLE | INT
	'' | SHORT [INT]
	'' | LONG [LONG] [INT]
	'' ]
	''
	'' 1) Remember position of [UN]SIGNED
	'' 2) Parse base type (void/char/int/...)
	'' 3) If base type found, remove the [UN]SIGNED, otherwise translate it
	''    as [U]INTEGER

	dim as integer sign = x
	dim as integer signed = TRUE
	dim as integer basekw = -1

	select case (tk_get(sign))
	case KW_SIGNED
		x = skip(x)
	case KW_UNSIGNED
		signed = FALSE
		x = skip(x)
	end select

	select case (tk_get(x))
	case KW_VOID
		basekw = KW_ANY

	case KW_CHAR
		basekw = iif(signed, KW_BYTE, KW_UBYTE)

	case KW_FLOAT
		basekw = KW_SINGLE

	case KW_DOUBLE
		basekw = KW_DOUBLE

	case KW_INT
		basekw = iif(signed, KW_INTEGER, KW_UINTEGER)

	case KW_SHORT
		basekw = iif(signed, KW_SHORT, KW_USHORT)

		'' [INT]
		if (tk_get(skip(x)) = KW_INT) then
			tk_remove_range(x + 1, skip(x))
		end if

	case KW_LONG
		basekw = iif(signed, KW_LONG, KW_ULONG)

		'' [LONG]
		if (tk_get(skip(x)) = KW_LONG) then
			basekw = iif(signed, KW_LONGINT, KW_ULONGINT)
			tk_remove_range(x + 1, skip(x))
		end if

		'' [INT]
		if (tk_get(skip(x)) = KW_INT) then
			tk_remove_range(x + 1, skip(x))
		end if

	end select

	if (basekw >= 0) then
		tk_replace(x, basekw, NULL)
		x = skip(x)
	end if

	select case (tk_get(sign))
	case KW_SIGNED, KW_UNSIGNED
		if (basekw >= 0) then
			'' Remove the [UN]SIGNED, it's now encoded into the
			'' FB type (e.g. UINTEGER)
			dim as integer last = skip(sign) - 1
			tk_remove_range(sign, last)
			x -= (last - sign + 1)
		else
			'' Found [UN]SIGNED only, treat it as [U]INTEGER
			tk_replace(sign, iif(signed, KW_INTEGER, KW_UINTEGER), NULL)
		end if
	end select

	return x
end function

private function translate_ptrs(byval x as integer) as integer
	'' Pointers: '*' -> PTR, plus some space if needed
	while (tk_get(x) = TK_MUL)
		if ((skiprev(x) + 1) = x) then
			tk_insert_space(x - 1)
		end if
		tk_replace(x, KW_PTR, NULL)
		if ((skip(x) - 1) = x) then
			tk_insert_space(x + 1)
		end if
		x = skip(x)
	wend
	return x
end function

private function translate_field(byval x as integer) as integer
	'' The field parser accepts all possible variations, now here we need
	'' an ok way to translate them.
	''
	'' int *i;              ->      as integer ptr i
	'' int a, b, c;         ->      as integer a, b, c
	'' Simple: insert an AS, translate type and pointers, remove the ';'.
	''
	'' int *a, **b, c;      ->      int *a; int **b; int c;
	'' Commas + pointers can't be translated to FB 1:1, but we can split it
	'' up into separate declarations by replacing ',' by ';' and duplicating
	'' the type. Then the normal simple translation rules can work.

	split_field_if_needed(x)
	remove_unnecessary_ptrs(x)

	x = translate_base_type(x)
	x = translate_ptrs(x)

	'' identifier (',' identifier)*
	do
		'' identifier
		xassert(tk_get(x) = TK_ID)
		x = skip(x)

		'' ','
		if (tk_get(x) <> TK_COMMA) then
			exit do
		end if
		x = skip(x)
	loop

	'' ';'
	'' Remove it, and if there is no EOL following,
	'' insert a ':' statement separator.
	xassert(tk_get(x) = TK_SEMI)
	tk_remove(x)
	if (is_whitespace_until_eol(x) = FALSE) then
		x = insert_statement_separator(x)
	end if

	return x
end function

private function translate_struct(byval x as integer) as integer
	'' structs/enums
	''
	'' struct T { ... };               ->    type T : ... : end type
	'' Note: any occurences of <struct T> will be translated to just <T>
	'' by the type translator, so this just works.
	''
	'' typedef struct { ... } TT;      ->    type TT : ... : end type
	''
	'' typedef struct T { ... } TT;    ->    type T : ... : end type
	''                                       type TT as T
	'' Both identifiers might be needed.
	''
	'' typedef struct A { ... } A;     ->    type A : ... : end type
	''
	'' All in all this means:
	'' If the struct has an id, then use it for the type. If it has no
	'' id, then move the one at the end of the declaration to the front.
	'' If there is none there either, insert a TODO instead.
	'' If both struct and typedef have an identifier, but they are
	'' different, then insert a <type typedef-id as struct-id> after the
	'' struct end.
	''
	'' Same for enums. And this also handles unions and nested structs...
	'' 

	'' [TYPEDEF]
	dim as integer is_typedef = FALSE
	if (tk_get(x) = KW_TYPEDEF) then
		tk_remove_range(x, skip(x) - 1)
		is_typedef = TRUE
	end if

	dim as integer compoundkw = tk_get(x)
	if (compoundkw = KW_STRUCT) then
		'' STRUCT -> TYPE
		tk_replace(x, KW_TYPE, NULL)
	else
		xassert((compoundkw = KW_ENUM) or (compoundkw = KW_UNION))
	end if

	x = skip(x)

	'' ['id']
	dim as integer structid = x
	if (tk_get(x) = TK_ID) then
		x = skip(x)
	end if

	'' '{'
	xassert(tk_get(x) = TK_LBRACE)

	if (is_typedef) then
		'' Find the '}' and the following typedef id (if it's there)
		dim as integer typedefid = find_parentheses(x)

		if (compoundkw = KW_ENUM) then
			xassert(tk_stmt(typedefid) = STMT_ENDENUM)
		elseif (compoundkw = KW_STRUCT) then
			xassert(tk_stmt(typedefid) = STMT_ENDSTRUCT)
		else
			xassert(tk_stmt(typedefid) = STMT_ENDUNION)
		end if

		xassert(tk_get(typedefid) = TK_RBRACE)

		typedefid = skip(typedefid)

		if (tk_get(structid) = TK_ID) then
			'' There is a struct-id. If the typedef has the same
			'' id, it can be ignored. If the typedef has a
			'' different id, we need to insert an FB typedef
			'' after the endstruct. If the typedef has no id
			'' at all, that's bad C, but we don't care.
			if (tk_get(typedefid) = TK_ID) then
				if (*tk_text(structid) <> *tk_text(typedefid)) then
					dim as integer y = skip(typedefid)
					xassert(tk_get(y) = TK_SEMI)

					'' Fake a typedef (building it up in
					'' reverse, so y doesn't need to be
					'' updated)
					y += 1
					tk_insert(y, TK_SEMI, NULL)
					tk_copy(y, typedefid)
					tk_insert_space(y)
					tk_copy(y, structid)
					tk_insert_space(y)
					tk_insert(y, compoundkw, NULL)
					tk_insert_space(y)
					tk_insert(y, KW_TYPEDEF, NULL)
					tk_mark_stmt(STMT_TYPEDEF, y, y + 7)
				end if
			end if
		else
			'' Use the typedef-id for the struct,
			'' or insert a TODO if there is no typedef-id.
			if (tk_get(typedefid) = TK_ID) then
				tk_copy(structid, typedefid)
			else
				tk_insert(structid, TK_TODO, "missing identifier")
			end if
			'' If needed, insert a space to separate the STRUCT
			'' from the identifier.
			if (tk_get(structid - 1) <> TK_SPACE) then
				tk_insert_space(structid)
				x += 1
			end if
			x += 1
		end if
	end if

	'' Remove the '{' and space in front of it. If there is an EOL
	'' following, insert a ':' statement separator.
	xassert(tk_get(x) = TK_LBRACE)
	dim as integer spacebegin = skiprev(x) + 1
	tk_remove_range(spacebegin, x)
	x -= x - spacebegin

	if (is_whitespace_until_eol(x) = FALSE) then
		x = insert_statement_separator(x)
	end if

	return x
end function

private function translate_typedef(byval x as integer) as integer
	'' typedef T id;    ->    type id as T

	dim as integer begin = x

	'' TYPEDEF -> TYPE
	xassert(tk_get(x) = KW_TYPEDEF)
	tk_replace(x, KW_TYPE, NULL)
	x = skip(x)

	'' Translate the type + pointers
	dim as integer typebegin = x
	x = translate_base_type(x)
	x = translate_ptrs(x)

	'' Copy the id to the front, in between the TYPE and the type,
	'' and also insert a single space to separate the identifier from
	'' the type's AS.
	xassert(tk_get(x) = TK_ID)
	tk_insert_space(typebegin)
	x += 1
	tk_copy(typebegin, x)

	'' Remove the old id and space in front of it.
	tk_remove_range(x, skip(x - 1))

	x = skip(x - 1)

	xassert(tk_get(x) = TK_SEMI)
	tk_remove(x)
	if (is_whitespace_until_eol(x) = FALSE) then
		x = insert_statement_separator(x)
	end if

	return x
end function

private function translate_procdecl(byval x as integer) as integer
	''    type id '(' params ')' ';'
	'' ->
	''    DECLARE {SUB | FUNCTION} id '(' params ')' [AS type]
	''
	'' params:
	''    type '*'* id          ->         BYVAL id AS type PTR*
	''

	'' DECLARE
	tk_insert(x, KW_DECLARE, NULL)
	x += 1
	tk_insert_space(x)
	x += 1

	'' VOID only?
	dim as integer is_sub = _
			((tk_get(x) = KW_VOID) and (tk_get(skip(x)) = TK_ID))

	'' SUB | FUNCTION
	tk_insert(x, iif(is_sub, KW_SUB, KW_FUNCTION), NULL)
	x += 1
	tk_insert_space(x)
	x += 1

	if (is_sub) then
		'' Remove the VOID and space behind it
		xassert(tk_get(x) = KW_VOID)
		tk_remove_range(x, skip(x) - 1)
	else
		'' type + pointers
		dim as integer typebegin = x
		dim as integer typeend = translate_base_type(typebegin)
		xassert(typeend > typebegin)
		dim as integer y = translate_ptrs(typeend)
		typeend = skiprev(y)

		'' id
		xassert(tk_get(y) = TK_ID)
		y = skip(y)

		'' '('
		xassert(tk_get(y) = TK_LPAREN)

		'' ')'
		y = find_parentheses(y)
		y += 1

		tk_insert_space(y)
		y += 1

		'' Copy the translated type/ptrs behind the ')'
		tk_copy_range(y, typebegin, typeend)

		'' Remove the old type and space behind it
		tk_remove_range(typebegin, skip(typeend) - 1)
	end if

	'' id
	x = skip(x - 1)
	xassert(tk_get(x) = TK_ID)
	x = skip(x)

	'' '('
	xassert(tk_get(x) = TK_LPAREN)
	x = skip(x)

	'' Parameter list
	'' [ type id  (',' type id )*  [ ',' '...' ] ]
	do
		select case (tk_get(x))
		case TK_RPAREN, TK_EOF
			exit do

		case TK_COMMA, TK_ELLIPSIS
			'' Let ',' and '...' pass
			x = skip(x)

		case else
			'' BYVAL
			tk_insert(x, KW_BYVAL, NULL)
			x += 1
			tk_insert_space(x)
			x += 1

			'' type + pointers
			dim as integer typebegin = x
			dim as integer typeend = translate_base_type(typebegin)
			xassert(typeend > typebegin)
			x = translate_ptrs(typeend)
			typeend = skiprev(x)

			'' [id]
			if (tk_get(x) = TK_ID) then
				x += 1
				tk_insert_space(x)
				x += 1
			end if

			'' Copy the translated type/ptrs behind the id
			tk_copy_range(x, typebegin, typeend)

			'' Skip it
			x += (typeend - typebegin + 1)

			xassert((tk_get(x) = TK_COMMA) or (tk_get(x) = TK_RPAREN))

			'' Remove the old type and following space
			typeend = skip(typeend) - 1
			tk_remove_range(typebegin, typeend)
			x -= (typeend - typebegin + 1)

		end select
	loop

	'' ')'
	xassert(tk_get(x) = TK_RPAREN)
	x = skip(x)

	if (is_sub = FALSE) then
		'' Skip over the function type copied here before...
		while (tk_get(x) <> TK_SEMI)
			xassert(tk_get(x) <> TK_EOF)
			x = skip(x)
		wend
	end if

	'' ';'
	xassert(tk_get(x) = TK_SEMI)
	tk_remove(x)
	if (is_whitespace_until_eol(x) = FALSE) then
		x = insert_statement_separator(x)
	end if

	return x
end function

private sub translate_toplevel()
	dim as integer x = 0
	while (tk_get(x) <> TK_EOF)
		select case as const (tk_stmt(x))
		case STMT_EXTERN
			'' Just jump to the '{' and remove it
			x = remove_following_lbrace(x)

		case STMT_ENDEXTERN
			x = translate_compound_end(x, KW_EXTERN)

		case STMT_STRUCT
			x = translate_struct(x)

		case STMT_ENDENUM
			x = translate_compound_end(x, KW_ENUM)

		case STMT_ENDSTRUCT
			x = translate_compound_end(x, KW_TYPE)

		case STMT_ENDUNION
			x = translate_compound_end(x, KW_UNION)

		case STMT_ENUMCONST
			x = translate_enumconst(x)

		case STMT_FIELD
			x = translate_field(x)

		case STMT_TYPEDEF
			x = translate_typedef(x)

		case STMT_PROCDECL
			x = translate_procdecl(x)

		case else
			x = skip(x)

		end select
	wend
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
				tk_replace(x, TK_UNDERSCORE, NULL)

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

			tk_in_file(hfile)

			parse_toplevel()

			translate_toplevel()
			fixup_eols()

			bifile = path_strip_ext(hfile) & ".bi"
			tk_emit_file(bifile)
		end if
	next

	tk_end()
	end 0

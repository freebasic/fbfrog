#include once "fbfrog.bi"

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
		case TK_EOL, TK_SPACE, TK_COMMENT, TK_LINECOMMENT

		case else
			exit do
		end select
	loop
	return x
end function

private function skip(byval x as integer) as integer
	return skip_(x, 1)
end function

private function skiprev(byval x as integer) as integer
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
		case TK_SPACE, TK_COMMENT, TK_LINECOMMENT

		case else
			exit do
		end select
	loop

	return x
end function

private function skip_unless_eol(byval x as integer) as integer
	return skip_unless_eol_(x, 1)
end function

private function skiprev_unless_eol(byval x as integer) as integer
	return skip_unless_eol_(x, -1)
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

private function insert_spaced_token _
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

private function insert_statement_separator(byval x as integer) as integer
	'' Only if there's not already another ':'
	if ((tk_get(x) = TK_COLON) or (tk_get(skiprev(x)) = TK_COLON)) then
		return x
	end if
	return insert_spaced_token(x, TK_COLON, NULL)
end function

private sub remove_this_and_space(byval x as integer)
	tk_remove(x, skip(x) - 1)
end sub

private function find_token(byval x as integer, byval tk as integer) as integer
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

private function find_parentheses _
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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' Parsing

private function parse_unknown(byval x as integer) as integer
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

enum
	DECL_PARAM = 0
	DECL_TOP
	DECL_TYPEDEF
	DECL_TYPEDEFSTRUCTBLOCK
	DECL_VAR
	DECL_FIELD
	DECL_PROC
end enum

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
	tk_set_mark(MARK_PP, begin, x - 1)

	'' Make sure to return at a proper non-space token though,
	'' to avoid confusing the other parsers.
	return skip(x)
end function

private function parse_enumconst _
	( _
		byval x as integer, _
		byval is_unknown as integer _
	) as integer

	dim as integer begin = x
	dim as integer skip_expression = is_unknown

	if (is_unknown = FALSE) then
		'' id
		if (tk_get(x) <> TK_ID) then
			return begin
		end if
		x = skip(x)

		'' ['=']
		if (tk_get(x) = TK_EQ) then
			skip_expression = TRUE
			x = skip(x)
		end if
	end if

	if (skip_expression) then
		dim as integer level = 0
		do
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

			x = skip(x)
		loop
	end if

	select case (tk_get(x))
	case TK_COMMA
		'' Treat the comma as part of the constant declaration
		x = skip(x)

	case TK_RBRACE

	case else
		if (is_unknown = FALSE) then
			return begin
		end if

	end select

	'' Mark the constant declaration
	tk_set_mark(iif(is_unknown, MARK_UNKNOWNENUMCONST, MARK_ENUMCONST), _
			begin, skiprev(x))

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

private function parse_ptrs(byval x as integer) as integer
	'' Pointers: ('*')*
	while (tk_get(x) = TK_STAR)
		x = skip(x)
	wend
	return x
end function

private function parse_multdecl _
	( _
		byval x as integer, _
		byval begin as integer, _
		byval decl as integer _
	) as integer

	'' Generic 'type *a, **b;' parsing,
	'' used for vardecls/fielddecls/procdecls/params...
	''
	'' type '*'* var (',' '*'* var)* ';'
	''
	'' Where var can be:
	'' a plain id: var = id
	'' a procptr:  var = '(' '*'+ id ')' '(' params ')'
	'' a procdecl: var = id '(' params ')'

	'' No type hack for the typedef-to-struct-block parser:
	'' its type is the struct block, which it already parsed...
	if (decl <> DECL_TYPEDEFSTRUCTBLOCK) then
		'' type
		dim as integer typebegin = x
		x = parse_base_type(x)
		if (x = typebegin) then
			return begin
		end if
	end if

	'' var (',' var)*
	do
		'' '*'*
		x = parse_ptrs(x)

		'' '('?
		dim as integer is_procptr = FALSE
		if (tk_get(x) = TK_LPAREN) then
			is_procptr = TRUE
			x = skip(x)

			'' '*'
			if (tk_get(x) <> TK_STAR) then
				return begin
			end if
			x = skip(x)
		end if

		'' id (must be there except for params)
		if (tk_get(x) = TK_ID) then
			x = skip(x)
		elseif (decl <> DECL_PARAM) then
			return begin
		end if

		if (is_procptr) then
			'' ')'
			if (tk_get(x) <> TK_RPAREN) then
				return begin
			end if
			x = skip(x)
		end if

		'' Check for '(' params ')'
		if (tk_get(x) = TK_LPAREN) then
			'' Note: typedef to procdecl can't be translated,
			'' so it's disallowed here. If there are procdecls
			'' in fields, then fine, that's C++ stuff, but oh well.
			'' Note: procptrs always have params.
			if ((is_procptr = FALSE) and _
			    ((decl = DECL_TYPEDEF) or _
			     (decl = DECL_TYPEDEFSTRUCTBLOCK))) then
				return begin
			end if

			'' '('
			x = skip(x)

			'' Just '(void)'?
			if ((tk_get(x) = KW_VOID) and (tk_get(skip(x)) = TK_RPAREN)) then
				'' VOID
				x = skip(x)
			else
				'' [ type [id]  (',' type [id] )*  [ ',' '...' ] ]
				'' Note: The following isn't even doing that much
				'' syntax verification at all, but it's ok, it's not
				'' a compiler afterall.
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
						x = parse_multdecl(x, x, DECL_PARAM)

					end select
				loop
			end if

			'' ')'
			if (tk_get(x) <> TK_RPAREN) then
				return begin
			end if
			x = skip(x)
		end if

		'' Everything can have a comma and more identifiers,
		'' except for params.
		if ((decl = DECL_PARAM) or (tk_get(x) <> TK_COMMA)) then
			exit do
		end if
		x = skip(x)
	loop

	select case (decl)
	case DECL_FIELD, DECL_TYPEDEF, DECL_TOP
		'' ';'
		if (tk_get(x) <> TK_SEMI) then
			return begin
		end if

		dim as integer mark = MARK_TOPDECL
		select case (decl)
		case DECL_FIELD
			mark = MARK_FIELDDECL
		case DECL_TYPEDEF
			mark = MARK_TYPEDEF
		end select

		tk_set_mark(mark, begin, x)
		x = skip(x)

	end select

	return x
end function

private function parse_topdecl_or_typedef(byval x as integer) as integer
	'' Toplevel declarations: global variables (including procedure
	'' pointers), procedure declarations, and also typedefs, since they
	'' are almost the same (parse_multdecl() disallows typedefs to
	'' procdecls, because those aren't possible FB).
	''    int a, *b, (*c)(), f(), *g();
	''    ...
	''
	'' [TYPEDEF|EXTERN|STATIC] multdecl

	dim as integer begin = x
	dim as integer decl = DECL_TOP

	select case (tk_get(x))
	case KW_EXTERN, KW_STATIC
		x = skip(x)

	case KW_TYPEDEF
		decl = DECL_TYPEDEF
		x = skip(x)

	end select

	dim as integer multdecl = x
	x = parse_multdecl(x, begin, decl)
	if (x = multdecl) then
		return begin
	end if

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

	tk_set_mark(MARK_STRUCT, begin, x)

	return skip(x)
end function

private function parse_nested_struct_end _
	( _
		byval x as integer, _
		byval toplevelopening as integer _
	) as integer

	dim as integer begin = x

	'' '}'
	if (tk_get(x) <> TK_RBRACE) then
		return begin
	end if

	'' Find the opening '{', to determine whether this is a struct
	'' or a union. Bail out if there is no matching nested struct/union
	'' begin.
	dim as integer opening = find_parentheses(x, TRUE)
	if ((opening = x) or (opening <= toplevelopening)) then
		return begin
	end if
	opening = skiprev(opening)
	ASSUMING(tk_mark(opening) = MARK_STRUCT)

	'' '}'
	ASSUMING(tk_get(x) = TK_RBRACE)
	x = skip(x)

	'' [id]
	if (tk_get(x) = TK_ID) then
		x = skip(x)
	end if

	'' ';'
	if (tk_get(x) <> TK_SEMI) then
		return begin
	end if

	tk_set_mark(iif((tk_get(opening) = KW_STRUCT), _
				MARK_ENDSTRUCT, MARK_ENDUNION), begin, x)

	return skip(x)
end function

private function parse_struct(byval x as integer) as integer
	'' [TYPEDEF] {STRUCT|UNION|ENUM} [id] '{'
	'' ...
	'' '}' [no-type-multdecl] ';'

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

	tk_set_mark(MARK_STRUCT, begin, x)

	dim as integer toplevelopening = x
	x = skip(x)

	'' Body: Struct fields/enum constants, nested structs/unions,
	'' possibly intermixed with PP directives.
	dim as integer level = 0
	do
		dim as integer old = x

		if (compoundkw = KW_ENUM) then
			x = parse_enumconst(x, FALSE)
		else
			x = parse_nested_struct_begin(x)
			if (x > old) then
				level += 1
			end if

			x = parse_multdecl(x, x, DECL_FIELD)
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
			'' didn't recognize. Ignore this and try to parse
			'' other fields etc.
			''return begin
			if (compoundkw = KW_ENUM) then
				x = parse_enumconst(x, TRUE)
			else
				x = parse_unknown(x)
			end if
		end if
	loop

	'' '}'
	dim as integer structend = x
	ASSUMING(tk_get(x) = TK_RBRACE)
	x = skip(x)

	'' If this was a typedef-to-struct-block, there will be a multdecl
	'' following now...
	if (tk_get(begin) = KW_TYPEDEF) then
		x = parse_multdecl(x, begin, DECL_TYPEDEFSTRUCTBLOCK)
	end if

	'' ';'
	if (tk_get(x) <> TK_SEMI) then
		return begin
	end if

	if (compoundkw = KW_ENUM) then
		compoundkw = MARK_ENDENUM
	elseif (compoundkw = KW_STRUCT) then
		compoundkw = MARK_ENDSTRUCT
	else
		compoundkw = MARK_ENDUNION
	end if

	tk_set_mark(compoundkw, structend, x)

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

	tk_set_mark(MARK_EXTERN, begin, x)

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
	dim as integer opening = find_parentheses(x, TRUE)
	if (opening = x) then
		return x
	end if

	dim as integer mark = tk_mark(opening)
	if (mark <> MARK_EXTERN) then
		return x
	end if

	tk_set_mark(MARK_ENDEXTERN, x, x)

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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' Translating

private function translate_enumconst(byval x as integer) as integer
	'' identifer ['=' expression] [',']

	ASSUMING(tk_get(x) = TK_ID)

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
				exit do
			end if

		case TK_RBRACE
			exit do

		case TK_EOF
			ASSUMING(FALSE)
			exit do

		end select
	loop

	if (tk_get(x) = TK_COMMA) then
		'' Beautification: Remove the comma if there is only EOL or
		'' '}' (end of enum) coming.
		if (is_whitespace_until_eol(x + 1) or _
		    (tk_get(skip(x)) = TK_RBRACE)) then
			tk_remove(x, x)
			x -= 1
		end if

		'' Either way, skip to the next token after the comma
		x = skip(x)
	end if

	'' If the '}' is following in the same line, then a separator must
	'' be added. But take care not to add it in any other cases.
	if (tk_get(x) = TK_RBRACE) then
		if (skip_unless_eol(skiprev(x)) = x) then
			x = insert_statement_separator(x)
		end if
	end if

	return x
end function

private sub split_multdecl_if_needed(byval x as integer, byval begin as integer)
	'' First job: split the declaration in two, so that the head part
	'' consists only of identifiers with equal pointer count on them,
	'' or is just one procedure pointer or procedure declaration.
	'' (To translate a multdecl of procptrs to FB we'd need to compare
	'' the procptr parameters etc., that's way too much work; and procdecls
	'' can't be multdecl in FB, there is no <declare sub a(), b()>)
	'' When splitting, the comma is replaced by a semicolon, and the
	'' beginning of the head part (special tokens such as EXTERN, STATIC,
	'' TYPEDEF; and the type) is duplicated for the tail. The tail part
	'' is now again a valid declaration and will be handled during the
	'' next call to this function, when the translation process reaches it.
	''
	'' Second job: In the head part, remove overhead pointers, as in:
	''    int *a, *b;
	'' turns into:
	''    int *a, b;
	'' then later:
	''    dim as integer ptr a, b
	''
	'' An example of the overall translation process of a topdecl:
	'' (fields are the same, except perhaps they won't have procdecls in C)
	''
	''        1. int *a, *b, c, f();
	'' splitted:
	''        2. int *a, *b; int c, f();
	'' overhead ptrs removed:
	''        3. int *a, b; int c, f();
	'' translated:
	''        4. dim as integer ptr a, b : int c, f();
	'' splitted:
	''        5. dim as integer ptr a, b : int c; int f();
	'' translated:
	''        6. dim as integer ptr a, b : dim as integer c : int f();
	'' translated:
	''        7. dim as integer ptr a, b : dim as integer c : declare function f() as integer
	''
	'' Note: The identifiers can't be reordered, because that would
	'' change the offsets of struct fields.

	'' Skip over the type, but remember its position/size, because in case
	'' of a split, the type tokens need to be duplicated.
	x = parse_base_type(x)

	'' The type parser skips to the next non-type token,
	'' so to get the real typeend, skip back.
	dim as integer typeend = skiprev(x)

	'' Properties of the first identifier. Following ones are compared
	'' against these to determine whether a split is needed.
	dim as integer first_ptrcount = 0
	dim as integer first_is_procptr = FALSE
	dim as integer first_is_procdecl = FALSE
	dim as integer lastcomma = -1
	dim as integer end_of_first = x

	do
		'' This can be:
		''   normal: '*'* id
		'' procdecl: '*'* id '(' params ')'
		''  procptr: '*'* '(' '*' id ')' '(' params ')'

		'' '*'*
		'' (in case of procdecl/procptr, this are the pointers on the
		'' function result type)
		dim as integer ptrcount = 0
		while (tk_get(x) = TK_STAR)
			ptrcount += 1
			x = skip(x)
		wend

		'' ['(' '*']
		dim as integer is_procptr = FALSE
		if (tk_get(x) = TK_LPAREN) then
			'' '('
			is_procptr = TRUE
			x = skip(x)

			ASSUMING(tk_get(x) = TK_STAR)
			x = skip(x)
		end if

		'' id
		ASSUMING(tk_get(x) = TK_ID)
		x = skip(x)

		'' [')']
		if (is_procptr) then
			'' ')'
			ASSUMING(tk_get(x) = TK_RPAREN)
			x = skip(x)
		end if

		'' ['(' params ')']
		dim as integer is_procdecl = FALSE
		if (tk_get(x) = TK_LPAREN) then
			'' '('
			is_procdecl = not is_procptr
			x = find_parentheses(x, FALSE)

			'' ')'
			ASSUMING(tk_get(x) = TK_RPAREN)
			x = skip(x)
		end if

		'' ',' or ';' terminates this part of the decl
		ASSUMING((tk_get(x) = TK_COMMA) or (tk_get(x) = TK_SEMI))

		if (lastcomma < 0) then
			'' Just parsed the first identifier
			first_ptrcount = ptrcount
			first_is_procptr = is_procptr
			first_is_procdecl = is_procdecl
			end_of_first = x
		else
			'' Just parsed the second/third/... identifier.
			'' Different properties? Then split it off, together
			'' with all following ones.
			if ((first_ptrcount <> ptrcount) or _
			    (first_is_procptr <> is_procptr) or _
			    (first_is_procdecl <> is_procdecl)) then

				'' Replace ',' with ';'
				tk_remove(lastcomma, lastcomma)
				tk_insert(lastcomma, TK_SEMI, NULL)

				'' Copy in the begin tokens (e.g. TYPEDEF in
				'' case of a typedef) and the type. It all goes
				'' behind the new semicolon. And also preserve
				'' the marks.
				lastcomma += 1
				tk_copy(lastcomma, begin, typeend)
				tk_set_mark(tk_mark(begin), lastcomma, _
				            lastcomma + typeend - begin + 1)

				'' And a space between semicolon/type
				tk_insert_space(lastcomma)

				'' Done -- the rest will be parsed during
				'' a following call to this function,
				'' when the translation process reaches it...
				exit do
			end if
		end if

		select case (tk_get(x))
		case TK_SEMI
			exit do

		case TK_EOF
			ASSUMING(FALSE)
			exit do

		end select

		lastcomma = x
		x = skip(x)
	loop

	'' After the split (or no split in case it was just one identifier
	'' in the first place), the head part is remarked, so the corresponding
	'' translators will translate it now.
	''  - If the begin token is marked as topdecl, then the new mark
	''    is either vardecl or procdecl depending on whether it's a
	''    procdecl or not.
	''  - If the begin token is marked as typedef or field, nothing
	''    neds to be done, since it's already marked correctly.
	dim as integer mark = tk_mark(begin)
	if (mark = MARK_TOPDECL) then
		if (first_is_procdecl) then
			mark = MARK_PROCDECL
		else
			mark = MARK_VARDECL
		end if
		tk_set_mark(mark, begin, end_of_first)
	end if
end sub

private sub remove_overhead_ptrs(byval x as integer)
	'' Second job: remove overhead pointers
	'' Assuming field declarations with multiple identifiers but different
	'' ptrcounts on some of them have been split up properly, we will only
	'' encounter fields of these forms here:
	''    int a;
	''    int *a;
	''    int a, b, c;
	''    int *a, *b, *c;
	''    int f();
	''    int *f();
	''    int (*p)();
	''    int *(*p)();
	'' i.e. all identifiers per declaration with equal ptrcounts.
	'' This translation step turns this:
	''    int *a, *b, *c;
	'' into:
	''    int *a, b, c;
	'' which is needed to get to this:
	''    as integer ptr a, b, c
	'' (Note: this also needs to handle procptrs and procdecls)

	x = parse_base_type(x)

	'' Just remove all ptrs behind commas and outside parentheses.
	'' Also remove unnecessary space.
	dim as integer level = 0
	dim as integer have_comma = FALSE
	do
		select case (tk_get(x))
		case TK_STAR
			if ((level = 0) and have_comma) then
				remove_this_and_space(x)
				'' Counter the x += 1 below; we already are
				'' at the next token after having removed the
				'' current one...
				x -= 1
			end if

		case TK_COMMA
			if (level = 0) then
				have_comma = TRUE
			end if

		case TK_LPAREN
			level += 1

		case TK_RPAREN
			level -= 1

		case TK_SEMI
			ASSUMING(level = 0)
			exit do

		case TK_EOF
			ASSUMING(FALSE)
			exit do

		end select

		x += 1
	loop
end sub

private sub fixup_multdecl(byval x as integer, byval begin as integer)
	'' A multdecl (a declaration allowing multiple identifiers separated
	'' by commas, but reusing the same base type) is something like this:
	''    int a, b;        ->    dim as integer a, b
	''
	'' Because in C pointers belong to the identifier, but in FB, they
	'' belong to the type, multdecls with pointers might need to be split
	'' up into separate [mult]decls.
	''    int a, b, *c;    ->    dim as integer a, b : dim as integer ptr c
	''
	'' To top it of, in C you can also declare procedures in one
	'' declaration together with variables.
	''    int a, f();      ->    dim as integer a : declare function f() as integer
	''
	'' And not to forget, variables can also be procedure pointers.
	''    int (*a)(), *(*b)();   ->    dim as function() as integer a
	''                                 dim as function() as integer ptr b

	split_multdecl_if_needed(x, begin)
	remove_overhead_ptrs(x)
end sub

private function translate_base_type(byval x as integer) as integer
	''    int             ->      as integer
	''    unsigned int    ->      as uinteger
	''    struct T        ->      as T
	'' etc.

	'' Insert the AS
	x = insert_spaced_token(x, KW_AS, NULL)

	select case (tk_get(x))
	case KW_ENUM, KW_STRUCT, KW_UNION
		'' {ENUM | STRUCT | UNION} id
		tk_remove(x, skip(x) - 1)

		ASSUMING(tk_get(x) = TK_ID)
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
			tk_remove(x + 1, skip(x))
		end if

	case KW_LONG
		basekw = iif(signed, KW_LONG, KW_ULONG)

		'' [LONG]
		if (tk_get(skip(x)) = KW_LONG) then
			basekw = iif(signed, KW_LONGINT, KW_ULONGINT)
			tk_remove(x + 1, skip(x))
		end if

		'' [INT]
		if (tk_get(skip(x)) = KW_INT) then
			tk_remove(x + 1, skip(x))
		end if

	end select

	if (basekw >= 0) then
		tk_remove(x, x)
		tk_insert(x, basekw, NULL)
		x = skip(x)
	end if

	select case (tk_get(sign))
	case KW_SIGNED, KW_UNSIGNED
		if (basekw >= 0) then
			'' Remove the [UN]SIGNED, it's now encoded into the
			'' FB type (e.g. UINTEGER)
			dim as integer last = skip(sign) - 1
			tk_remove(sign, last)
			x -= (last - sign + 1)
		else
			'' Found [UN]SIGNED only, treat it as [U]INTEGER
			tk_remove(sign, sign)
			tk_insert(sign, iif(signed, KW_INTEGER, KW_UINTEGER), NULL)
		end if
	end select

	return x
end function

private function translate_ptrs(byval x as integer) as integer
	'' Pointers: '*' -> PTR, plus some space if needed
	while (tk_get(x) = TK_STAR)
		remove_this_and_space(x)
		x = insert_spaced_token(x, KW_PTR, NULL)
	wend
	return x
end function

private function decl_is_procptr(byval x as integer) as integer
	return (tk_get(parse_ptrs(parse_base_type(x))) = TK_LPAREN)
end function

'' Check whether the declaration is a sub procdecl or sub procptr
private function decl_type_is_void_only(byval x as integer) as integer
	if (tk_get(x) <> KW_VOID) then
		return FALSE
	end if
	x = skip(x)

	'' It's a sub if there are no '*'s (pointers would make it a <function
	'' as any ptr+>), same for procptr
	return ((tk_get(x) = TK_ID) or (tk_get(x) = TK_LPAREN))
end function

declare function translate_decl _
	( _
		byval x as integer, _
		byval decl as integer _
	) as integer

private function translate_params(byval x as integer) as integer
	'' '('
	ASSUMING(tk_get(x) = TK_LPAREN)

	'' Just '(void)'?
	if (tk_get(skip(x)) = KW_VOID) then
		dim as integer rparen = skip(skip(x))
		if (tk_get(rparen) = TK_RPAREN) then
			'' Go to first token behind lparen
			x += 1

			'' Just remove the whole [space]VOID[space]
			tk_remove(x, rparen - 1)

			'' Skip rparen and done
			return skip(x)
		end if
	end if

	x = skip(x)

	'' params
	do
		select case (tk_get(x))
		case TK_RPAREN
			exit do

		case TK_COMMA, TK_ELLIPSIS
			'' Let ',' and '...' pass
			x = skip(x)

		case TK_EOF
			ASSUMING(FALSE)
			exit do

		case else
			'' Translate the param (recursion!)
			x = translate_decl(x, DECL_PARAM)
			ASSUMING((tk_get(x) = TK_COMMA) or (tk_get(x) = TK_RPAREN))

		end select
	loop

	'' ')'
	ASSUMING(tk_get(x) = TK_RPAREN)
	x = skip(x)

	return x
end function

private function translate_decl _
	( _
		byval x as integer, _
		byval decl as integer _
	) as integer

	'' {var|field|proc}decl, param and typedef translation.
	''
	'' Here we'll only get things like this:
	''    int *i               ->    as integer ptr i
	''    int a, b, c          ->    as integer a, b, c
	''    struct T **a, b      ->    as T ptr ptr a, b
	''    int f()              ->    declare function f() as integer
	''    void (*p)()          ->    as sub() p
	''    int (*p)()           ->    as function() as integer p
	''
	'' Thanks to the preprocessing by fixup_multdecl(),
	''  - Pointers ('*') will only appear right behind the type,
	''    ready to be replaced by PTRs
	''  - For plain vardecls/fielddecls, there can be multiple identifiers
	''    separated by commas
	''  - However, if it's a procptr or procdecl, it will be the only thing
	''    in this declaration
	''
	'' vardecls/fields:
	''    1. int * a, b
	''    2. as integer ptr a, b
	''
	'' procptrs (complex vardecls):
	''    1. int (*p)(...)
	''     . as function(...) as integer p
	''
	'' procdecls:
	''        [EXTERN | STATIC] type {'(' id ')' | id} '(' params ')' ';'
	''    ->
	''        DECLARE {SUB | FUNCTION} id '(' params ')' [AS type]
	''
	''    1. int a(...)
	''    2. declare function int a(...)
	''    3. declare function as integer a(...)
	''    4. declare function a(...) as integer
	''
	'' params:
	''    1. int a
	''    2. byval int a
	''    3. byval as integer a
	''    4. byval a as integer
	''
	'' typedefs:
	''     typedef T id;    ->    type as T id
	''

	select case (decl)
	case DECL_PROC
		select case (tk_get(x))
		case KW_EXTERN, KW_STATIC
			remove_this_and_space(x)
		end select

	case DECL_VAR
		if (tk_get(x) = KW_EXTERN) then
			x = skip(x)
		else
			if (tk_get(x) = KW_STATIC) then
				remove_this_and_space(x)

				'' PRIVATE
				x = insert_spaced_token(x, KW_PRIVATE, NULL)
			end if

			'' DIM SHARED
			x = insert_spaced_token(x, KW_DIM, NULL)
			x = insert_spaced_token(x, KW_SHARED, NULL)
		end if

	case DECL_TYPEDEF
		'' TYPEDEF -> TYPE
		ASSUMING(tk_get(x) = KW_TYPEDEF)
		tk_remove(x, x)
		tk_insert(x, KW_TYPE, NULL)
		x = skip(x)

	case DECL_PARAM
		'' Note: params only appear behind '(' or ','

		'' BYVAL
		tk_insert(x, KW_BYVAL, NULL)
		x = skip(x)

	end select

	dim as integer is_procptr = decl_is_procptr(x)
	dim as integer is_sub = decl_type_is_void_only(x)

	'' procdecls: DECLARE {FUNCTION | SUB}
	'' typedef/var/field procptrs: AS {FUNCTION | SUB}
	'' The AS {FUNCTION | SUB} for procptr params is inserted later...
	if ((is_procptr and (decl <> DECL_PARAM)) or (decl = DECL_PROC)) then
		'' DECLARE | AS
		x = insert_spaced_token(x, _
				iif(is_procptr, KW_AS, KW_DECLARE), NULL)

		'' FUNCTION | SUB
		x = insert_spaced_token(x, _
				iif(is_sub, KW_SUB, KW_FUNCTION), NULL)
	end if

	'' Type + pointers
	dim as integer typebegin = x
	x = translate_ptrs(translate_base_type(x))
	dim as integer typeend = skiprev(x)

	'' Exclude space that might have been added from the type
	typebegin = skip(typebegin - 1)

	'' '('?
	if (is_procptr) then
		ASSUMING(tk_get(x) = TK_LPAREN)
		remove_this_and_space(x)

		'' '*'
		ASSUMING(tk_get(x) = TK_STAR)
		remove_this_and_space(x)
	end if

	'' id (optional for params, required elsewhere)
	if (tk_get(x) = TK_ID) then
		x = skip(x)
	else
		ASSUMING(decl = DECL_PARAM)
	end if

	if (is_procptr and (decl = DECL_PARAM)) then
		'' Move directly behind the id or the BYVAL (if there's no id)
		'' (could be space here)
		x = skiprev(x) + 1

		'' AS {SUB | FUNCTION} for procptr params
		'' (inserted here instead of the front so it doesn't
		'' need to be moved here)

		tk_insert_space(x)
		x += 1

		tk_insert(x, KW_AS, NULL)
		x += 1

		tk_insert_space(x)
		x += 1

		tk_insert(x, iif(is_sub, KW_SUB, KW_FUNCTION), NULL)
		x += 1

		'' Ensure to go back to the next non-space token
		x = skip(x - 1)
	end if

	'' ')'
	if (is_procptr) then
		ASSUMING(tk_get(x) = TK_RPAREN)
		remove_this_and_space(x)
	end if

	'' '(' params ')' (recursion!)
	if (is_procptr or (decl = DECL_PROC)) then
		x = translate_params(x)
	end if

	if (is_procptr or (decl = DECL_PROC) or (decl = DECL_PARAM)) then
		'' Procdecls, procptrs and params require the type to be moved from the
		'' front to the back:
		''    declare function as integer f() -> declare function f() as integer
		''    as function as integer p()      -> as function() as integer p
		''    byval as integer i              -> byval i as integer
		''    declare sub as any s()          -> declare sub s()
		''    as sub as any s()               -> as sub() s
		''
		'' For subs (procdecls or procptrs), the AS ANY just needs to
		'' be removed, not copied.

		'' Move directly behind the id/')' (could be space here)
		x = skiprev(x) + 1

		if (is_sub = FALSE) then
			'' Append the type directly behind the id (for params),
			'' or ')' (for procdecls/procptrs), and insert a space too.

			tk_insert_space(x)
			x += 1

			'' Copy the translated type/ptrs
			tk_copy(x, typebegin, typeend)
			x += (typeend - typebegin + 1)
		end if

		'' For all procptrs except params, the id must be moved to the
		'' end, that's easier than moving the params instead.
		if (is_procptr and (decl <> DECL_PARAM)) then
			tk_insert_space(x)
			x += 1

			'' Just extend the typeend to the id so it will be
			'' deleted below too...
			typeend = skip(typeend)

			'' Copy the id to the back
			ASSUMING(tk_get(typeend) = TK_ID)
			tk_copy(x, typeend, typeend)
			x += 1

			'' Remove space behind the id below too
			''    as function ()              ->    as function()
			typeend = skip(typeend) - 1
		end if

		'' Remove the old type + pointers + space in the front
		typebegin = skiprev(typebegin) + 1
		tk_remove(typebegin, typeend)
		x -= (typeend - typebegin + 1)

		'' Ensure to go back to the next non-space token
		x = skip(x - 1)
	else
		'' Not a procdecl/procptr/param:
		'' Can have more ids separated by commas.
		'' (',' id)*
		while (tk_get(x) = TK_COMMA)
			x = skip(x)

			'' id
			ASSUMING(tk_get(x) = TK_ID)
			x = skip(x)
		wend
	end if

	select case (decl)
	case DECL_PROC, DECL_VAR, DECL_FIELD, DECL_TYPEDEF
		'' ';'
		ASSUMING(tk_get(x) = TK_SEMI)
		tk_remove(x, x)
		if (is_whitespace_until_eol(x) = FALSE) then
			x = insert_statement_separator(x)
		end if
		x = skip(x - 1)

	end select

	return x
end function

private function translate_compound_end _
	( _
		byval x as integer, _
		byval compoundkw as integer _
	) as integer

	'' '}'                  ->    END EXTERN
	'' '}' [multdecl] ';'   ->    END {TYPE|ENUM|UNION}
	'' Where multdecl can be something like <A, *PA, (*FPA)()> etc. in
	'' the case of <typedef struct {} multdecl ;>. See also the struct
	'' translator. Here this just needs to be removed.

	'' Remove '}' and space in front of it
	ASSUMING(tk_get(x) = TK_RBRACE)

	if (compoundkw = KW_EXTERN) then
		tk_remove(x, x)
	else
		tk_remove(x, find_token(x, TK_SEMI))
	end if

	x = insert_spaced_token(x, KW_END, NULL)
	x = insert_spaced_token(x, compoundkw, NULL)

	if (is_whitespace_until_eol(skiprev(x) + 1) = FALSE) then
		x = insert_statement_separator(x)
	end if

	return x
end function

private function translate_struct(byval x as integer) as integer
	'' structs/enums beginnings are translated here.
	'' To support typedefs to struct blocks (which might also be
	'' anonymous) the ending is searched to retrieve the typedef id
	'' from there.
	''
	'' struct T { ... };               ->    type T : ... : end type
	'' Note: any occurences of <struct T> will be translated to just <T>
	'' by the type translator, so this just works.
	''
	'' typedef struct { ... } TT;      ->    type TT : ... : end type
	''
	'' typedef struct T { ... } TT;    ->    type T : ... : end type
	''                                       type as T TT
	'' Afterall both identifiers might be needed.
	''
	'' typedef struct A { ... } A;     ->    type A : ... : end type
	''
	'' All in all this means:
	'' If the struct has an id, then use it for the type. If it has no
	'' id, then move the one at the end of the declaration to the front.
	'' If there is none there either, insert a TODO instead.
	'' If both struct and typedef have an identifier, but they are
	'' different, then insert a <typedef struct structid typedefid;>
	'' after the struct end, which will then be handled by the typedef
	'' translator.
	''
	'' Same for enums/unions. And this is also used to translate nested
	'' structs/unions. Note: FB doesn't support non-anonymous nested
	'' structs.
	''    struct { } id;               ->    type id /' TODO '/ : ...

	'' [TYPEDEF]
	dim as integer is_typedef = FALSE
	if (tk_get(x) = KW_TYPEDEF) then
		tk_remove(x, skip(x) - 1)
		is_typedef = TRUE
	end if

	dim as integer compoundkw = tk_get(x)
	if (compoundkw = KW_STRUCT) then
		'' STRUCT -> TYPE
		tk_remove(x, x)
		tk_insert(x, KW_TYPE, NULL)
	else
		ASSUMING((compoundkw = KW_ENUM) or (compoundkw = KW_UNION))
	end if

	x = skip(x)

	'' ['id']
	dim as integer structid = x
	if (tk_get(x) = TK_ID) then
		x = skip(x)
	end if

	'' '{'
	ASSUMING(tk_get(x) = TK_LBRACE)

	'' Typedef-to-struct-block split up hack:
	''    typedef struct T { ... } a, *b, (*c)();
	'' must be translated to:
	''    type T : ... : end type : typedef struct T a, *b, (*c)();
	'' so then the normal typedef parser can handle the typedef.
	'' (Having something like <typedef struct { ... } FOO, *PFOO;>
	'' is quite common, so this is important)
	''
	'' However, there also is this common use case with an
	'' anonymous struct:
	''    typedef struct { ... } T;
	'' And that is usually best translated to:
	''    type T : ... : end type
	''
	'' But of course this can only be done if the typedef is just
	'' a plain simple identifier like 'T'. If it has pointers,
	'' commas + multiple identifiers or even procedure pointers,
	'' then the typedef must be split off. If the struct is
	'' anonymous then, a fake identifer has to be used (though
	'' choosing that id is the user's job, we just insert a TODO).
	''
	'' And there also is this use case (named nested union/struct):
	''    struct { } foo;
	'' Which isn't supported in FB, but we can still translate it
	'' to translate the contained fields too, and add a TODO.

	'' 1. Find the '}'
	'' 2. Check whether there is a single identifier.
	''    If the struct is anonymous, copy that identifier to the
	''    front.
	''    Otherwise, do the typedef split up hack.

	dim as integer y = find_parentheses(x, FALSE)

	'' '}'
	ASSUMING(tk_get(y) = TK_RBRACE)
	if (compoundkw = KW_ENUM) then
		ASSUMING(tk_mark(y) = MARK_ENDENUM)
	elseif (compoundkw = KW_STRUCT) then
		ASSUMING(tk_mark(y) = MARK_ENDSTRUCT)
	else
		ASSUMING(tk_mark(y) = MARK_ENDUNION)
	end if
	y = skip(y)

	'' <id ';'> and struct is anonymous?
	if ((tk_get(y) = TK_ID) and _
	    (tk_get(skip(y)) = TK_SEMI) and _
	    (tk_get(structid) <> TK_ID)) then

		'' Add TODO for <struct { } id;>
		if (is_typedef = FALSE) then
			tk_insert(structid, TK_TODO, "translated from anonymous struct/union/enum in declaration")
			tk_insert_space(structid)
			x += 2
			y += 2
		end if

		'' Copy the trailing id to the front.
		tk_copy(structid, y, y)
		x += 1

		'' If needed, insert a space to separate the STRUCT
		'' from the identifier.
		if (tk_get(structid - 1) <> TK_SPACE) then
			tk_insert_space(structid)
			x += 1
		end if

	elseif (is_typedef) then
		'' Do the split up hack.
		'' If the struct is anonymous, we must fake an id,
		'' it's needed for the typedef we want to split off.
		'' (need to declare a typedef to /something/)

		if (tk_get(structid) <> TK_ID) then
			tk_insert(structid, TK_TODO, "added fake id for anonymous struct")
			tk_insert_space(structid)
			tk_insert(structid, TK_ID, "__FAKE__")

			'' Inserting at the top moves everything down..
			x += 3
			y += 3
		end if

		'' Turn this:
		''    ... a, *b, (*c)();
		'' into:
		''    ... ; typedef struct structid a, *b, (*c)();

		'' Add a new ';' and mark it as same as '}'
		tk_insert(y, TK_SEMI, NULL)
		tk_set_mark(tk_mark(skiprev(y)), y, y)
		y += 1

		tk_insert_space(y)
		y += 1

		dim as integer typedefbegin = y

		tk_insert(y, KW_TYPEDEF, NULL)
		y += 1
		tk_insert_space(y)
		y += 1
		tk_insert(y, compoundkw, NULL)
		y += 1
		tk_insert_space(y)
		y += 1
		tk_copy(y, structid, structid)
		y += 1
		if (tk_get(y) <> TK_SPACE) then
			tk_insert_space(y)
			y += 1
		end if

		'' Skip over the existing multdecl, until ';'
		do
			select case (tk_get(y))
			case TK_SEMI
				exit do
			case TK_EOF
				ASSUMING(FALSE)
				exit do
			end select

			y = skip(y)
		loop

		'' Ensure it's translated as typedef
		tk_set_mark(MARK_TYPEDEF, typedefbegin, y)
	end if

	'' Remove the '{' and space in front of it. If there is an EOL
	'' following, insert a ':' statement separator.
	ASSUMING(tk_get(x) = TK_LBRACE)
	dim as integer spacebegin = skiprev(x) + 1
	tk_remove(spacebegin, x)
	x -= x - spacebegin

	if (is_whitespace_until_eol(x) = FALSE) then
		x = insert_statement_separator(x)
	end if

	return x
end function

sub translate_toplevel()
	dim as integer x = skip(-1)

	while (tk_get(x) <> TK_EOF)
		select case as const (tk_mark(x))
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
			dim as integer first = skiprev_unless_eol(x)
			select case (tk_get(first))
			case TK_EOL, TK_EOF
				'' Ok, there's only indentation in front of us.
				'' Insert the TODO right here, then an EOL,
				'' then duplicate the indentation to re-indent
				'' the unknown construct. That way, the TODO
				'' will appear nicely aligned above the
				'' construct it refers to.
				'' (Often there won't be any indentation,
				'' then nothing will be copied)
				first += 1 '' (not the EOL at the front)
				dim as integer last = x - 1

				tk_insert(x, TK_TODO, "unknown construct")
				x += 1

				tk_insert(x, TK_EOL, NULL)
				x += 1

				if (first <= last) then
					tk_copy(x, first, last)
					x += last - first + 1
				end if
			case else
				'' Insert in the middle of the line
				x = insert_spaced_token(x, TK_TODO, _
							"unknown construct")
			end select

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

			case MARK_PP
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

sub frog_work()
	dim as FrogFile ptr f = list_head(@frog.files)
	while (f)
		print "working on '" & f->h & "'"
		tk_init()
		tk_insert_file(0, f->h)

		parse_toplevel()
		translate_toplevel()
		fixup_eols()
		fixup_comments()

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

#include once "frog.bi"

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

function parse_struct(byval x as integer) as integer
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

		x = parse_pp_directive(x, FALSE)

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

function parse_extern_begin(byval x as integer) as integer
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

function parse_extern_end(byval x as integer) as integer
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

function translate_compound_end _
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

function translate_struct(byval x as integer) as integer
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

		'' If needed, insert a space to separate the STRUCT
		'' from the identifier.
		if (tk_get(structid - 1) <> TK_SPACE) then
			tk_insert_space(structid)
			structid += 1

			x += 1
			y += 1
		end if

		'' Add TODO for <struct { } id;>
		if (is_typedef = FALSE) then
			tk_insert(structid, TK_TODO, "illegal FB")
			structid += 1

			tk_insert_space(structid)
			structid += 1

			x += 2
			y += 2
		end if

		'' Copy the trailing id to the front.
		tk_copy(structid, y, y)
		x += 1

	elseif (is_typedef) then
		'' Do the split up hack.
		'' If the struct is anonymous, we must fake an id,
		'' it's needed for the typedef we want to split off.
		'' (need to declare a typedef to /something/)

		if (tk_get(structid) <> TK_ID) then
			'' If needed, insert a space to separate the STRUCT
			'' from the identifier.
			if (tk_get(structid - 1) <> TK_SPACE) then
				tk_insert_space(structid)
				structid += 1

				x += 1
				y += 1
			end if

			tk_insert(structid, TK_TODO, "faked id")
			structid += 1
			tk_insert_space(structid)
			structid += 1
			tk_insert(structid, TK_ID, "FAKE")

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

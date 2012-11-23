#include once "parser.bi"

function parseEnumconst _
	( _
		byval x as integer, _
		byval is_unknown as integer _
	) as integer

	dim as integer begin = any, skip_expression = any, level = any

	begin = x
	skip_expression = is_unknown

	if( is_unknown = FALSE ) then
		'' id
		if( tkGet( x ) <> TK_ID ) then
			return begin
		end if
		x = hSkip( x )

		select case( tkGet( x ) )
		case TK_EQ
			'' ['=']
			skip_expression = TRUE
			x = hSkip( x )
		case TK_LPAREN
			'' '('
			'' This allows function-like macros like <FOO(...)>
			'' in place of constants in an enum, at least the
			'' libcurl headers make extensive use of that...
			'' The '(...)' skipping is covered by the same code
			'' that skips over the '= ...' expressions.
			skip_expression = TRUE
		end select
	end if

	if( skip_expression ) then
		'' Skip until ',' or '}'
		level = 0
		do
			select case( tkGet( x ) )
			case TK_LPAREN
				level += 1
			case TK_RPAREN
				level -= 1
			case TK_COMMA
				if( level = 0 ) then
					exit do
				end if
			case TK_RBRACE, TK_EOF, TK_HASH
				'' Note: '#' (PP directives) not allowed in
				'' expressions in FB, this can't be translated.
				exit do
			end select
			x = hSkip( x )
		loop
	end if

	select case( tkGet( x ) )
	case TK_COMMA
		'' Treat the comma as part of the constant declaration
		x = hSkip( x )
	case TK_RBRACE

	case else
		if( is_unknown = FALSE ) then
			return begin
		end if
	end select

	'' Mark the constant declaration
	tkSetMark( iif( is_unknown, MARK_UNKNOWNENUMCONST, MARK_ENUMCONST ), _
			begin, hSkipRev( x ) )

	function = x
end function

private function parseBaseType( byval x as integer ) as integer
	dim as integer old = any

	old = x

	'' [CONST]
	x = hSkipOptional( x, KW_CONST )

	select case( tkGet( x ) )
	case KW_ENUM, KW_STRUCT, KW_UNION
		'' {ENUM | STRUCT | UNION} id
		x = hSkip( x )

		'' id
		if( tkGet( x ) <> TK_ID ) then
			return old
		end if
		x = hSkip( x )

		'' [CONST]
		x = hSkipOptional( x, KW_CONST )

		return x

	case TK_ID
		'' Just a single id
		x = hSkip( x )

		'' [CONST]
		x = hSkipOptional( x, KW_CONST )

		return x
	end select

	'' [SIGNED | UNSIGNED]
	select case( tkGet( x ) )
	case KW_SIGNED, KW_UNSIGNED
		x = hSkip( x )
	end select

	'' [ VOID | CHAR | FLOAT | DOUBLE | INT
	'' | SHORT [INT]
	'' | LONG [LONG] [INT]
	'' ]
	select case( tkGet( x ) )
	case KW_VOID, KW_CHAR, KW_FLOAT, KW_DOUBLE, KW_INT
		x = hSkip( x )

	case KW_SHORT
		x = hSkip( x )

		'' [INT]
		x = hSkipOptional( x, KW_INT )

	case KW_LONG
		x = hSkip( x )

		'' [LONG]
		x = hSkipOptional( x, KW_LONG )

		'' [INT]
		x = hSkipOptional( x, KW_INT )

	end select

	'' [CONST]
	x = hSkipOptional( x, KW_CONST )

	'' In case of no type keyword at all, x = old
	function = x
end function

private function parsePtrs( byval x as integer ) as integer
	'' Pointers: ('*')*
	while( tkGet( x ) = TK_STAR )
		x = hSkip( x )

		'' [CONST] (behind the '*')
		x = hSkipOptional( x, KW_CONST )
	wend
	function = x
end function

function parseMultdecl _
	( _
		byval x as integer, _
		byval begin as integer, _
		byval decl as integer _
	) as integer

	dim as integer typebegin = any, is_procptr = any, old = any, mark = any

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
	if( decl <> DECL_TYPEDEFSTRUCTBLOCK ) then
		'' type
		typebegin = x
		x = parseBaseType( x )
		if( x = typebegin ) then
			return begin
		end if
	end if

	'' var (',' var)*
	do
		'' '*'*
		x = parsePtrs( x )

		'' '('?
		is_procptr = FALSE
		if( tkGet( x ) = TK_LPAREN ) then
			is_procptr = TRUE
			x = hSkip( x )

			'' '*'
			if( tkGet( x ) <> TK_STAR ) then
				return begin
			end if
			x = hSkip( x )
		end if

		'' id (must be there except for params)
		if( tkGet( x ) = TK_ID ) then
			x = hSkip( x )
		elseif( decl <> DECL_PARAM ) then
			return begin
		end if

		if( is_procptr ) then
			'' ')'
			if( tkGet( x ) <> TK_RPAREN ) then
				return begin
			end if
			x = hSkip( x )
		end if

		'' Check for '(' params ')'
		if( tkGet( x ) = TK_LPAREN ) then
			'' Note: typedef to procdecl can't be translated,
			'' so it's disallowed here. If there are procdecls
			'' in fields, then fine, that's C++ stuff, but oh well.
			'' Note: procptrs always have params.
			if( (is_procptr = FALSE) and _
			    ((decl = DECL_TYPEDEF) or _
			     (decl = DECL_TYPEDEFSTRUCTBLOCK)) ) then
				return begin
			end if

			'' '('
			x = hSkip( x )

			'' Just '(void)'?
			if( (tkGet( x ) = KW_VOID) and (tkGet( hSkip( x ) ) = TK_RPAREN)) then
				'' VOID
				x = hSkip( x )
			else
				'' [ type [id]  (',' type [id] )*  [ ',' '...' ] ]
				'' Note: The following isn't even doing that much
				'' syntax verification at all, but it's ok, it's not
				'' a compiler afterall.
				do
					select case( tkGet( x ) )
					case TK_RPAREN
						exit do
					case TK_EOF
						return begin
					case TK_COMMA, TK_ELLIPSIS
						'' Let ',' and '...' pass
						x = hSkip( x )
					case else
						old = x
						x = parseMultdecl( x, x, DECL_PARAM )
						if( x = old ) then
							return begin
						end if
					end select
				loop
			end if

			'' ')'
			if( tkGet( x ) <> TK_RPAREN ) then
				return begin
			end if
			x = hSkip( x )
		end if

		'' Everything can have a comma and more identifiers,
		'' except for params.
		if( (decl = DECL_PARAM) or (tkGet( x ) <> TK_COMMA) ) then
			exit do
		end if
		x = hSkip( x )
	loop

	select case( decl )
	case DECL_FIELD, DECL_TYPEDEF, DECL_TOP
		'' ';'
		if( tkGet( x ) <> TK_SEMI ) then
			return begin
		end if

		mark = MARK_TOPDECL
		select case( decl )
		case DECL_FIELD
			mark = MARK_FIELDDECL
		case DECL_TYPEDEF
			mark = MARK_TYPEDEF
		end select

		tkSetMark( mark, begin, x )
		x = hSkip( x )

	end select

	function = x
end function

function parseTopdeclOrTypedef( byval x as integer ) as integer
	dim as integer begin = any, decl = any, multdecl = any

	'' Toplevel declarations: global variables (including procedure
	'' pointers), procedure declarations, and also typedefs, since they
	'' are almost the same (parse_multdecl() disallows typedefs to
	'' procdecls, because those aren't possible FB).
	''    int a, *b, (*c)(), f(), *g();
	''    ...
	''
	'' [TYPEDEF|EXTERN|STATIC] multdecl

	begin = x
	decl = DECL_TOP

	select case( tkGet( x ) )
	case TK_ID
		'' Allow an id at the front, if it represents a calling
		'' convention, a dllexport declspec or is empty...
		if( frogAddDefine( tkText( x ), 0 ) ) then
			x = hSkip( x )
		end if
	case KW_EXTERN, KW_STATIC
		x = hSkip( x )
	case KW_TYPEDEF
		decl = DECL_TYPEDEF
		x = hSkip( x )
	end select

	multdecl = x
	x = parseMultdecl( x, begin, decl )
	if( x = multdecl ) then
		return begin
	end if

	function = x
end function

function translateEnumconst( byval x as integer ) as integer
	dim as integer ends_at_eol = any, level = any

	'' identifer ['=' expression] [',']

	assert( tkGet( x ) = TK_ID )

	ends_at_eol = FALSE
	level = 0
	do
		x = hSkip( x )

		assert( tkGet( x ) <> TK_EOF )

		select case( tkGet( x ) )
		case TK_LPAREN
			level += 1
		case TK_RPAREN
			level -= 1
		case TK_COMMA
			if( level = 0 ) then
				exit do
			end if
		case TK_RBRACE
			exit do
		end select
	loop

	if( tkGet( x ) = TK_COMMA ) then
		'' Beautification: Remove the comma if there is only EOL or
		'' '}' (end of enum) coming.
		if( hIsWhitespaceUntilEol( x + 1 ) or _
		    (tkGet( hSkip( x ) ) = TK_RBRACE)   ) then
			tkRemove( x, x )
			x -= 1
		end if

		'' Either way, skip to the next token after the comma
		x = hSkip( x )
	end if

	'' If the '}' is following in the same line, then a separator must
	'' be added. But take care not to add it in any other cases.
	if( tkGet( x ) = TK_RBRACE ) then
		if( hSkipUnlessEol( hSkipRev( x ) ) = x ) then
			x = hInsertStatementSeparator( x )
		end if
	end if

	function = x
end function

private sub hSplitMultdeclIfNeeded( byval x as integer, byval begin as integer )
	dim as integer typeend = any, lastcomma = any, end_of_first = any, _
		first_ptrcount = any, ptrcount = any, _
		first_is_procptr = any, is_procptr = any, _
		first_is_procdecl = any, is_procdecl = any, _
		mark = any
	dim as uinteger first_constmask = any, constmask = any

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
	x = parseBaseType( x )

	'' The type parser skips to the next non-type token,
	'' so to get the real typeend, hSkip back.
	typeend = hSkipRev( x )

	'' Properties of the first identifier. Following ones are compared
	'' against these to determine whether a split is needed.
	first_ptrcount = 0
	first_constmask = 0
	first_is_procptr = FALSE
	first_is_procdecl = FALSE
	lastcomma = -1
	end_of_first = x

	do
		'' This can be:
		''   normal: ptrs id
		'' procdecl: ptrs id '(' params ')'
		''  procptr: ptrs '(' '*' id ')' '(' params ')'

		'' Pointers (in case of procdecl/procptr, this are the pointers
		'' on the function result type)
		ptrcount = 0
		constmask = 0
		while( tkGet( x ) = TK_STAR )
			ptrcount += 1
			constmask shl= 1
			x = hSkip( x )

			'' [CONST]
			if( tkGet( x ) = KW_CONST ) then
				constmask or= 1
				x = hSkip( x )
			end if
		wend

		'' ['(' '*']
		is_procptr = FALSE
		if( tkGet( x ) = TK_LPAREN ) then
			'' '('
			is_procptr = TRUE
			x = hSkip( x )

			assert( tkGet( x ) = TK_STAR )
			x = hSkip( x )
		end if

		'' id
		assert( tkGet( x ) = TK_ID )
		x = hSkip( x )

		'' [')']
		if( is_procptr ) then
			'' ')'
			assert( tkGet( x ) = TK_RPAREN )
			x = hSkip( x )
		end if

		'' ['(' params ')']
		is_procdecl = FALSE
		if( tkGet( x ) = TK_LPAREN ) then
			'' '('
			is_procdecl = not is_procptr
			x = hFindParentheses( x, 1 )

			'' ')'
			assert( tkGet( x ) = TK_RPAREN )
			x = hSkip( x )
		end if

		'' ',' or ';' terminates this part of the decl
		assert( (tkGet( x ) = TK_COMMA) or (tkGet( x ) = TK_SEMI) )

		if( lastcomma < 0 ) then
			'' Just parsed the first identifier
			first_ptrcount = ptrcount
			first_constmask = constmask
			first_is_procptr = is_procptr
			first_is_procdecl = is_procdecl
			end_of_first = x
		else
			'' Just parsed the second/third/... identifier.
			'' Different properties? Then split it off, together
			'' with all following ones.
			if( (first_ptrcount    <> ptrcount   ) or _
			    (first_constmask   <> constmask  ) or _
			    (first_is_procptr  <> is_procptr ) or _
			    (first_is_procdecl <> is_procdecl)      ) then

				'' Replace ',' with ';'
				tkRemove( lastcomma, lastcomma )
				tkInsert( lastcomma, TK_SEMI, NULL )

				'' Copy in the begin tokens (e.g. TYPEDEF in
				'' case of a typedef) and the type. It all goes
				'' behind the new semicolon. And also preserve
				'' the marks.
				lastcomma += 1
				tkCopy( lastcomma, begin, typeend )
				tkSetMark( tkMark( begin ), lastcomma, _
				            lastcomma + typeend - begin + 1 )

				'' And a space between semicolon/type
				tkInsertSpace( lastcomma )

				'' Done -- the rest will be parsed during
				'' a following call to this function,
				'' when the translation process reaches it...
				exit do
			end if
		end if

		assert( tkGet( x ) <> TK_EOF )

		'' ';'
		if( tkGet( x ) = TK_SEMI ) then
			exit do
		end if

		lastcomma = x
		x = hSkip( x )
	loop

	'' After the split (or no split in case it was just one identifier
	'' in the first place), the head part is remarked, so the corresponding
	'' translators will translate it now.
	''  - If the begin token is marked as topdecl, then the new mark
	''    is either vardecl or procdecl depending on whether it's a
	''    procdecl or not.
	''  - Also, a fielddecl is changed to a procdecl, if it is one
	''    (C++ methods; those can appear as fielddecls inside the
	''    struct/class compound or outside as topdecls, so it's best to
	''    let the procdecl translator handle them)
	''  - Otherwise, it could be a normal fielddecl or a typedef, nothing
	''    needs to be done then, since it's already marked correctly.
	mark = tkMark( begin )
	select case( mark )
	case MARK_TOPDECL
		if( first_is_procdecl ) then
			mark = MARK_PROCDECL
		else
			mark = MARK_VARDECL
		end if
		tkSetMark( mark, begin, end_of_first )
	case MARK_FIELDDECL
		if( first_is_procdecl ) then
			tkSetMark( MARK_PROCDECL, begin, end_of_first )
		end if
	end select
end sub

private sub hRemoveOverheadPtrs( byval x as integer )
	dim as integer level = any, have_comma = any

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

	x = parseBaseType( x )

	'' Just remove all ptrs behind commas and outside parentheses.
	'' Also remove unnecessary space.
	level = 0
	have_comma = FALSE
	do
		assert( tkGet( x ) <> TK_EOF )

		select case( tkGet( x ) )
		case TK_STAR
			if( (level = 0) and have_comma ) then
				hRemoveThisAndSpace( x )

				'' [CONST]
				if( tkGet( x ) = KW_CONST ) then
					hRemoveThisAndSpace(x)
				end if

				'' Counter the x += 1 below; we already are
				'' at the next token after having removed the
				'' current one...
				x -= 1
			end if

		case TK_COMMA
			if( level = 0 ) then
				have_comma = TRUE
			end if
		case TK_LPAREN
			level += 1
		case TK_RPAREN
			level -= 1
		case TK_SEMI
			assert( level = 0 )
			exit do
		end select

		x += 1
	loop
end sub

sub hFixupMultdecl( byval x as integer, byval begin as integer )
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

	hSplitMultdeclIfNeeded( x, begin )
	hRemoveOverheadPtrs( x )
end sub

private function hMaybeMoveConstToFront _
	( _
		byval x as integer, _
		byval front as integer, _
		byval is_ptr as integer _
	) as integer

	dim as integer move_it = any

	'' CONST?
	if( tkGet( x ) <> KW_CONST ) then
		return x
	end if

	'' Copy this CONST (and space in front of it) to the front,
	'' unless there already is a CONST at the front (in C both seem to
	'' be allowed together; only affects CONST on base types, not CONST
	'' on PTRs).
	''
	'' For base types:
	''    as integer const    ->    as const integer
	''
	'' For PTRs:
	''    as integer ptr const    ->    as integer const ptr
	''

	move_it = TRUE
	if( is_ptr = FALSE ) then
		assert( tkGet( hSkipRev( front ) ) = KW_AS )
		move_it = (tkGet( front ) <> KW_CONST)
	end if

	if( move_it ) then
		x += hInsertSpacedToken( front, KW_CONST, NULL ) - front
	end if

	assert( tkGet( x ) = KW_CONST )
	hRemoveThisAndSpace( x )

	function = x
end function

private function translateBaseType( byval x as integer ) as integer
	dim as integer front = any, sign = any, signed = any, basekw = any, _
		last = any

	''    int             ->      as integer
	''    unsigned int    ->      as uinteger
	''    struct T        ->      as T
	'' etc.

	'' Insert the AS
	x = hInsertSpacedToken( x, KW_AS, NULL )

	front = x

	'' [CONST] (in front of the type as in FB, perfect)
	x = hSkipOptional( x, KW_CONST )

	select case( tkGet( x ) )
	case KW_ENUM, KW_STRUCT, KW_UNION
		'' {ENUM | STRUCT | UNION} id
		tkRemove( x, hSkip( x ) - 1 )

		assert( tkGet( x ) = TK_ID )
		x = hSkip( x )

		'' [CONST]
		x = hMaybeMoveConstToFront( x, front, FALSE )

		return x

	case TK_ID
		'' Just a single id
		x = hSkip( x )

		'' [CONST]
		x = hMaybeMoveConstToFront( x, front, FALSE )

		return x

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

	sign = x
	signed = TRUE
	basekw = -1

	select case( tkGet( sign ) )
	case KW_SIGNED
		x = hSkip( x )
	case KW_UNSIGNED
		signed = FALSE
		x = hSkip( x )
	end select

	select case( tkGet( x ) )
	case KW_VOID
		basekw = KW_ANY
	case KW_CHAR
		basekw = iif( signed, KW_BYTE, KW_UBYTE )
	case KW_FLOAT
		basekw = KW_SINGLE
	case KW_DOUBLE
		basekw = KW_DOUBLE
	case KW_INT
		basekw = iif( signed, KW_INTEGER, KW_UINTEGER )
	case KW_SHORT
		basekw = iif( signed, KW_SHORT, KW_USHORT )

		'' [INT]
		if( tkGet( hSkip( x ) ) = KW_INT ) then
			tkRemove( x + 1, hSkip( x ) )
		end if

	case KW_LONG
		basekw = iif( signed, KW_LONG, KW_ULONG )

		'' [LONG]
		if( tkGet( hSkip( x ) ) = KW_LONG ) then
			basekw = iif( signed, KW_LONGINT, KW_ULONGINT )
			tkRemove( x + 1, hSkip( x ) )
		end if

		'' [INT]
		if( tkGet( hSkip( x ) ) = KW_INT ) then
			tkRemove( x + 1, hSkip( x ) )
		end if

	end select

	if( basekw >= 0 ) then
		tkRemove( x, x )
		tkInsert( x, basekw, NULL )
		x = hSkip( x )
	end if

	select case( tkGet( sign ) )
	case KW_SIGNED, KW_UNSIGNED
		if( basekw >= 0 ) then
			'' Remove the [UN]SIGNED, it's now encoded into the
			'' FB type (e.g. UINTEGER)
			last = hSkip( sign ) - 1
			tkRemove( sign, last )
			x -= (last - sign + 1)
		else
			'' Found [UN]SIGNED only, treat it as [U]INTEGER
			tkRemove( sign, sign )
			tkInsert( sign, iif( signed, KW_INTEGER, KW_UINTEGER ), NULL )
		end if
	end select

	'' [CONST]
	x = hMaybeMoveConstToFront( x, front, FALSE )

	function = x
end function

private function translatePtrs( byval x as integer ) as integer
	dim as integer ptrpos = any

	'' Pointers: '*' -> PTR, plus some space if needed
	while( tkGet( x ) = TK_STAR )
		ptrpos = x

		hRemoveThisAndSpace( x )
		x = hInsertSpacedToken( x, KW_PTR, NULL )

		'' [CONST]
		x = hMaybeMoveConstToFront( x, ptrpos, TRUE )
	wend

	function = x
end function

private function hDeclIsProcptr( byval x as integer ) as integer
	function = (tkGet( parsePtrs( parseBaseType( x ) ) ) = TK_LPAREN)
end function

'' Check whether the declaration is a sub procdecl or sub procptr
private function hDeclTypeIsVoidOnly( byval x as integer ) as integer
	if( tkGet( x ) <> KW_VOID ) then
		return FALSE
	end if
	x = hSkip( x )

	'' It's a sub if there are no '*'s (pointers would make it a <function
	'' as any ptr+>), same for procptr
	function = ((tkGet( x ) = TK_ID) or (tkGet( x ) = TK_LPAREN))
end function

private function translateParams( byval x as integer ) as integer
	dim as integer rparen = any

	'' '('
	assert( tkGet( x ) = TK_LPAREN )

	'' Just '(void)'?
	if( tkGet( hSkip( x ) ) = KW_VOID ) then
		rparen = hSkip( hSkip( x ) )
		if( tkGet( rparen ) = TK_RPAREN ) then
			'' Go to first token behind lparen
			x += 1

			'' Just remove the whole [space]VOID[space]
			tkRemove( x, rparen - 1 )

			'' Skip rparen and done
			return hSkip( x )
		end if
	end if

	x = hSkip( x )

	'' params
	do
		assert( tkGet( x ) <> TK_EOF )

		select case( tkGet( x ) )
		case TK_RPAREN
			exit do
		case TK_COMMA, TK_ELLIPSIS
			'' Let ',' and '...' pass
			x = hSkip( x )
		case else
			'' Translate the param (recursion!)
			x = translateDecl( x, DECL_PARAM )
			assert( (tkGet( x ) = TK_COMMA) or (tkGet( x ) = TK_RPAREN) )
		end select
	loop

	'' ')'
	assert( tkGet( x ) = TK_RPAREN )
	x = hSkip( x )

	function = x
end function

function translateDecl _
	( _
		byval x as integer, _
		byval decl as integer _
	) as integer

	dim as string calldefine
	dim as uinteger flags = any
	dim as integer is_extern = any, is_procptr = any, is_sub = any, _
		typebegin = any, typeend = any

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

	select case( decl )
	case DECL_PROC
		select case( tkGet( x ) )
		case TK_ID
			flags = frogAddDefine( tkText( x ), 0 )
			if( flags and DEFINE_CALL ) then
				calldefine = *tkText( x )
				hRemoveThisAndSpace( x )
			elseif( flags and DEFINE_EMPTY ) then
				hRemoveThisAndSpace( x )
			end if
		case KW_EXTERN, KW_STATIC
			hRemoveThisAndSpace( x )
		end select

	case DECL_VAR
		is_extern = FALSE

		select case( tkGet( x ) )
		case TK_ID
			if( frogAddDefine( tkText( x ), 0 ) ) then
				hRemoveThisAndSpace( x )
			end if
		case KW_EXTERN
			is_extern = TRUE
			x = hSkip( x )
		case KW_STATIC
			hRemoveThisAndSpace( x )
		end select

		if( is_extern = FALSE ) then
			'' DIM SHARED
			x = hInsertSpacedToken( x, KW_DIM, NULL )
			x = hInsertSpacedToken( x, KW_SHARED, NULL )
		end if

	case DECL_TYPEDEF
		'' TYPEDEF -> TYPE
		assert( tkGet( x ) = KW_TYPEDEF )
		tkRemove( x, x )
		tkInsert( x, KW_TYPE, NULL )
		x = hSkip( x )

	case DECL_PARAM
		'' Note: params only appear behind '(' or ','

		'' BYVAL
		tkInsert( x, KW_BYVAL, NULL )
		x = hSkip( x )

	end select

	is_procptr = hDeclIsProcptr( x )
	is_sub = hDeclTypeIsVoidOnly( x )

	'' procdecls: DECLARE {FUNCTION | SUB}
	'' typedef/var/field procptrs: AS {FUNCTION | SUB}
	'' The AS {FUNCTION | SUB} for procptr params is inserted later...
	if( (is_procptr and (decl <> DECL_PARAM)) or (decl = DECL_PROC) ) then
		'' DECLARE | AS
		x = hInsertSpacedToken( x, iif( is_procptr, KW_AS, KW_DECLARE ), NULL )

		'' FUNCTION | SUB
		x = hInsertSpacedToken( x, iif( is_sub, KW_SUB, KW_FUNCTION ), NULL )
	end if

	'' Type + pointers
	typebegin = x
	x = translatePtrs( translateBaseType( x ) )
	typeend = hSkipRev( x )

	'' Exclude space that might have been added from the type
	typebegin = hSkip( typebegin - 1 )

	'' '('?
	if( is_procptr ) then
		assert( tkGet( x ) = TK_LPAREN )
		hRemoveThisAndSpace( x )

		'' '*'
		assert( tkGet( x ) = TK_STAR )
		hRemoveThisAndSpace( x )
	end if

	'' id (optional for params, required elsewhere)
	if( tkGet( x ) = TK_ID ) then
		x = hSkip( x )
	else
		assert( decl = DECL_PARAM )
	end if

	if( len( calldefine ) > 0 ) then
		x = hInsertSpacedToken( x, TK_ID, calldefine )
	end if

	if( is_procptr and (decl = DECL_PARAM) ) then
		'' Move directly behind the id or the BYVAL (if there's no id)
		'' (could be space here)
		x = hSkipRev( x ) + 1

		'' AS {SUB | FUNCTION} for procptr params
		'' (inserted here instead of the front so it doesn't
		'' need to be moved here)

		tkInsertSpace( x )
		x += 1

		tkInsert( x, KW_AS, NULL )
		x += 1

		tkInsertSpace( x )
		x += 1

		tkInsert( x, iif( is_sub, KW_SUB, KW_FUNCTION ), NULL )
		x += 1

		'' Ensure to go back to the next non-space token
		x = hSkip( x - 1 )
	end if

	'' ')'
	if( is_procptr ) then
		assert( tkGet( x ) = TK_RPAREN )
		hRemoveThisAndSpace( x )
	end if

	'' '(' params ')' (recursion!)
	if( is_procptr or (decl = DECL_PROC) ) then
		x = translateParams( x )
	end if

	if( is_procptr or (decl = DECL_PROC) or (decl = DECL_PARAM) ) then
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
		x = hSkipRev( x ) + 1

		if( is_sub = FALSE ) then
			'' Append the type directly behind the id (for params),
			'' or ')' (for procdecls/procptrs), and insert a space too.

			tkInsertSpace( x )
			x += 1

			'' Copy the translated type/ptrs
			tkCopy( x, typebegin, typeend )
			x += (typeend - typebegin + 1)
		end if

		'' For all procptrs except params, the id must be moved to the
		'' end, that's easier than moving the params instead.
		if( is_procptr and (decl <> DECL_PARAM) ) then
			tkInsertSpace( x )
			x += 1

			'' Just extend the typeend to the id so it will be
			'' deleted below too...
			typeend = hSkip( typeend )

			'' Copy the id to the back
			assert( tkGet( typeend ) = TK_ID )
			tkCopy( x, typeend, typeend )
			x += 1

			'' Remove space behind the id below too
			''    as function ()              ->    as function()
			typeend = hSkip( typeend ) - 1
		end if

		'' Remove the old type + pointers + space in the front
		typebegin = hSkipRev( typebegin ) + 1
		tkRemove( typebegin, typeend )
		x -= (typeend - typebegin + 1)

		'' Ensure to go back to the next non-space token
		x = hSkip( x - 1 )
	else
		'' Not a procdecl/procptr/param:
		'' Can have more ids separated by commas.
		'' (',' id)*
		while( tkGet( x ) = TK_COMMA )
			x = hSkip( x )

			'' id
			assert( tkGet( x ) = TK_ID )
			x = hSkip( x )
		wend
	end if

	select case( decl )
	case DECL_PROC, DECL_VAR, DECL_FIELD, DECL_TYPEDEF
		'' ';'
		assert( tkGet( x ) = TK_SEMI )
		tkRemove( x, x )
		if( hIsWhitespaceUntilEol( x ) = FALSE ) then
			x = hInsertStatementSeparator( x )
		end if
		x = hSkip( x - 1 )

	end select

	function = x
end function

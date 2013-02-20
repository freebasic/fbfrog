#include once "fbfrog.bi"

private function cFindEOL( byval x as integer ) as integer
	while( tkIsStmtSep( x ) = FALSE )
		x += 1
	wend
	function = x
end function

private function cFindClosingParen( byval x as integer ) as integer
	dim as integer level = any, opening = any, closing = any

	opening = tkGet( x )
	level = 0
	select case( opening )
	case TK_LBRACE
		closing = TK_RBRACE
	case TK_LBRACKET
		closing = TK_RBRACKET
	case TK_LPAREN
		closing = TK_RPAREN
	case else
		return x
	end select

	do
		x += 1

		select case( tkGet( x ) )
		case opening
			level += 1

		case closing
			if( level = 0 ) then
				exit do
			end if

			level -= 1

		case -1
			exit do

		end select
	loop

	function = x
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' Remove all comments unless they're at EOL
sub cPurgeInlineComments( )
	dim as integer x = any

	x = 0
	do
		select case( tkGet( x ) )
		case TK_COMMENT
			if( tkIsStmtSep( x + 1 ) = FALSE ) then
				tkRemove( x, x )
				x -= 1
			end if

		case TK_EOF
			exit do
		end select

		x += 1
	loop
end sub

private function cPPDirective( byval x as integer ) as integer
	dim as integer begin = any

	'' '#'
	begin = x
	x += 1

	select case( tkGet( x ) )
	'' DEFINE Identifier ['(' ParameterList ')'] Body Eol .
	case KW_DEFINE
		'' DEFINE
		x += 1

		'' Identifier?
		if( tkGet( x ) <> TK_ID ) then
			return begin
		end if

		tkInsert( begin, TK_PPDEFINE, tkGetText( x ) )
		begin += 1
		tkRemove( begin, x )

		'' '(' and not separated from the Identifier with spaces?
		'' TODO

		'' Body
		x = cFindEOL( begin )
		tkSetFlags( begin, x, FLAG_PPDEFINE )

	case KW_INCLUDE
		'' INCLUDE
		x += 1

		if( tkGet( x ) <> TK_STRING ) then
			return begin
		end if

		tkInsert( begin, TK_PPINCLUDE, tkGetText( x ) )
		begin += 1
		tkRemove( begin, cFindEOL( begin ) )
		x = begin

	case else
		return begin
	end select

	function = x
end function

sub cPPDirectives( )
	dim as integer x = any, old = any

	x = 0
	do
		old = x

		select case( tkGet( x ) )
		'' '#'?
		case TK_HASH
			'' BOL?
			if( tkIsStmtSep( x - 1 ) ) then
				x = cPPDirective( x )
				if( x <> old ) then
					x -= 1
				end if
			end if

		case TK_EOF
			exit do
		end select

		x += 1
	loop
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#if 0

function cStructCompound _
	( _
		byval ast as ASTNODE ptr, _
		byval x as ASTNODE ptr _
	) as ASTNODE ptr

	dim as ASTNODE ptr begin = any, id = any, newstruct = any

	'' {STRUCT|UNION} [Identifier] '{'
	'' ...
	'' '}'
	begin = x

	'' {STRUCT|UNION}
	select case( astGet( x ) )
	case KW_STRUCT, KW_UNION

	case else
		return begin
	end select
	x = x->next

	'' [Identifier]
	if( astGet( x ) = TK_ID ) then
		id = x
		x = x->next
	else
		id = NULL
	end if

	'' '{'
	if( astGet( x ) <> TK_LBRACE ) then
		return begin
	end if

	newstruct = astNew( TK_STRUCT, id )

	bodybegin = x->next
	bodyend   = astFindLastBeforeClosingParen( x )

	x = bodyend->next

	'' '}'
	if( astGet( x ) = TK_RBRACE ) then
		x = x->next
	end if

	'' [';']
	if( astGet( x ) = TK_SEMI ) then
		x = x->next
	end if

	'' Insert a TK_STRUCT in place of the struct compound tokens,
	'' and copy the body into it

			astCloneInto( newtk, i, astFindLastInLine( i ) )

			astInsert( ast, newtk, begin )
			i = astRemoveUntilBehindEol( ast, begin )



	function
end function

sub cToplevel( byval ast as ASTNODE ptr )
	dim as ASTNODE ptr x = any, begin = any

	x = ast->head
	while( x )
		begin = x
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

enum
	DECL_PARAM = 0
	DECL_TOP
	DECL_TYPEDEF
	DECL_TYPEDEFSTRUCTBLOCK
	DECL_VAR
	DECL_FIELD
	DECL_PROC
end enum

'' Skips the token and any following whitespace
private function hSkipRaw _
	( _
		byval x as integer, _
		byval delta as integer _
	) as integer

	do
		x += delta

		select case( tkGet( x ) )
		case TK_EOL, TK_COMMENT, TK_LINECOMMENT, TK_TODO

		case else
			exit do
		end select
	loop

	function = x
end function

function hSkip( byval x as integer ) as integer
	function = hSkipRaw( x, 1 )
end function

function hSkipRev( byval x as integer ) as integer
	function = hSkipRaw( x, -1 )
end function

private function hSkipUnlessEolRaw _
	( _
		byval x as integer, _
		byval delta as integer _
	) as integer

	do
		x += delta

		select case( tkGet( x ) )
		case TK_COMMENT, TK_LINECOMMENT, TK_TODO

		case else
			exit do
		end select
	loop

	function = x
end function

function hSkipUnlessEol( byval x as integer ) as integer
	function = hSkipUnlessEolRaw( x, 1 )
end function

function hSkipRevUnlessEol( byval x as integer ) as integer
	function = hSkipUnlessEolRaw( x, -1 )
end function

function hIsWhitespaceUntilEol( byval x as integer ) as integer
	do
		select case( tkGet( x ) )
		case TK_EOL, TK_EOF
			exit do
		case TK_COMMENT, TK_LINECOMMENT, TK_TODO

		case else
			return FALSE
		end select

		x += 1
	loop
	function = TRUE
end function

function hSkipOptional _
	( _
		byval x as integer, _
		byval tk as integer _
	) as integer

	if( tkGet( x ) = tk ) then
		x = hSkip( x )
	end if

	function = x
end function

function hInsertStatementSeparator( byval x as integer ) as integer
	'' Only if there's not already another ':'
	if( (tkGet( x ) <> TK_COLON) and (tkGet( hSkipRev( x ) ) <> TK_COLON) ) then
		tkInsert( x, TK_COLON, NULL )
		x += 1
	end if
	function = x
end function

private function hInsertTodo _
	( _
		byval x as integer, _
		byval text as zstring ptr _
	) as integer

	dim as integer first = any, last = any

	first = hSkipRevUnlessEol( x )

	select case( tkGet( first ) )
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
		last = x - 1

		tkInsert( x, TK_TODO, text )
		x += 1

		tkInsert( x, TK_EOL, NULL )
		x += 1

		if( first <= last ) then
			tkCopy( x, first, last )
			x += last - first + 1
		end if
	case else
		'' Insert in the middle of the line
		tkInsert( x, TK_TODO, text )
		x += 1
	end select

	return x
end function

sub hRemoveThisAndSpace( byval x as integer )
	tkRemove( x, hSkip(x) - 1 )
end sub

function hFindToken( byval x as integer, byval tk as integer ) as integer
	dim as integer begin = any

	begin = x
	do
		select case( tkGet( x ) )
		case tk
			exit do
		case TK_EOF
			x = begin
			exit do
		end select

		x += 1
	loop

	function = x
end function

function hFindParentheses _
	( _
		byval x as integer, _
		byval delta as integer _
	) as integer

	dim as integer old = any, a = any, b = any, level = any

	old = x
	a = tkGet( x )
	b = -1

	if( delta < 0 ) then
		select case( a )
		case TK_RPAREN
			b = TK_LPAREN
		case TK_RBRACE
			b = TK_LBRACE
		case TK_RBRACKET
			b = TK_LBRACKET
		end select
	else
		select case( a )
		case TK_LPAREN
			b = TK_RPAREN
		case TK_LBRACE
			b = TK_RBRACE
		case TK_LBRACKET
			b = TK_RBRACKET
		end select
	end if

	if( b < 0 ) then
		return old
	end if

	level = 0
	do
		x = hSkipRaw( x, delta )

		select case( tkGet( x ) )
		case b
			if( level = 0 ) then
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

	function = x
end function

private function hSkipStatement( byval x as integer ) as integer
	dim as integer level = any

	level = 0
	do
		select case( tkGet( x ) )
		case TK_SEMI
			if( level = 0 ) then
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

		x = hSkip( x )
	loop

	function = hSkip( x )
end function

function parseUnknown( byval x as integer ) as integer
	dim as integer begin = any

	begin = x

	'' Find the next '#' or ';' while skipping over ';'s inside
	'' '{...}'.
	x = hSkipStatement( x )

	function = x
end function

private function hSkipPP( byval x as integer ) as integer
	'' Skip over current token like hSkip(), but stop at EOL, unless it's
	'' escaped (CPP line continuation).

	do
		x = hSkipUnlessEol( x )
	loop while( (tkGet( x ) = TK_EOL) and (tkGet( x - 1 ) = TK_BACKSLASH) )

	function = x
end function

private function hSkipPPDirective( byval x as integer ) as integer
	do
		x = hSkipPP( x )

		select case( tkGet( x ) )
		case TK_EOL, TK_EOF
			exit do
		end select
	loop
	function = x
end function

private function hIndentComment _
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
	dim as integer behindeol

	behindeol = FALSE
	for i as integer = 0 to len( oldtext )-1
		if( behindeol ) then
			newtext += prefix
		end if

		'' Check the current char, if's an EOL, we'll insert whitespace
		'' behind it later...
		select case( oldtext[i] )
		case &h0A '' LF
			behindeol = TRUE
		case &h0D '' CR
			if( (i + 1) < len( oldtext ) ) then
				behindeol = (oldtext[i+1] <> &h0A) '' CRLF
			else
				behindeol = TRUE
			end if
		case else
			behindeol = FALSE
		end select

		'' Copy current char
		newtext += chr( oldtext[i] )
	next

	if( behindeol ) then
		newtext += prefix
	end if

	function = newtext
end function

private sub hMergeFileIn _
	( _
		byref filename as string, _
		byval begin as integer, _
		byval x as integer _
	)

	dim as integer first = any, last = any, y = any
	dim as string text

	'' Remove the #include (but not the EOL behind it)
	assert( tkGet( begin ) = TK_HASH )
	tkRemove( begin, x - 1 )

	'' and insert the file content
	x = lexInsertFile( begin, filename )

	'' If the #include was indented, indent the whole inserted block too.
	first = hSkipRevUnlessEol( begin )
	select case( tkGet( first ) )
	case TK_EOL, TK_EOF
		'' Ok, there's only indentation in front of the #include.
		'' Copy it in front of every line of the inserted block.

		first += 1 '' (not the EOL in front of the #include)
		last = begin - 1

		'' Often there won't be any indentation, then there's nothing
		'' to do.
		if( first <= last ) then
			y = begin
			while( y < x )
				select case( tkGet( y ) )
				case TK_EOL
					y += 1
					tkCopy( y, first, last )
					y += last - first + 1
					x += last - first + 1

				case TK_COMMENT
					'' Reindent multiline comments too

					'' 1) Collect the whitespace into
					''    a string
					for i as integer = first to last
						text += *tkText( i )
					next

					'' 2) Prefix it to every line in the
					''    comment body
					text = hIndentComment( *tkText( y ), text )

					'' 3) Insert the new comment
					tkInsert( y, TK_COMMENT, text )
					y += 1

					'' 4) Remove the old one
					tkRemove( y, y )

				case else
					y += 1
				end select
			wend
		end if
	end select
end sub

private function parsePPDefineAttribute _
	( _
		byval x as integer, _
		byval pflags as uinteger ptr _
	) as integer

	dim as integer begin = any

	begin = x

	'' __attribute__
	if( tkGet( x ) <> TK_ID ) then
		return begin
	end if
	if( *tkText( x ) <> "__attribute__" ) then
		return begin
	end if
	x = hSkipPP( x )

	'' '(' '('
	for i as integer = 0 to 1
		if( tkGet( x ) <> TK_LPAREN ) then
			return begin
		end if
		x = hSkipPP( x )
	next

	'' id
	if( tkGet( x ) <> TK_ID ) then
		return begin
	end if
	select case( *tkText( x ) )
	case "cdecl", "stdcall", "__stdcall__"
		*pflags or= DEFINE_CALL
	case "dllexport", "dllimport"
		*pflags or= DEFINE_EMPTY
	case else
		return begin
	end select
	x = hSkipPP( x )

	function = x
end function

private function parsePPDefineDeclspec _
	( _
		byval x as integer, _
		byval pflags as uinteger ptr _
	) as integer

	dim as integer begin = any

	begin = x

	'' __declspec
	if( tkGet( x ) <> TK_ID ) then
		return begin
	end if
	if( *tkText( x ) <> "__declspec" ) then
		return begin
	end if
	x = hSkipPP( x )

	'' '('
	if( tkGet( x ) <> TK_LPAREN ) then
		return begin
	end if
	x = hSkipPP( x )

	'' id
	if( tkGet( x ) <> TK_ID ) then
		return begin
	end if
	select case( *tkText( x ) )
	case "dllexport", "dllimport"
		*pflags or= DEFINE_EMPTY
	case else
		return begin
	end select

	x = hSkipPP( x )

	function = x
end function

private function hDeterminePPDefineFlags( byval x as integer ) as uinteger
	dim as uinteger flags = any

	flags = 0

	'' Empty?
	if( tkGet( x ) = TK_EOL ) then
		flags or= DEFINE_EMPTY
	end if

	do
		x = parsePPDefineAttribute( x, @flags )
		x = parsePPDefineDeclspec( x, @flags )

		select case( tkGet( x ) )
		case TK_EOL
			exit do
		case TK_ID
			select case( *tkText( x ) )
			case "__stdcall"
				flags or= DEFINE_CALL
			end select
		end select

		x = hSkipPP( x )
	loop

	function = flags
end function

private function parseNestedStructBegin( byval x as integer ) as integer
	dim as integer begin = any

	'' {STRUCT|UNION} '{'
	select case( tkGet( x ) )
	case KW_STRUCT, KW_UNION

	case else
		return x
	end select

	begin = x
	x = hSkip( x )

	'' '{'
	if( tkGet( x ) <> TK_LBRACE ) then
		return begin
	end if

	function = hSkip( x )
end function

private function parseNestedStructEnd _
	( _
		byval x as integer, _
		byval toplevelopening as integer _
	) as integer

	dim as integer begin = any, opening = any

	begin = x

	'' '}'
	if( tkGet( x ) <> TK_RBRACE ) then
		return begin
	end if

	'' Find the opening '{', to determine whether this is a struct
	'' or a union. Bail out if there is no matching nested struct/union
	'' begin.
	opening = hFindParentheses( x, -1 )
	if( (opening = x) or (opening <= toplevelopening) ) then
		return begin
	end if
	opening = hSkipRev( opening )

	'' '}'
	assert( tkGet( x ) = TK_RBRACE )
	x = hSkip( x )

	'' [id]
	if( tkGet( x ) = TK_ID ) then
		x = hSkip( x )
	end if

	'' ';'
	if( tkGet( x ) <> TK_SEMI ) then
		return begin
	end if

	function = hSkip( x )
end function


function parseExternBegin( byval x as integer ) as integer
	dim as integer begin = any

	'' EXTERN "C" '{'

	if( tkGet( x ) <> KW_EXTERN ) then
		return x
	end if

	begin = x
	x = hSkip( x )

	'' "C"
	if( tkGet( x ) <> TK_STRING ) then
		return begin
	end if
	x = hSkip( x )

	'' '{'
	if( tkGet( x ) <> TK_LBRACE ) then
		return begin
	end if

	'' EXTERN parsing is done here, so the content is parsed from the
	'' toplevel loop.
	function = hSkip( x )
end function

function parseExternEnd( byval x as integer ) as integer
	dim as integer opening = any

	'' '}'
	if( tkGet( x ) <> TK_RBRACE ) then
		return x
	end if

	'' Check whether this '}' belongs to an 'extern "C" {'
	opening = hFindParentheses( x, -1 )
	if( opening = x ) then
		return x
	end if

	function = hSkip( x )
end function

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

sub cMultdecl( byval decl as integer )
	dim as integer typebegin = any, is_procptr = any, old = any

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

		x = hSkip( x )

	end select

	function = x
end function

sub cDeclarations( byval ast as ASTNODE ptr )
	dim as ASTNODE ptr i = any

	'' Toplevel declarations: global variables (including procedure
	'' pointers), procedure declarations, and also typedefs, since they
	'' are almost the same (parse_multdecl() disallows typedefs to
	'' procdecls, because those aren't possible FB).
	''    int a, *b, (*c)(), f(), *g();
	''    ...
	''
	'' [TYPEDEF|EXTERN|STATIC] multdecl

	i = ast->head
	while( i )
		decl = DECL_TOP

		select case( parseGet( ) )
		case KW_EXTERN, KW_STATIC
			parseNext( )

		case KW_TYPEDEF
			decl = DECL_TYPEDEF
			parseNext( )

		end select

		cMultDecl( decl )
	wend
end sub

#endif

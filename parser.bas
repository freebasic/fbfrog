#include once "parser.bi"

#if 0

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

function parsePPDirective _
	( _
		byval x as integer, _
		byval is_preparse as integer _
	) as integer

	dim as string filename
	dim as integer begin = any, y = any, lt = any
	dim as uinteger flags = any
	dim as zstring ptr text = any

	begin = x

	'' '#'?
	'' (Assuming all '#' are indicating a PP directive)
	if( tkGet( x ) <> TK_HASH ) then
		return begin
	end if
	x = hSkipPP( x )

	select case( tkGet( x ) )
	case KW_IF, KW_IFDEF, KW_IFNDEF, KW_ELIF '' #if & co

	case KW_INCLUDE '' #include
		y = hSkipPP( x )

		'' This will usually be one of:
		''
		'' #include <...>
		'' #include "..."
		'' #include SOME_DEFINE
		''
		'' FB doesn't support the <...> form, so it must be turned into
		'' a "..." string literal. This *should* be done by the C lexer,
		'' but it can't handle <...> as string literal. It's only a
		'' string literal in the context of #include which the lexer
		'' doesn't track. So, the tokens in <...> must be merged into
		'' a single string literal token, preventing the #include file
		'' name from being touched by the translation process.

		'' '<'?
		if( tkGet( y ) = TK_LT ) then
			lt = y
			do
				y += 1

				select case( tkGet( y ) )
				case TK_GT
					'' Replace <...> with "..."
					tkRemove( lt, y )
					tkInsert( lt, TK_STRING, filename )
					y = lt
					exit do

				case TK_EOL, TK_EOF
					filename = ""
					exit do

				end select

				text = tkText( y )
				if( text = NULL ) then
					text = token_text(tkGet( y ))
				end if
				filename += *text
			loop
		end if

		'' "..." followed by EOL?
		if( tkGet( y ) = TK_STRING ) then
			if( tkGet( hSkipPP( y ) ) = TK_EOL ) then
				filename = *tkText( y )
			end if
		end if

	case KW_DEFINE '' #define
		y = hSkipPP( x )
		if( tkGet( y ) = TK_ID ) then
			flags = hDeterminePPDefineFlags( hSkipPP( y ) )
			if( flags ) then
				frogAddDefine( tkText( y ), flags )
			end if
		end if

	end select

	y = x + 1
	x = hSkipPPDirective( x )

	if( len( filename ) > 0 ) then
		print "parser: include: ", filename
	end if

	'' In case of #include merge: hSkip whitespace at BOF
	'' Otherwise: hSkip EOL + other following whitespace
	x = hSkip( x - 1 )

	function = x
end function

#endif

'' Remove all comments unless they're at EOL
sub cPurgeInlineComments( byval ast as ASTNODE ptr )
	dim as ASTNODE ptr i = any

	i = ast->head
	while( i )

		if( i->id = TK_COMMENT ) then
			if( astIsStmtSep( i->next ) = FALSE ) then
				i = astRemove( ast, i )
				continue while
			end if
		end if

		i = i->next
	wend
end sub

'' '#' DEFINE Identifier ['(' ParameterList ')'] Body Eol .
private function cPPDefine _
	( _
		byval ast as ASTNODE ptr, _
		byval i as ASTNODE ptr _
	) as ASTNODE ptr

	dim as ASTNODE ptr begin = any, ppdefine = any

	'' '#' DEFINE
	begin = i
	i = i->next->next

	'' Identifier?
	if( astGet( i ) <> TK_ID ) then
		return begin
	end if
	ppdefine = astNew( TK_PPDEFINE, i->text )
	i = i->next

	'' '(' and not separated from the Identifier with spaces?
	'' TODO

	'' Body
	astCloneInto( ppdefine, i, astFindLastInLine( i ) )

	astInsert( ast, ppdefine, begin )
	astRemoveUntilBehindEol( ast, begin )

	function = ppdefine
end function

sub cParsePPDirectives( byval ast as ASTNODE ptr )
	dim as ASTNODE ptr i = any

	i = ast->head
	while( i )

		'' '#' at BOL?
		if( astIsAtBOL( i ) and (i->id = TK_HASH) ) then
			select case( astGet( i->next ) )
			case KW_DEFINE
				i = cPPDefine( ast, i )
				continue while

			case KW_INCLUDE
				if( astGet( i->next->next ) = TK_STRING ) then
					astInsert( ast, astNew( TK_PPINCLUDE, i->next->next->text ), i )

					'' '#' INCLUDE STRING
					i = astRemoveUntilBehindEol( ast, i )
					continue while
				end if
			end select
		end if

		i = i->next
	wend

end sub

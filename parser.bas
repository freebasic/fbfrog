#include once "parser.bi"

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

	'' The current token/construct couldn't be identified,
	'' so try to hSkip over the whole statement. Parsing
	'' needs to advance somehow, and everything here should
	'' be re-marked as MARK_UNKNOWN, so it can be marked
	'' with a /single/ TODO.
	begin = x

	'' Find the next '#' or ';' while skipping over ';'s inside
	'' '{...}'.
	x = hSkipStatement( x )
	tkSetMark( MARK_UNKNOWN, begin, hSkipRev( x ) )

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

private function hHandleInclude _
	( _
		byval begin as integer, _
		byval x as integer, _
		byref filename as string, _
		byval is_preparse as integer _
	) as integer

	dim as FROGFILE ptr f = any

	'' #includes are important for the preparse (which collects #include
	'' file names) and for merging, but nothing else.
	if( (is_preparse = FALSE) and (frog.merge = FALSE) ) then
		return x
	end if

	'' Lookup an existing file entry or find and add a new file entry
	'' For #includes, the file search always needs to be done, there is
	'' no other way to determine the full name.
	'' If this file is new, it will marked as "was found during preparse",
	'' which helps us deciding whether to translate it or not, depending
	'' on whether --follow was given or not. Normally we only have input
	'' files from the command line.
	f = frogAddFile( filename, is_preparse, TRUE )

	'' File not found? Not interesting for neither preparse nor merging...
	if( f = NULL ) then
		return x
	end if

	'' If this is for the preparse: increase the file's refcount, that's it.
	if( is_preparse ) then
		f->refcount += 1
		return x
	end if

	'' Otherwise, this must be for the normal parsing process with merging
	'' enabled.
	if( frogCanMerge( f ) ) then
		frogSetVisited( f )
		print "merging in: " + *f->softname

		'' This will remove the #include, insert the file content in
		'' its place, and we have to continue parsing at its beginning,
		'' which also is the former beginning of the #include.
		hMergeFileIn( *f->hardname, begin, x )
		x = begin
	end if

	function = x
end function

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
	dim as integer begin = any, mark = any, y = any, lt = any
	dim as uinteger flags = any
	dim as zstring ptr text = any

	begin = x

	'' '#'?
	'' (Assuming all '#' are indicating a PP directive)
	if( tkGet( x ) <> TK_HASH ) then
		return begin
	end if
	x = hSkipPP( x )

	'' Mark the expression parts of #if (but not #if itself) specially
	mark = MARK_PP
	select case( tkGet( x ) )
	case KW_IF, KW_IFDEF, KW_IFNDEF, KW_ELIF '' #if & co
		mark = MARK_PPEXPR

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

	tkSetMark( MARK_PP, begin, x )

	y = x + 1
	x = hSkipPPDirective( x )

	'' The last EOL is not part of the #directive, otherwise the EOL
	'' fixup would replace it with line continuation...
	tkSetMark( mark, y, x - 1 )

	if( len( filename ) > 0 ) then
		x = hHandleInclude( begin, x, filename, is_preparse )
	end if

	'' In case of #include merge: hSkip whitespace at BOF
	'' Otherwise: hSkip EOL + other following whitespace
	x = hSkip( x - 1 )

	function = x
end function

sub preparseToplevel( )
	dim as integer x = any, old = any
	x = hSkip( -1 )
	while( tkGet( x ) <> TK_EOF )
		old = x
		x = parsePPDirective( x, TRUE )
		if( x = old ) then
			x = hSkip( x )
		end if
	wend
end sub

'' Normally this will only be called once, but when concatenating, it's called
'' repeatedly to parse the appended files, so found #includes can be searched
'' for based on their parent, the current file.
sub parseToplevel( byval begin as integer )
	dim as integer x = any, old = any

	'' Skip space at begin-of-file
	x = hSkip( begin - 1 )

	do
		old = x

		x = parsePPDirective( x, FALSE )
		x = parseTopdeclOrTypedef( x )
		x = parseStruct( x )
		x = parseExternBegin( x )
		x = parseExternEnd( x )

		select case( tkGet( x ) )
		case TK_SEMI
			'' Random toplevel semicolon
			x = hSkip( x )
		case TK_EOF
			exit do
		end select

		if( x = old ) then
			x = parseUnknown( x )
		end if
	loop
end sub

'' EOL fixup -- in C it's possible to have constructs split over multiple
'' lines, which requires a '_' line continuation char in FB. Also, the CPP
'' \<EOL> line continuation needs to be converted to FB.
private function hFixUpEol( byval x as integer ) as integer
	dim as integer y = any

	select case( tkMark( x ) )
	case MARK_TOPLEVEL, MARK_UNKNOWN
		'' (EOLs at toplevel are supposed to stay)

	case MARK_PP, MARK_PPEXPR
		'' CPP uses <'\' EOL> for line continuation; translate that to
		'' <'_' EOL>, and add a space in front of '_' if needed,
		'' so it doesn't become part of some identifier/keyword.
		x -= 2

		'' Back to '\'
		x += 1

		'' Replace the '\' by '_'
		assert( tkGet( x ) = TK_BACKSLASH )
		tkRemove( x, x )
		tkInsert( x, TK_UNDERSCORE, NULL )

		'' Back to EOL
		x += 1

	case else
		'' For EOLs inside constructs, add '_' so the line wrapping
		'' is preserved. Alternatively we could just remove the EOLs.
		y = x - 1

		'' If there is a line comment, then the '_' needs to be added
		'' in front of it.
		y += (tkGet( y ) = TK_LINECOMMENT)

		tkInsert( y + 1, TK_UNDERSCORE, NULL )
		x += 1

	end select

	function = x
end function

private sub hFixUpEols( )
	dim as integer x = any

	x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do
		case TK_EOL
			x = hFixUpEol( x )
		end select

		x += 1
	loop
end sub

private sub hRemoveNestedFBComments( byval s as zstring ptr )
	dim as ubyte ptr p = any, limit = any

	p = s
	limit = p + len( *s )
	while( p < limit )

		select case( p[0] )
		case asc( "'" )
			if( (p + 1) < limit ) then
				if( p[1] = asc( "/" ) ) then
					p[0] = asc( "?" )
				end if
			end if
		case asc( "/" )
			if( (p + 1) < limit ) then
				if( p[1] = asc( "'" ) ) then
					p[0] = asc( "?" )
				end if
			else
				p[0] = asc( "?" )
			end if
		end select

		p += 1
	wend
end sub

private sub hFixUpComments( )
	dim as integer x = any

	'' Remove /' and '/ inside comments, since those have meaning in FB

	x = 0
	do
		select case( tkGet( x ) )
		case TK_COMMENT
			hRemoveNestedFBComments( tkText( x ) )
		case TK_EOF
			exit do
		end select

		x += 1
	loop
end sub

private function hFixUpLogicalOperator _
	( _
		byval x as integer, _
		byval kwforpp as integer, _
		byval kwnormal as integer _
	) as integer

	dim as integer mark = any

	mark = tkMark( x )
	tkRemove( x, x )

	if( mark = MARK_PPEXPR ) then
		tkInsert( x, kwforpp, NULL )
	else
		tkInsert( x, kwnormal, NULL )
	end if
	x += 1

	function = x
end function

private function hReplaceOperator _
	(_
		byval x as integer, _
		byval tk1 as integer, _
		byval tk2 as integer _
	) as integer

	tkRemove( x, x )

	'' Inserting a keyword? That might need spaces too...
	if( tk1 > TK_ID ) then
		tkInsert( x, tk1, NULL )
		x += 1

		'' Inserting a double-token self-op? (e.g. '%=' -> MOD '=')
		if( tk2 >= 0 ) then
			assert( tkGet( x - 1 ) = tk1 )
			tkInsert( x, tk2, NULL )
			x = hSkip( x )
		else
			x = hSkip( x - 1 )
		end if
	else
		tkInsert( x, tk1, NULL )
		x = hSkip( x )
	end if

	function = x
end function

private function hFixUpOperator( byval x as integer ) as integer
	dim as integer is_pp = any, kw = any

	select case as const( tkGet( x ) )
	case TK_EXCL, TK_TILDE '' {'!' | '~'} -> NOT
		tkInsert( x, TK_TODO, "add parentheses around NOT (different precedence)" )
		x += 1
		x = hReplaceOperator( x, KW_NOT, -1 )

	case TK_EXCLEQ '' != -> <>
		x = hReplaceOperator( x, TK_LTGT, -1 )

	case TK_PERCENT '' % -> mod
		x = hReplaceOperator( x, KW_MOD, -1 )

	case TK_PERCENTEQ '' %= -> mod=
		x = hReplaceOperator( x, KW_MOD, TK_EQ )

	case TK_AMP '' & -> AND | @
		tkInsert( x, TK_TODO, "check whether & meant @ or AND" )
		x += 1

		select case( tkGet( hSkipRev( x ) ) )
		case TK_ID, _                   '' abc & x
		     TK_DECNUM, TK_HEXNUM, _    '' 123 & x
		     TK_OCTNUM, _
		     TK_RPAREN, _               '' ...) & x
		     TK_RBRACKET                '' ...] & x
			'' This is likely to be AND
			x = hReplaceOperator( x, KW_AND, -1 )

		case else
			'' (&x
			'' , &x
			'' etc., this is likely to be @
			x = hReplaceOperator( x, TK_AT, -1 )

		end select

	case TK_AMPEQ '' &= -> and=
		x = hReplaceOperator( x, KW_AND, TK_EQ )

	case TK_PLUSPLUS, TK_MINUSMINUS
		tkInsert( x, TK_TODO, "translate ++/--" )
		x += 1
		x = hSkip( x )

	case TK_LTLT '' << -> shl
		x = hReplaceOperator( x, KW_SHL, -1 )

	case TK_LTLTEQ '' <<= -> shl=
		x = hReplaceOperator( x, KW_SHL, TK_EQ )

	case TK_EQEQ '' == -> =
		x = hReplaceOperator( x, TK_EQ, -1 )

	case TK_GTGT '' >> -> shr
		x = hReplaceOperator( x, KW_SHR, -1 )

	case TK_GTGTEQ '' >>= -> shr=
		x = hReplaceOperator( x, KW_SHR, TK_EQ )

	case TK_QUEST '' ?
		'' TODO: should turn this into iif(), but that's not easy
		tkInsert( x, TK_TODO, "turn a?b:c into iif(a,b,c)" )
		x += 1
		x = hSkip( x )

	case TK_CIRCUMFLEX
		x = hReplaceOperator( x, KW_XOR, -1 )

	case TK_CIRCUMFLEXEQ
		x = hReplaceOperator( x, KW_XOR, TK_EQ )

	case TK_PIPE '' | -> or
		x = hReplaceOperator( x, KW_OR, -1 )

	case TK_PIPEEQ '' |= -> or=
		x = hReplaceOperator( x, KW_OR, TK_EQ )

	case TK_AMPAMP, TK_PIPEPIPE
		'' || -> orelse | or
		'' && -> andalso | and

		is_pp = (tkMark( x ) = MARK_PPEXPR)
		kw = -1

		select case( tkGet( x ) )
		case TK_AMPAMP
			kw = iif( is_pp, KW_AND, KW_ANDALSO )
		case TK_PIPEPIPE
			kw = iif( is_pp, KW_OR, KW_ORELSE )
		end select

		assert( kw >= 0 )
		x = hReplaceOperator( x, kw, -1 )

	case else
		x = hSkip( x )
	end select

	function = x
end function

private sub hFixUpOperators( )
	dim as integer x = any
	x = hSkip( -1 )
	while( tkGet( x ) <> TK_EOF )
		select case( tkMark( x ) )
		case MARK_TOPLEVEL, MARK_UNKNOWN, MARK_UNKNOWNENUMCONST
			x = hSkip( x )
		case else
			x = hFixUpOperator( x )
		end select
	wend
end sub

sub translateToplevel( )
	dim as integer x = any, typebegin = any

	x = hSkip( -1 )

	while( tkGet( x ) <> TK_EOF )
		select case as const( tkMark( x ) )
		case MARK_PP
			if( tkGet( hSkipPP( x ) ) = KW_PRAGMA) then
				'' Add TODO for #pragmas
				x = hInsertTodo( x, "somehow translate this #pragma if needed" )
			end if

			assert( tkGet( x ) = TK_HASH )
			x = hSkipPP( x )

			if( tkGet( x ) = KW_ELIF ) then
				'' #elif -> #elseif
				tkRemove( x, x )
				tkInsert( x, KW_ELSEIF, NULL )
			end if

			x = hSkip( hSkipPPDirective( x ) - 1 )

		case MARK_EXTERN
			assert( tkGet( x ) = KW_EXTERN )
			x = hSkip( x )
			assert( tkGet( x ) = TK_STRING )
			x = hSkip( x )
			assert( tkGet( x ) = TK_LBRACE )
			tkRemove( x, x )

			if( hIsWhitespaceUntilEol( hSkipRev( x ) + 1 ) = FALSE ) then
				x = hInsertStatementSeparator( x )
			end if

		case MARK_ENDEXTERN
			x = translateCompoundEnd( x, KW_EXTERN )

		case MARK_STRUCT
			x = translateStruct( x )

		case MARK_ENDENUM
			x = translateCompoundEnd( x, KW_ENUM )

		case MARK_ENDSTRUCT
			x = translateCompoundEnd( x, KW_TYPE )

		case MARK_ENDUNION
			x = translateCompoundEnd( x, KW_UNION )

		case MARK_ENUMCONST
			x = translateEnumconst( x )

		case MARK_TYPEDEF
			hFixUpMultdecl( hSkip( x ), x )
			x = translateDecl( x, DECL_TYPEDEF )

		case MARK_TOPDECL
			typebegin = x

			select case( tkGet( typebegin ) )
			case TK_ID
				if( frogAddDefine( tkText( typebegin ), 0 ) ) then
					typebegin = hSkip( typebegin )
				end if
			case KW_EXTERN, KW_STATIC
				typebegin = hSkip( typebegin )
			end select

			'' Split up combined var/proc declarations into separate declarations,
			'' as needed for FB, and mark them as vardecl/procdecl to let those
			'' translators finish the translation.
			hFixUpMultdecl( typebegin, x )

			'' Note: x doesn't advanced here, so that the /whole/
			'' former topdecl will be handled as vardecl/procdecl.

		case MARK_PROCDECL
			x = translateDecl( x, DECL_PROC )

		case MARK_VARDECL
			x = translateDecl( x, DECL_VAR )

		case MARK_FIELDDECL
			hFixUpMultdecl( x, x )

			'' fielddecls can be changed to procdecls (methods),
			'' in that case x doesn't advance here (as if this was
			'' a topdecl), so the decl will be handled as procdecl
			'' instead.
			if( tkMark( x ) = MARK_FIELDDECL ) then
				x = translateDecl( x, DECL_FIELD )
			end if

		case MARK_UNKNOWN, MARK_UNKNOWNENUMCONST
			'' Insert a TODO at the begin of this statement.
			assert( tkGet( x ) <> TK_EOL )

			x = hInsertTodo( x, "translate (sorry)" )

			if( tkMark( x ) = MARK_UNKNOWNENUMCONST ) then
				x = parseEnumconst( x, TRUE )
			else
				x = hSkipStatement( x )
			end if

		case else
			x = hSkip( x )
		end select
	wend

	hFixUpComments( )
	hFixUpOperators( )
	hFixUpEols( )
end sub

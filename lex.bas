'' C lexer

#include once "fbfrog.bi"

enum
	CH_BELL     = &h07  '' \a
	CH_BKSPC    = &h08  '' \b
	CH_TAB      = &h09  '' \t
	CH_LF       = &h0A  '' \n
	CH_VTAB     = &h0B  '' \v
	CH_FORMFEED = &h0C  '' \f
	CH_CR       = &h0D  '' \r
	CH_ESC      = &h1B

	CH_SPACE    = &h20
	CH_EXCL         '' !
	CH_DQUOTE       '' "
	CH_HASH         '' #
	CH_DOLLAR       '' $
	CH_PERCENT      '' %
	CH_AMP          '' &
	CH_QUOTE        '' '
	CH_LPAREN       '' (
	CH_RPAREN       '' )
	CH_STAR         '' *
	CH_PLUS         '' +
	CH_COMMA        '' ,
	CH_MINUS        '' -
	CH_DOT          '' .
	CH_SLASH        '' /

	CH_0, CH_1, CH_2, CH_3, CH_4, CH_5, CH_6, CH_7, CH_8, CH_9

	CH_COLON        '' :
	CH_SEMI         '' ;
	CH_LT           '' <
	CH_EQ           '' =
	CH_GT           '' >
	CH_QUEST        '' ?
	CH_AT           '' @

	CH_A, CH_B, CH_C, CH_D, CH_E, CH_F, CH_G
	CH_H, CH_I, CH_J, CH_K, CH_L, CH_M, CH_N, CH_O, CH_P
	CH_Q, CH_R, CH_S, CH_T, CH_U, CH_V, CH_W
	CH_X, CH_Y, CH_Z

	CH_LBRACKET     '' [
	CH_BACKSLASH    '' \
	CH_RBRACKET     '' ]
	CH_CIRC         '' ^
	CH_UNDERSCORE   '' _
	CH_GRAVE        '' `

	CH_L_A, CH_L_B, CH_L_C, CH_L_D, CH_L_E, CH_L_F, CH_L_G
	CH_L_H, CH_L_I, CH_L_J, CH_L_K, CH_L_L, CH_L_M, CH_L_N, CH_L_O, CH_L_P
	CH_L_Q, CH_L_R, CH_L_S, CH_L_T, CH_L_U, CH_L_V, CH_L_W
	CH_L_X, CH_L_Y, CH_L_Z

	CH_LBRACE       '' {
	CH_PIPE         '' |
	CH_RBRACE       '' }
	CH_TILDE        '' ~
end enum

type LEXSTUFF
	buffer		as ubyte ptr  '' File content buffer
	i		as ubyte ptr  '' Current char, will always be <= limit
	limit		as ubyte ptr  '' (end of buffer)

	x		as integer
	linenum		as integer

	kwhash		as THASH
end type

dim shared as LEXSTUFF lex

private sub hAddTextToken( byval tk as integer, byval begin as ubyte ptr )
	dim as integer old = any
	dim as uinteger hash = any
	dim as THASHITEM ptr item = any

	'' Insert a null terminator temporarily
	old = lex.i[0]
	lex.i[0] = 0

	'' Lookup C keyword
	hash = hashHash( begin )
	item = hashLookup( @lex.kwhash, begin, hash )

	'' Is it a C keyword?
	if( item->s ) then
		'' Then use the proper KW_* instead of TK_ID
		tkInsert( lex.x, cint( item->data ) )
	else
		'' TK_ID
		tkInsert( lex.x, tk, begin )
	end if
	tkSetLineNum( lex.x, lex.linenum )

	lex.i[0] = old
end sub

private sub hAddTodo( byval text as zstring ptr )
	tkInsert( lex.x, TK_TODO, text )
	tkSetLineNum( lex.x, lex.linenum )
end sub

private sub hReadBytes( byval tk as integer, byval length as integer )
	lex.i += length
	tkInsert( lex.x, tk )
	tkSetLineNum( lex.x, lex.linenum )
end sub

private sub hReadSpace( )
	dim as ubyte ptr begin = any

	begin = lex.i

	do
		lex.i += 1
	loop while( (lex.i[0] = CH_SPACE) and (lex.i[0] = CH_TAB) )

	hAddTextToken( TK_SPACE, begin )
end sub

private sub hReadLineComment( )
	dim as ubyte ptr begin = any
	dim as integer escaped = any

	'' Line comments, starting at the first '/' of '// foo...'
	'' The whole comment body except for the // will be put into the token.
	'' EOL remains a separate token. Escaped newlines ('\' [Spaces] EOL)
	'' means the comment continues on the next line.
	lex.i += 2
	begin = lex.i
	escaped = FALSE

	do
		select case( lex.i[0] )
		case CH_CR
			if( escaped = FALSE ) then
				exit do
			end if

			lex.i += 1
			if( lex.i[0] = CH_LF ) then	'' CRLF
				lex.i += 1
			end if

			lex.linenum += 1

		case CH_LF
			if( escaped = FALSE ) then
				exit do
			end if
			lex.linenum += 1

		case CH_SPACE, CH_TAB
			'' Spaces don't change escaped status
			'' (at least gcc/clang support spaces between \ and EOL)

		case CH_BACKSLASH
			escaped = TRUE

		case 0
			exit do

		case else
			escaped = FALSE
		end select

		lex.i += 1
	loop

	hAddTextToken( TK_COMMENT, begin )
end sub

private sub hReadComment( )
	dim as ubyte ptr begin = any
	dim as integer saw_end = any

	lex.i += 2
	begin = lex.i

	saw_end = FALSE
	do
		select case( lex.i[0] )
		case 0
			hAddTodo( "comment left open" )
			exit do

		case CH_STAR		'' *
			if( lex.i[1] = CH_SLASH ) then	'' */
				saw_end = TRUE
				exit do
			end if

		end select

		lex.i += 1
	loop

	hAddTextToken( TK_COMMENT, begin )

	if( saw_end ) then
		lex.i += 2
	end if
end sub

private sub hReadId( )
	dim as ubyte ptr begin = any

	'' Identifier/keyword parsing: sequences of a-z, A-Z, 0-9, _
	'' The current char is one of those already. The whole identifier
	'' will be stored into a TK_ID, or if it's a keyword the proper KW_*
	'' is used instead of TK_ID and the text is not stored.
	begin = lex.i

	do
		lex.i += 1

		select case as const( lex.i[0] )
		case CH_A   to CH_Z  , _
		     CH_L_A to CH_L_Z, _
		     CH_0   to CH_9  , _
		     CH_UNDERSCORE

		case else
			exit do
		end select
	loop

	hAddTextToken( TK_ID, begin )
end sub

private sub hReadNumber( )
	dim as ubyte ptr begin = any
	dim as integer digit = any, found_dot = any, numbase = any, id = any

	'' Number literal parsing starting at '0'-'9' or '.'
	'' These are covered:
	''    123
	''    .123       (float)
	''    123.123    (float)
	''    0xAABBCCDD (hexadecimal)
	''    010        (octal)
	'' There also is some simple float exponent and type suffix parsing.
	'' TODO: hex floats?
	''
	'' In case of oct/hex literals only the important part of the number
	'' literals is stored into the token, so it can easily be translated
	'' to FB, for example:
	''    0xAABBCCDD -> TK_HEXLIT, AABBCCDD -> &hAABBCCDD
	''    010        -> TK_OCTLIT, 10       -> &o10

	numbase = 10
	id = TK_DECNUM
	if( lex.i[0] = CH_0 ) then '' 0
		if( lex.i[1] = CH_L_X ) then '' 0x
			lex.i += 2
			numbase = 16
			id = TK_HEXNUM
		elseif( (lex.i[1] >= CH_0) and (lex.i[1] <= CH_9) ) then
			lex.i += 1
			numbase = 8
			id = TK_OCTNUM
		end if
	end if

	begin = lex.i
	found_dot = FALSE
	do
		digit = lex.i[0]

		if( digit = CH_DOT ) then
			'' Only one dot allowed
			if( found_dot ) then
				exit do
			end if
			found_dot = TRUE
		else
			select case as const( digit )
			case CH_A to CH_F
				digit -= (CH_A - 10)

			case CH_L_A to CH_L_F
				digit -= (CH_L_A - 10)

			case CH_0 to CH_9
				digit -= CH_0

			case else
				exit do
			end select

			'' Do not allow A-F in decimal numbers, etc.
			if( digit >= numbase ) then
				exit do
			end if
		end if

		lex.i += 1
	loop

	'' Exponent? (can be used even without fractional part, e.g. '1e1')
	select case( lex.i[0] )
	case CH_E, CH_L_E   '' 'E', 'e'
		lex.i += 1

		'' ['+' | '-']
		select case( lex.i[0] )
		case CH_PLUS, CH_MINUS
			lex.i += 1
		end select

		'' ['0'-'9']*
		while( (lex.i[0] >= CH_0) and (lex.i[0] <= CH_9) )
			lex.i += 1
		wend

	end select

	'' Type suffixes
	if( found_dot ) then
		select case( lex.i[0] )
		case CH_F, CH_L_F, _    '' 'F' | 'f'
		     CH_D, CH_L_D       '' 'D' | 'd'
			lex.i += 1
		end select
	else
		select case( lex.i[0] )
		case CH_U, CH_L_U       '' 'U' | 'u'
			lex.i += 1
		end select

		select case( lex.i[0] )
		case CH_L, CH_L_L       '' 'L' | 'l'
			lex.i += 1
			select case( lex.i[0] )
			case CH_L, CH_L_L       '' 'L' | 'l'
				lex.i += 1
			end select
		end select
	end if

	hAddTextToken( id, begin )
end sub

enum
	STRFLAG_CHAR = &b001 '' is char literal?
	STRFLAG_WIDE = &b010 '' is wide (wchar) literal?
	STRFLAG_ESC  = &b100 '' contains escape sequences?
end enum

private sub hReadString( )
	dim as ubyte ptr begin = any
	dim as integer saw_end = any, id = any, strflags = any, quotechar = any

	'' String/char literal parsing, starting at ", ', or L, covering:
	''    'a'
	''    "foo"
	''    L"foo"
	'' The string content is stored into the token, but not the quotes.
	'' If escape sequences are found, we'll generate a TK_ESTRING instead
	'' of TK_STRING, so the emitter can prepend an '!', for example:
	''    "\n" -> \n -> !"\n"

	strflags = 0

	id = TK_STRING
	if( lex.i[0] = CH_L ) then
		lex.i += 1
		strflags or= STRFLAG_WIDE
	end if

	quotechar = lex.i[0]
	select case( quotechar )
	case CH_QUOTE
		strflags or= STRFLAG_CHAR
	case CH_LT
		quotechar = CH_GT
	end select

	lex.i += 1
	begin = lex.i
	saw_end = FALSE
	do
		select case( lex.i[0] )
		case quotechar
			saw_end = TRUE
			exit do

		case CH_LF, CH_CR, 0
			hAddTodo( "string/char literal left open" )
			exit do

		case CH_BACKSLASH	'' \
			strflags or= STRFLAG_ESC
			select case( lex.i[0] )
			case CH_BACKSLASH, _ '' \\
			     quotechar       '' \" | \'
				lex.i += 1
			end select

		end select

		lex.i += 1
	loop

	if( strflags ) then
		if( strflags and STRFLAG_CHAR ) then
			hAddTodo( "char literal" )
		else
			hAddTodo( "non-trivial string literal" )
		end if
	end if

	hAddTextToken( id, begin )

	if( saw_end ) then
		lex.i += 1
	end if
end sub

private sub lexNext( )
	dim as integer y = any

	'' Identify the next token
	select case as const( lex.i[0] )
	case CH_CR
		hReadBytes( TK_EOL, 1 )

		if( lex.i[0] = CH_LF ) then	'' CRLF
			lex.i += 1
		end if

		lex.linenum += 1

	case CH_LF
		hReadBytes( TK_EOL, 1 )
		lex.linenum += 1

	case CH_TAB, CH_SPACE
		hReadSpace( )

	case CH_EXCL		'' !
		if( lex.i[1] = CH_EQ ) then	'' !=
			hReadBytes( TK_EXCLEQ, 2 )
		else
			hReadBytes( TK_EXCL, 1 )
		end if

	case CH_DQUOTE		'' "
		hReadString( )

	case CH_HASH		'' #
		if( lex.i[1] = CH_HASH ) then	'' ##
			hReadBytes( TK_HASHHASH, 2 )
		else
			hReadBytes( TK_HASH, 1 )
		end if

	case CH_PERCENT		'' %
		if( lex.i[1] = CH_EQ ) then	'' %=
			hReadBytes( TK_PERCENTEQ, 2 )
		else
			hReadBytes( TK_PERCENT, 1 )
		end if

	case CH_AMP		'' &
		select case( lex.i[1] )
		case CH_AMP	'' &&
			hReadBytes( TK_AMPAMP, 2 )
		case CH_EQ	'' &=
			hReadBytes( TK_AMPEQ, 2 )
		case else
			hReadBytes( TK_AMP, 1 )
		end select

	case CH_QUOTE		'' '
		hReadString( )

	case CH_LPAREN		'' (
		hReadBytes( TK_LPAREN, 1 )

	case CH_RPAREN		'' )
		hReadBytes( TK_RPAREN, 1 )

	case CH_STAR		'' *
		if( lex.i[1] = CH_EQ ) then	'' *=
			hReadBytes( TK_STAREQ, 2 )
		else
			hReadBytes( TK_STAR, 1 )
		end if

	case CH_PLUS		'' +
		select case( lex.i[1] )
		case CH_PLUS	'' ++
			hReadBytes( TK_PLUSPLUS, 2 )
		case CH_EQ	'' +=
			hReadBytes( TK_PLUSEQ, 2 )
		case else
			hReadBytes( TK_PLUS, 1 )
		end select

	case CH_COMMA		'' ,
		hReadBytes( TK_COMMA, 1 )

	case CH_MINUS		'' -
		select case( lex.i[1] )
		case CH_GT	'' ->
			hReadBytes( TK_ARROW, 2 )
		case CH_MINUS	'' --
			hReadBytes( TK_MINUSMINUS, 2 )
		case CH_EQ	'' -=
			hReadBytes( TK_MINUSEQ, 2 )
		case else
			hReadBytes( TK_MINUS, 1 )
		end select

	case CH_DOT		'' .
		select case( lex.i[1] )
		case CH_0 to CH_9   '' 0-9 (Decimal float beginning with '.')
			hReadNumber( )
		case CH_DOT
			if( lex.i[2] = CH_DOT ) then	'' ...
				hReadBytes( TK_ELLIPSIS, 3 )
			else
				hReadBytes( TK_DOT, 1 )
			end if
		case else
			hReadBytes( TK_DOT, 1 )
		end select

	case CH_SLASH		'' /
		select case( lex.i[1] )
		case CH_EQ	'' /=
			hReadBytes( TK_SLASHEQ, 2 )
		case CH_SLASH	'' //
			hReadLineComment( )
		case CH_STAR	'' /*
			hReadComment( )
		case else
			hReadBytes( TK_SLASH, 1 )
		end select

	case CH_0 to CH_9	'' 0 - 9
		hReadNumber( )

	case CH_COLON		'' :
		hReadBytes( TK_COLON, 1 )

	case CH_SEMI		'' ;
		hReadBytes( TK_SEMI, 1 )

	case CH_LT		'' <
		select case( lex.i[1] )
		case CH_LT	'' <<
			if( lex.i[2] = CH_EQ ) then	'' <<=
				hReadBytes( TK_LTLTEQ, 3 )
			else
				hReadBytes( TK_LTLT, 2 )
			end if
		case CH_EQ	'' <=
			hReadBytes( TK_LTEQ, 2 )
		case else
			'' If it's an #include, parse <...> as string literal
			if( tkGet( lex.x ) = KW_INCLUDE ) then
				y = tkSkipSpaceAndComments( lex.x, -1 )
				if( tkGet( y ) = TK_HASH ) then
					y = tkSkipSpaceAndComments( y, -1 )
					select case( tkGet( y ) )
					case TK_EOL, TK_EOF
						hReadString( )
						exit select, select
					end select
				end if
			end if

			hReadBytes( TK_LT, 1 )
		end select

	case CH_EQ		'' =
		if( lex.i[1] = CH_EQ ) then	'' ==
			hReadBytes( TK_EQEQ, 2 )
		else
			hReadBytes( TK_EQ, 1 )
		end if

	case CH_GT		'' >
		select case( lex.i[1] )
		case CH_GT	'' >>
			if( lex.i[2] = CH_EQ ) then	'' >>=
				hReadBytes( TK_GTGTEQ, 3 )
			else
				hReadBytes( TK_GTGT, 2 )
			end if
		case CH_EQ	'' >=
			hReadBytes( TK_GTEQ, 2 )
		case else
			hReadBytes( TK_GT, 1 )
		end select

	case CH_QUEST	'' ?
		hReadBytes( TK_QUEST, 1 )

	case CH_A       to (CH_L - 1), _	'' A-Z except L
	     (CH_L + 1) to CH_Z
		hReadId( )

	case CH_L		'' L
		if( lex.i[1] = CH_DQUOTE ) then
			hReadString( )
		else
			hReadId( )
		end if

	case CH_LBRACKET	'' [
		hReadBytes( TK_LBRACKET, 1 )

	case CH_BACKSLASH	'' \
		hReadBytes( TK_BACKSLASH, 1 )

	case CH_RBRACKET	'' ]
		hReadBytes( TK_RBRACKET, 1 )

	case CH_CIRC		'' ^
		if( lex.i[1] = CH_EQ ) then	'' ^=
			hReadBytes( TK_CIRCEQ, 2 )
		else
			hReadBytes( TK_CIRC, 1 )
		end if

	case CH_L_A to CH_L_Z, _	'' a-z
	     CH_UNDERSCORE		'' _
		hReadId( )

	case CH_LBRACE		'' {
		hReadBytes( TK_LBRACE, 1 )

	case CH_PIPE		'' |
		select case( lex.i[1] )
		case CH_PIPE	'' ||
			hReadBytes( TK_PIPEPIPE, 2 )
		case CH_EQ	'' |=
			hReadBytes( TK_PIPEEQ, 2 )
		case else
			hReadBytes( TK_PIPE, 1 )
		end select

	case CH_RBRACE		'' }
		hReadBytes( TK_RBRACE, 1 )

	case CH_TILDE		'' ~
		hReadBytes( TK_TILDE, 1 )

	case else
		hAddTodo( "unusual character" )
		lex.i += 1
		hAddTextToken( TK_BYTE, lex.i - 1 )

	end select
end sub

private sub hLoadFile( byref filename as string )
	dim as integer f = any, found = any, result = any, size = any
	dim as longint filesize = any

	'' Read in the whole file content into lex.buffer
	f = freefile( )
	if( open( filename, for binary, access read, as #f ) ) then
		oops( "could not open file: '" + filename + "'" )
	end if

	filesize = lof( f )
	if( filesize > &h40000000 ) then
		oops( "a header file bigger than 1 GiB? no way..." )
	end if

	'' An extra 0 byte at the end of the buffer so we can look ahead
	'' without bound checks, and don't need to give special treatment
	'' to empty files
	size = filesize
	lex.buffer = callocate( size + 1 )
	lex.buffer[size] = 0
	lex.i = lex.buffer
	lex.limit = lex.buffer + size

	if( size > 0 ) then
		found = 0
		result = get( #f, , *lex.buffer, size, found )
		if( result or (found <> size) ) then
			oops( "file I/O failed" )
		end if
	end if

	close #f
end sub

private sub hComplainAboutEmbeddedNulls( )
	'' Currently tokens store text as null-terminated strings, so they
	'' can't allow embedded nulls, and null also indicates EOF to the lexer.
	while( lex.i < lex.limit )
		if( lex.i[0] = 0 ) then
			oops( "file has embedded nulls, please fix that first!" )
		end if
		lex.i += 1
	wend
	lex.i = lex.buffer
end sub

private sub hInitKeywords( )
	static as integer lazy = FALSE
	dim as uinteger hash = any
	dim as THASHITEM ptr item = any

	'' Load C keywords if not yet done
	if( lazy ) then
		exit sub
	end if
	lazy = TRUE

	hashInit( @lex.kwhash, 10 )

	for i as integer = KW_AUTO to KW_WHILE
		hash = hashHash( tkInfoText( i ) )
		item = hashLookup( @lex.kwhash, tkInfoText( i ), hash )
		hashAdd( @lex.kwhash, item, hash, tkInfoText( i ), cast( any ptr, i ) )
	next
end sub

function lexLoadFile( byval x as integer, byref filename as string ) as integer
	dim as integer count = any

	count = 0
	lex.x = x
	lex.linenum = 1
	hLoadFile( filename )
	hComplainAboutEmbeddedNulls( )
	hInitKeywords( )

	'' Tokenize and insert into tk buffer
	while( lex.i < lex.limit )
		lexNext( )
		lex.x += 1
		count += 1
	wend

	if( frog.verbose ) then
		print "  lex: " & cuint( lex.limit ) - cuint( lex.buffer ) & _
			" bytes -> " & count & " tokens"
	end if

	deallocate( lex.buffer )
	function = lex.x
end function

''
'' C lexer
''
'' Call lexLoadFile() to insert tokens corresponding to the content of a
'' certain file into the tk buffer.
''
'' lexLoadFile() loads the whole file into a buffer and then repeatedly calls
'' lexNext() to parse it into tokens which are added to tk through tkInsert().
''

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

	CH_DEL
end enum

type LEXSTUFF
	buffer		as ubyte ptr  '' File content buffer
	i		as ubyte ptr  '' Current char, will always be <= limit
	bol		as ubyte ptr  '' Last begin-of-line
	limit		as ubyte ptr  '' (end of buffer)

	x		as integer
	location	as TKLOCATION
	behindspace	as integer
	filename	as string
	fb_mode		as integer    '' C or FB?
	keep_comments	as integer    '' Whether to ignore comments or produce TK_COMMENTs

	fbkwhash	as THASH
	ckwhash		as THASH
	kwhash		as THASH ptr  '' Points to keywords hash tb for current mode, C or FB
end type

dim shared as LEXSTUFF lex

private sub lexOops( byref message as string )
	oopsLocation( @lex.location, message )
end sub

private sub hSetLocation( )
	lex.location.length = (lex.i - lex.bol) - lex.location.column
	tkSetLocation( lex.x, @lex.location )
	if( lex.behindspace ) then
		tkSetBehindSpace( lex.x )
	end if
end sub

private sub hAddTextToken( byval tk as integer, byval begin as ubyte ptr )
	'' Insert a null terminator temporarily
	var old = lex.i[0]
	lex.i[0] = 0

	'' Lookup C keyword
	var hash = hashHash( begin )
	var item = hashLookup( lex.kwhash, begin, hash )

	'' Is it a C keyword?
	if( item->s ) then
		'' Then use the proper KW_* instead of TK_ID
		tkInsert( lex.x, cint( item->data ) )
	else
		'' TK_ID
		tkInsert( lex.x, tk, begin )
	end if
	hSetLocation( )
	lex.x += 1

	lex.i[0] = old
end sub

private sub hReadBytes( byval tk as integer, byval length as integer )
	lex.i += length
	tkInsert( lex.x, tk )
	hSetLocation( )
	lex.x += 1
end sub

private sub hNewLine( )
	lex.bol = lex.i
	lex.location.linenum += 1
end sub

private sub hReadLineComment( )
	'' Line comments:
	''    'abcd
	''    //abcd
	'' The whole comment body except for the ' or // is put into the
	'' TK_COMMENT. The EOL behind the comment remains a separate token.
	'' In C mode the comment may contain escaped newlines ('\' [Spaces] EOL)
	'' which means the comment continues on the next line.

	lex.i += 2
	var begin = lex.i
	var escaped = FALSE

	do
		select case( lex.i[0] )
		case 0
			exit do

		case CH_CR
			if( escaped = FALSE ) then
				exit do
			end if

			if( lex.i[1] = CH_LF ) then	'' CRLF
				lex.i += 1
			end if
			lex.i += 1
			hNewLine( )

		case CH_LF
			if( escaped = FALSE ) then
				exit do
			end if
			lex.i += 1
			hNewLine( )

		case CH_SPACE, CH_TAB
			'' Spaces don't change escaped status
			'' (at least gcc/clang support spaces between \ and EOL)
			lex.i += 1

		case CH_BACKSLASH
			escaped = not lex.fb_mode
			lex.i += 1

		case else
			escaped = FALSE
			lex.i += 1
		end select
	loop

	if( lex.keep_comments ) then
		hAddTextToken( TK_COMMENT, begin )
	end if
end sub

private sub hReadComment( )
	var quote = lex.i[1]  '' ' (FB mode's /' '/) or * (C mode's /* */)
	lex.i += 2
	var begin = lex.i

	var saw_end = FALSE
	do
		select case( lex.i[0] )
		case 0
			lexOops( "comment left open" )

		case quote		'' ' or *
			if( lex.i[1] = CH_SLASH ) then	'' '/ or */
				saw_end = TRUE
				exit do
			end if
			lex.i += 1

		case CH_CR
			if( lex.i[1] = CH_LF ) then	'' CRLF
				lex.i += 1
			end if
			lex.i += 1
			hNewLine( )

		case CH_LF
			lex.i += 1
			hNewLine( )

		case else
			lex.i += 1
		end select
	loop

	if( lex.keep_comments ) then
		hAddTextToken( TK_COMMENT, begin )
	end if

	if( saw_end ) then
		lex.i += 2
	end if
end sub

private sub hReadId( )
	'' Identifier/keyword parsing: sequences of a-z, A-Z, 0-9, _
	'' The current char is one of those already. The whole identifier
	'' will be stored into a TK_ID, or if it's a keyword the proper KW_*
	'' is used instead of TK_ID and the text is not stored.
	var begin = lex.i

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

	var numbase = 10
	var id = TK_DECNUM
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

	var begin = lex.i
	var found_dot = FALSE
	do
		dim as integer digit = lex.i[0]

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

		'' The exponent makes this a float too
		found_dot = TRUE
	end select

	'' Type suffixes
	if( found_dot ) then
		if( id <> TK_DECNUM ) then
			lexOops( "non-decimal floats not supported" )
		end if
		id = TK_DECFLOAT

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
	'' String/char literal parsing, starting at ", ', or L, covering:
	''    'a'
	''    "foo"
	''    L"foo"
	'' The string content is stored into the token, but not the quotes.
	'' If escape sequences are found, we'll generate a TK_ESTRING instead
	'' of TK_STRING, so the emitter can prepend an '!', for example:
	''    "\n" -> \n -> !"\n"

	var id = TK_STRING
	var strflags = 0

	if( lex.i[0] = CH_L ) then
		lex.i += 1
		strflags or= STRFLAG_WIDE
	end if

	var quotechar = lex.i[0]
	select case( quotechar )
	case CH_QUOTE
		strflags or= STRFLAG_CHAR
	case CH_LT
		quotechar = CH_GT
	end select

	lex.i += 1
	var begin = lex.i
	var saw_end = FALSE
	do
		select case( lex.i[0] )
		case quotechar
			saw_end = TRUE
			exit do

		case CH_LF, CH_CR, 0
			lexOops( "string/char literal left open" )

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
			id = TK_CHAR
		end if
	end if

	hAddTextToken( id, begin )

	if( saw_end ) then
		lex.i += 1
	end if
end sub

private sub lexNext( )
	'' Skip spaces
	lex.behindspace = FALSE
	while( (lex.i[0] = CH_TAB) or (lex.i[0] = CH_SPACE) )
		lex.i += 1
		lex.behindspace = TRUE
	wend

	lex.location.column = lex.i - lex.bol
	lex.location.length = 1

	'' Identify the next token
	select case as const( lex.i[0] )
	case CH_CR
		if( lex.i[1] = CH_LF ) then	'' CRLF
			lex.i += 1
		end if
		hReadBytes( TK_EOL, 1 )
		hNewLine( )

	case CH_LF
		hReadBytes( TK_EOL, 1 )
		hNewLine( )

	case CH_FORMFEED
		hReadBytes( TK_DIVIDER, 1 )

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
		if( lex.fb_mode ) then
			hReadLineComment( )
		else
			hReadString( )
		end if

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
			if( lex.fb_mode ) then
				hReadBytes( TK_SLASH, 1 )
			else
				hReadLineComment( )
			end if
		case CH_QUOTE	'' /'
			if( lex.fb_mode ) then
				hReadComment( )
			else
				hReadBytes( TK_SLASH, 1 )
			end if
		case CH_STAR	'' /*
			if( lex.fb_mode ) then
				hReadBytes( TK_SLASH, 1 )
			else
				hReadComment( )
			end if
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
		case CH_GT	'' <>
			hReadBytes( TK_LTGT, 2 )
		case else
			'' If it's an #include, parse <...> as string literal
			var y = lex.x
			y = tkSkipComment( y, -1 )
			if( tkGet( y ) = KW_INCLUDE ) then
				y = tkSkipComment( y, -1 )
				if( tkGet( y ) = TK_HASH ) then
					y = tkSkipComment( y, -1 )
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
		'' Check for escaped EOLs and solve them out
		'' '\' [Space] EOL

		var i = 1
		while( (lex.i[i] = CH_TAB) or (lex.i[i] = CH_SPACE) )
			i += 1
		wend

		var found_eol = FALSE
		select case( lex.i[i] )
		case CH_CR
			i += 1
			if( lex.i[i] = CH_LF ) then	'' CRLF
				i += 1
			end if
			found_eol = TRUE
		case CH_LF
			i += 1
			found_eol = TRUE
		end select

		if( found_eol ) then
			lex.i += i
			hNewLine( )
		else
			hReadBytes( TK_BACKSLASH, 1 )
		end if

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
		lexOops( "stray &h" + hex( lex.i[0], 2 ) + " byte" )

	end select
end sub

private sub hLoadFile _
	( _
		byval file as FROGFILE ptr, _
		byref buffer as ubyte ptr, _
		byref limit as ubyte ptr _
	)

	'' Read in the whole file content into lex.buffer
	var f = freefile( )
	if( open( file->normed, for binary, access read, as #f ) ) then
		oops( "could not open file: '" + file->normed + "'" )
	end if

	var filesize = lof( f )
	if( filesize > &h40000000 ) then
		oops( "a header file bigger than 1 GiB? no way..." )
	end if

	'' An extra 0 byte at the end of the buffer so we can look ahead
	'' without bound checks, and don't need to give special treatment
	'' to empty files
	dim as integer size = filesize
	buffer = callocate( size + 1 )
	buffer[size] = 0
	limit = buffer + size

	if( size > 0 ) then
		var found = 0
		var result = get( #f, , *buffer, size, found )
		if( result or (found <> size) ) then
			oops( "file I/O failed" )
		end if
	end if

	close #f
end sub

private sub hComplainAboutEmbeddedNulls _
	( _
		byval file as FROGFILE ptr, _
		byval buffer as ubyte ptr, _
		byval limit as ubyte ptr _
	)

	'' Currently tokens store text as null-terminated strings, so they
	'' can't allow embedded nulls, and null also indicates EOF to the lexer.
	var i = buffer
	while( i < limit )
		if( i[0] = 0 ) then
			oops( file->pretty + ": file has embedded nulls, please fix that first!" )
		end if
		i += 1
	wend

end sub

private sub hInitKeywords( )
	'' Load keywords if not yet done, and switch between C/FB keyword
	'' hash tables to match the requested mode.

	dim as integer first, last
	if( lex.fb_mode ) then
		lex.kwhash = @lex.fbkwhash
		first = KW__FB_FIRST
		last = KW__FB_LAST
	else
		lex.kwhash = @lex.ckwhash
		first = KW__C_FIRST
		last = KW__C_LAST
	end if

	if( lex.kwhash->items = NULL ) then
		hashInit( lex.kwhash, 12 )
	end if

	for i as integer = first to last
		var hash = hashHash( tkInfoText( i ) )
		var item = hashLookup( lex.kwhash, tkInfoText( i ), hash )
		hashAdd( lex.kwhash, item, hash, tkInfoText( i ), cast( any ptr, i ) )
	next
end sub

function lexLoadFile _
	( _
		byval x as integer, _
		byval file as FROGFILE ptr, _
		byval fb_mode as integer, _
		byval keep_comments as integer _
	) as integer

	lex.x = x
	lex.location.file = file
	lex.location.linenum = 0
	lex.fb_mode = fb_mode
	lex.keep_comments = keep_comments
	hLoadFile( file, lex.buffer, lex.limit )
	lex.i = lex.buffer
	lex.bol = lex.i
	hComplainAboutEmbeddedNulls( file, lex.buffer, lex.limit )
	hInitKeywords( )

	'' Tokenize and insert into tk buffer
	while( lex.i < lex.limit )
		lexNext( )
	wend

	#if 0
	if( frog.verbose ) then
		print "  lex: " & cuint( lex.limit ) - cuint( lex.buffer ) & _
			" bytes -> " & lex.x - x & " tokens"
	end if
	#endif

	file->linecount = lex.location.linenum + 1

	deallocate( lex.buffer )
	function = lex.x
end function

'' Retrieve a line of source code from the original input file for display in
'' error messages.
function lexPeekLine _
	( _
		byval file as FROGFILE ptr, _
		byval targetlinenum as integer _
	) as string

	dim as ubyte ptr buffer, limit
	hLoadFile( file, buffer, limit )
	hComplainAboutEmbeddedNulls( file, buffer, limit )

	'' Find the targetlinenum'th line of code, bol will end up pointing
	'' to the begin of line of the target line, i will point to the end of
	'' the target line.
	var i = buffer
	var bol = i
	var linenum = 0
	do
		select case( i[0] )
		case 0, CH_LF, CH_CR  '' EOF or EOL
			if( linenum = targetlinenum ) then
				exit do
			end if

			select case( i[0] )
			case CH_CR
				if( i[1] = CH_LF ) then '' CRLF
					i += 1
				end if
			case 0
				exit do
			end select

			i += 1
			linenum += 1
			bol = i

		case else
			i += 1
		end select
	loop

	'' Turn the target line of code into a pretty string
	dim s as string
	while( bol < i )
		dim as integer ch = bol[0]

		'' Replace nulls, tabs, and other control chars with spaces
		select case( ch )
		case is < CH_SPACE, CH_DEL
			ch = CH_SPACE
		end select

		s += chr( ch )

		bol += 1
	wend

	function = s
end function

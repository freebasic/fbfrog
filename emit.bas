'' Token emitter

#include once "fbfrog.bi"
#include once "parser.bi"

type EmitterStuff
	fo		as integer '' Output file
	todocount	as integer
	filecount	as integer
end type

dim shared as EmitterStuff stuff

private sub emitInit( byref filename as string )
	stuff.fo = freefile( )
	if( open( filename, for binary, access write, as #stuff.fo ) ) then
		oops( "could not open output file: '" + filename + "'" )
	end if
end sub

private sub emitEnd( )
	close #stuff.fo
	stuff.fo = 0
end sub

private sub emit( byval text as zstring ptr )
	dim as integer length = any
	length = len( *text )
	if( put( #stuff.fo, , *cptr( ubyte ptr, text ), length ) ) then
		oops( "file I/O failed" )
	end if
end sub

private sub emitToken( byval x as integer )
	dim as integer linecomment = any
	dim as zstring ptr text = any

	select case as const( tkGet( x ) )
	case TK_EOL
		#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
			emit( !"\r\n" )
		#else
			emit( !"\n" )
		#endif

	case TK_TODO
		linecomment = (tkGet( x + 1 ) = TK_EOL)
		text = tkText( x )

		if( linecomment ) then
			emit( "'' " )
		else
			emit( "/' " )
		end if

		emit( "TODO" )

		if( text ) then
			emit( ": " )
			emit( text )
		end if

		if( linecomment = FALSE ) then
			emit( " '/" )
		end if

		stuff.todocount += 1

	case TK_COMMENT
		emit( "/'" )
		emit( tkText( x ) )
		emit( "'/" )

	case TK_LINECOMMENT
		emit( "''" )
		emit( tkText( x ) )

	case TK_DECNUM
		emit( tkText( x ) )

	case TK_HEXNUM
		emit( "&h" )
		emit( ucase( *tkText( x ) ) )

	case TK_OCTNUM
		emit( "&o" )
		emit( tkText( x ) )

	case TK_STRING
		emit( """" )
		emit( tkText( x ) )
		emit( """" )

	case TK_WSTRING
		emit( "wstr(""" )
		emit( tkText( x ) )
		emit( """)" )

	case TK_ESTRING
		emit( "!""" )
		emit( tkText( x ) )
		emit( """" )

	case TK_EWSTRING
		emit( "wstr(!""" )
		emit( tkText( x ) )
		emit( """)" )

	case else
		text = tkText( x )
		if( text ) then
			emit( text )
		else
			emit( token_text(tkGet( x )) )
		end if

	end select
end sub

sub emitIndent( byval tabs as integer )
	for i as integer = 1 to tabs
		emit( !"\t" )
	next
end sub

sub emitSpace( )
	emit( " " )
end sub

sub emitWriteFile( byref filename as string )
	dim as integer x = any, level = any, nextlevel = any

	emitInit( filename )

	x = 0
	level = 0
	nextlevel = 0
	while( tkGet( x ) <> TK_EOF )

		select case as const( tkGet( x - 1 ) )

		'' At BOL or BOF?
		case TK_EOL, TK_EOF
			nextlevel = level

			select case( tkMark( x ) )
			case MARK_PP
				assert( tkGet( x ) = TK_HASH )
				select case( tkGet( hSkip( x ) ) )
				case KW_IF
					'' Next line and following should be indented
					nextlevel += 1
				case KW_ELSE, KW_ELIF, KW_ELSEIF
					'' This #else line shouldn't be indented
					level -= 1
				case KW_ENDIF
					'' Same for this #endif
					level -= 1
					'' Next line and following shouldn't be indented anymore
					nextlevel -= 1
				end select
			end select

			'' Insert indentation, unless it's an empty line
			if( tkGet( x ) <> TK_EOL ) then
				emitIndent( level )
			end if

			level = nextlevel

		'' Insert space between other tokens if needed
		case TK_HASH
			select case( tkMark( x - 1 ) )
			case MARK_PP
				'' not between PP '#' and the following keyword

			case else
				emitSpace( )
			end select

		case else
			select case( tkGet( x ) )
			case TK_EOL, TK_COMMA, TK_LPAREN
				'' Don't add space in front of EOL or ',' etc.

			case else
				emitSpace( )
			end select

		end select

		emitToken( x )

		x += 1
	wend

	emitEnd( )

	stuff.filecount += 1
end sub

sub emitStats( )
	dim as string message

	message = "& TODO"
	if( stuff.todocount <> 1 ) then
		message += "s"
	end if
	message += " in & file"
	if( stuff.filecount <> 1 ) then
		message += "s"
	end if
	print using message; stuff.todocount; stuff.filecount
end sub

'' Token emitter

#include once "fbfrog.bi"

type EmitterStuff
	fo		as integer '' Output file
	indentlevel	as integer

	todocount	as integer
	filecount	as integer
end type

dim shared as EmitterStuff stuff

private sub emitInit( byref filename as string )
	stuff.indentlevel = 0
	stuff.fo = freefile( )
	if( open( filename, for binary, access write, as #stuff.fo ) ) then
		oops( "could not open output file: '" + filename + "'" )
	end if
end sub

private sub emitEnd( )
	close #stuff.fo
	stuff.fo = 0
	stuff.filecount += 1
end sub

private sub emit( byval text as zstring ptr )
	dim as integer length = any
	length = len( *text )
	if( put( #stuff.fo, , *cptr( ubyte ptr, text ), length ) ) then
		oops( "file I/O failed" )
	end if
end sub

private sub emitEol( )
	#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
		emit( !"\r\n" )
	#else
		emit( !"\n" )
	#endif
end sub

private sub emitIndentBegin( )
	stuff.indentlevel += 1
end sub

private sub emitIndentEnd( )
	stuff.indentlevel -= 1
end sub

private sub emitStmtBegin( byref ln as string )
	for i as integer = 1 to stuff.indentlevel
		emit( !"\t" )
	next
	emit( ln )
end sub

private sub emitStmt( byref ln as string )
	emitStmtBegin( ln )
	emitEol( )
end sub

private sub emitToken( byval x as integer )
	dim as zstring ptr s = any

	select case as const( tkGet( x ) )
	case TK_PPINCLUDE
		emitStmt( "#include """ + *tkGetText( x ) + """" )

	case TK_PPDEFINE
		emitStmtBegin( "#define " + *tkGetText( x ) )
		emitEol( )

	case TK_EOL
		emitEol( )

	case TK_TODO
		emit( "'' TODO" )

		s = tkGetText( x )
		if( len( *s ) > 0 ) then
			emit( ": " )
			emit( s )
		end if

		stuff.todocount += 1

	case TK_COMMENT
		emit( "/'" )
		emit( tkGetText( x ) )
		emit( "'/" )

	case TK_LINECOMMENT
		emit( "''" )
		emit( tkGetText( x ) )

	case TK_DECNUM
		emit( tkGetText( x ) )

	case TK_HEXNUM
		emit( "&h" )
		emit( ucase( *tkGetText( x ) ) )

	case TK_OCTNUM
		emit( "&o" )
		emit( tkGetText( x ) )

	case TK_STRING
		emit( """" )
		emit( tkGetText( x ) )
		emit( """" )

	case TK_WSTRING
		emit( "wstr( """ )
		emit( tkGetText( x ) )
		emit( """ )" )

	case TK_ESTRING
		emit( "!""" )
		emit( tkGetText( x ) )
		emit( """" )

	case TK_EWSTRING
		emit( "wstr( !""" )
		emit( tkGetText( x ) )
		emit( """ )" )

	case else
		emit( tkGetText( x ) )

	end select
end sub

sub emitWriteFile( byref filename as string )
	dim as integer x = any

	emitInit( filename )

	x = 0
	while( tkGet( x ) <> TK_EOF )
		emitToken( x )
		x += 1
	wend

	emitEnd( )
end sub

sub emitStats( )
	dim as string message

	message = stuff.todocount & " TODO"
	if( stuff.todocount <> 1 ) then
		message += "s"
	end if
	message += " in " & stuff.filecount & " file"
	if( stuff.filecount <> 1 ) then
		message += "s"
	end if

	print message
end sub

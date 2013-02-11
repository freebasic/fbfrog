'' Token emitter

#include once "fbfrog.bi"
#include once "parser.bi"

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

sub emitIndentBegin( )
	stuff.indentlevel += 1
end sub

sub emitIndentEnd( )
	stuff.indentlevel -= 1
end sub

sub emitStmt( byref ln as string )
	for i as integer = 1 to stuff.indentlevel
		emit( !"\t" )
	next
	emit( ln )
	''emitEol( )
end sub

sub emitWriteFile( byval block as ASTNODE ptr, byref filename as string )
	dim as ASTNODE ptr i = any

	emitInit( filename )

	i = block->block.head
	while( i )
		select case as const( i->id )
		case TK_EOL
			emitEol( )

		case TK_PPINCLUDE
			emitStmt( "#include """ + *i->text + """" )

		case TK_TODO
			emit( "'' TODO" )

			if( i->text ) then
				emit( ": " )
				emit( i->text )
			end if

			stuff.todocount += 1

		case TK_COMMENT
			emit( "/'" )
			emit( astGetText( i ) )
			emit( "'/" )

		case TK_LINECOMMENT
			emit( "''" )
			emit( astGetText( i ) )

		case TK_DECNUM
			emit( astGetText( i ) )

		case TK_HEXNUM
			emit( "&h" )
			emit( ucase( *astGetText( i ) ) )

		case TK_OCTNUM
			emit( "&o" )
			emit( astGetText( i ) )

		case TK_STRING
			emit( """" )
			emit( astGetText( i ) )
			emit( """" )

		case TK_WSTRING
			emit( "wstr( """ )
			emit( astGetText( i ) )
			emit( """ )" )

		case TK_ESTRING
			emit( "!""" )
			emit( astGetText( i ) )
			emit( """" )

		case TK_EWSTRING
			emit( "wstr( !""" )
			emit( astGetText( i ) )
			emit( """ )" )

		case else
			emit( astGetText( i ) )

		end select

		i = i->next
	wend

	emitEnd( )
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

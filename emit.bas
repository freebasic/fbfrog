'' Token emitter

#include once "fbfrog.bi"
#include once "parser.bi"

declare sub emitNode( byval n as ASTNODE ptr )

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

private sub emitChildren( byval n as ASTNODE ptr )
	dim as ASTNODE ptr i = any
	i = n->head
	while( i )
		emitNode( i )
		i = i->next
	wend
end sub

private sub emitNode( byval n as ASTNODE ptr )
	select case as const( n->id )
	case TK_FILE
		emitStmt( "'' translated from: " + *n->text )
		emitChildren( n )

	case TK_PPINCLUDE
		emitStmt( "#include """ + *n->text + """" )

	case TK_PPDEFINE
		emitStmtBegin( "#define " + *n->text )
		emitChildren( n )
		emitEol( )

	case TK_EOL
		emitEol( )

	case TK_TODO
		emit( "'' TODO" )

		if( n->text ) then
			emit( ": " )
			emit( n->text )
		end if

		stuff.todocount += 1

	case TK_COMMENT
		emit( "/'" )
		emit( astGetText( n ) )
		emit( "'/" )

	case TK_LINECOMMENT
		emit( "''" )
		emit( astGetText( n ) )

	case TK_DECNUM
		emit( astGetText( n ) )

	case TK_HEXNUM
		emit( "&h" )
		emit( ucase( *astGetText( n ) ) )

	case TK_OCTNUM
		emit( "&o" )
		emit( astGetText( n ) )

	case TK_STRING
		emit( """" )
		emit( astGetText( n ) )
		emit( """" )

	case TK_WSTRING
		emit( "wstr( """ )
		emit( astGetText( n ) )
		emit( """ )" )

	case TK_ESTRING
		emit( "!""" )
		emit( astGetText( n ) )
		emit( """" )

	case TK_EWSTRING
		emit( "wstr( !""" )
		emit( astGetText( n ) )
		emit( """ )" )

	case else
		emit( astGetText( n ) )

	end select
end sub

sub emitWriteFile( byval ast as ASTNODE ptr, byref filename as string )
	emitInit( filename )
	emitNode( ast )
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

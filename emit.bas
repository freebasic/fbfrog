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

private sub emitIndent( )
	stuff.indentlevel += 1
end sub

private sub emitUnindent( )
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

private function emitType( byval x as integer ) as string
	static as zstring ptr types(0 to TYPE__COUNT-1) = _
	{ _
		NULL       , _
		@"any"     , _
		@"byte"    , _
		@"ubyte"   , _
		@"zstring" , _
		@"short"   , _
		@"ushort"  , _
		@"long"    , _
		@"ulong"   , _
		@"longint" , _
		@"ulongint", _
		@"single"  , _
		@"double"  , _
		NULL         _
	}

	dim as string s
	dim as integer dtype = any

	dtype = tkGetType( x )

	if( typeIsConstAt( dtype, 0 ) ) then
		s += "const "
	end if

	s += *types(typeGetDt( dtype ))

	for i as integer = 1 to typeGetPtrCount( dtype )
		if( typeIsConstAt( dtype, i ) ) then
			s += " const"
		end if

		s += " ptr"
	next

	function = s
end function

private sub emitDimShared( byval x as integer )
	emitStmt( "dim shared " + *tkGetText( x ) + !" as " + emitType( x ) )
end sub

private sub emitExtern( byval x as integer )
	emitStmt(     "extern " + *tkGetText( x ) + !" as " + emitType( x ) )
end sub

private function emitTk( byval x as integer ) as integer
	dim as zstring ptr s = any

	select case as const( tkGet( x ) )
	case TK_PPINCLUDE
		emitStmt( "#include """ + *tkGetText( x ) + """" )

	case TK_PPDEFINEBEGIN
		emitStmtBegin( "#define " )
		emit( tkGetText( x ) )
		emit( " " )

		do
			x += 1

			if( tkGet( x ) = TK_PPDEFINEEND ) then
				exit do
			end if

			assert( tkGet( x ) <> TK_EOF )
			emitTk( x )
		loop

		emitEol( )

	case TK_STRUCTBEGIN
		emitStmtBegin( "type" )
		s = tkGetText( x )
		if( s ) then
			emit( " " )
			emit( s )
		end if
		emitEol( )

		emitIndent( )
		do
			x += 1

			if( tkGet( x ) = TK_STRUCTEND ) then
				exit do
			end if

			assert( tkGet( x ) <> TK_EOF )
			emitTk( x )
		loop
		emitUnindent( )

		emitStmt( "end type" )

	case TK_FIELD
		emitStmt( *tkGetText( x ) + !"\t\tas " + emitType( x ) )

	case TK_GLOBAL
		emitExtern( x )
		emitDimShared( x )

	case TK_EXTERNGLOBAL
		emitExtern( x )

	case TK_STATICGLOBAL
		emitDimShared( x )

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

	x += 1
	function = x
end function

sub emitWriteFile( byref filename as string )
	dim as integer x = any

	emitInit( filename )

	x = 0
	while( tkGet( x ) <> TK_EOF )
		x = emitTk( x )
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

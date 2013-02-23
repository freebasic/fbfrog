'' Token emitter

#include once "fbfrog.bi"

declare function emitTk( byval x as integer ) as integer

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

private function emitPPDefine( byval x as integer ) as integer
	emitStmtBegin( "#define " + *tkGetText( x ) + " " )

	x += 1
	while( tkGet( x ) <> TK_PPDEFINEEND )
		x = emitTk( x )
	wend
	x += 1

	emitEol( )

	function = x
end function

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

	if( typeGetDt( dtype ) = TYPE_UDT ) then
		s += *tkGetSubtype( x )
	else
		s += *types(typeGetDt( dtype ))
	end if

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

private function emitStruct( byval x as integer ) as integer
	dim as zstring ptr s = any
	dim as string ln

	ln = "type"
	s = tkGetText( x )
	if( len( *s ) > 0 ) then
		ln += " " + *s
	end if
	emitStmt( ln )

	emitIndent( )
	x += 1
	while( tkGet( x ) <> TK_STRUCTEND )
		x = emitTk( x )
	wend
	x += 1
	emitUnindent( )

	emitStmt( "end type" )

	function = x
end function

private function emitProcDecl( byval x as integer ) as integer
	dim as integer y = any, count = any
	dim as string ln

	ln = "declare "
	if( tkGetType( x ) = TYPE_ANY ) then
		ln += "sub"
	else
		ln += "function"
	end if
	ln += " " + *tkGetText( x )

	ln += "( "
	y = x
	count = 0
	do
		y += 1

		select case( tkGet( y ) )
		case TK_PARAM, TK_PARAMPROCPTR, TK_PARAMVARARG
			if( count > 0 ) then
				ln += ", "
			end if

			select case( tkGet( y ) )
			case TK_PARAM
				ln += "byval "
				ln += *tkGetText( y )
				ln += " as " + emitType( y )

			case TK_PARAMPROCPTR
				ln += "TODO"

			case TK_PARAMVARARG
				ln += "..."
			end select

		case else
			exit do
		end select

		count += 1
	loop
	ln += " )"

	if( tkGetType( x ) <> TYPE_ANY ) then
		ln += " as " + emitType( x )
	end if

	emitStmt( ln )

	x = y

	function = x
end function

private sub emitTodo( byval x as integer )
	dim as string ln
	dim as zstring ptr s = any

	ln = "'' "

	if( tkHasSourceLocation( x + 1 ) ) then
		ln += *tkGetSourceFile( x + 1 )
		ln += "(" & tkGetLineNum( x + 1 ) + 1 & "): "
	end if

	ln += "TODO"

	s = tkGetText( x )
	if( len( *s ) > 0 ) then
		ln += ": " + *s
	end if

	emitStmt( ln )
	stuff.todocount += 1
end sub

private function emitTk( byval x as integer ) as integer
	dim as string ln

	select case as const( tkGet( x ) )
	case TK_EOF
		assert( FALSE )

	case TK_PPINCLUDE
		emitStmt( "#include """ + *tkGetText( x ) + """" )
		x += 1

	case TK_PPDEFINEBEGIN
		x = emitPPDefine( x )

	case TK_STRUCTBEGIN
		emitEol( )
		x = emitStruct( x )
		emitEol( )

	case TK_FIELD
		emitStmt( *tkGetText( x ) + !"\t\tas " + emitType( x ) )
		x += 1

	case TK_GLOBAL
		emitExtern( x )
		emitDimShared( x )
		x += 1

	case TK_EXTERNGLOBAL
		emitExtern( x )
		x += 1

	case TK_STATICGLOBAL
		emitDimShared( x )
		x += 1

	case TK_PROC
		x = emitProcDecl( x )

	case TK_EOL
		emitEol( )
		x += 1

	case TK_TODO
		emitTodo( x )
		x += 1

	case TK_TODOBEGIN
		emitEol( )
		emitTodo( x )

		x += 1
		while( tkGet( x ) <> TK_TODOEND )
			x = emitTk( x )
		wend
		x += 1

		emitEol( )

	case TK_COMMENT
		emit( "/'" )
		emit( tkGetText( x ) )
		emit( "'/" )
		x += 1

	case TK_LINECOMMENT
		emit( "''" )
		emit( tkGetText( x ) )
		x += 1

	case TK_DECNUM
		emit( tkGetText( x ) )
		x += 1

	case TK_HEXNUM
		emit( "&h" )
		emit( ucase( *tkGetText( x ) ) )
		x += 1

	case TK_OCTNUM
		emit( "&o" )
		emit( tkGetText( x ) )
		x += 1

	case TK_STRING
		emit( """" )
		emit( tkGetText( x ) )
		emit( """" )
		x += 1

	case TK_WSTRING
		emit( "wstr( """ )
		emit( tkGetText( x ) )
		emit( """ )" )
		x += 1

	case TK_ESTRING
		emit( "!""" )
		emit( tkGetText( x ) )
		emit( """" )
		x += 1

	case TK_EWSTRING
		emit( "wstr( !""" )
		emit( tkGetText( x ) )
		emit( """ )" )
		x += 1

	case else
		emit( tkGetText( x ) )
		x += 1

	end select

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

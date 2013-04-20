'' Token emitter

#include once "fbfrog.bi"

declare sub emitDecl _
	( _
		byref x as integer, _
		byref ln as string, _
		byval decl as integer, _
		byval belongs_to_dimshared as integer = FALSE _
	)
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

private sub emitStmt( byref ln as string, byref comment as string )
	if( len( comment ) > 0 ) then
		'' Multi-line comment? Then emit it above the statement, if any;
		'' otherwise, behind it (or at BOL, if no statement)
		if( instr( comment, !"\n" ) > 0 ) then
			emitStmtBegin( "''" + strReplace( comment, !"\n", !"\n''" ) )
			emitEol( )
			if( len( ln ) > 0 ) then
				emitStmtBegin( ln )
				emitEol( )
			end if
		else
			emitStmtBegin( ln )
			if( len( comment ) > 0 ) then
				'' Separate comment with a space if not a BOL
				if( len( ln ) > 0 ) then
					emit( " " )
				end if
				emit( "''" + comment )
			end if
			emitEol( )
		end if
	else
		emitStmtBegin( ln )
		emitEol( )
	end if
end sub

function emitType _
	( _
		byval dtype as integer, _
		byval subtype as zstring ptr _
	) as string

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
	dim as integer ptrcount = any

	ptrcount = typeGetPtrCount( dtype )

	if( typeIsConstAt( dtype, ptrcount ) ) then
		s += "const "
	end if

	if( typeGetDt( dtype ) = TYPE_UDT ) then
		s += *subtype
	else
		s += *types(typeGetDt( dtype ))
	end if

	for i as integer = (ptrcount - 1) to 0 step -1
		if( typeIsConstAt( dtype, i ) ) then
			s += " const"
		end if

		s += " ptr"
	next

	function = s
end function

private function emitSubOrFunction( byval x as integer ) as string
	if( tkGetType( x ) = TYPE_ANY ) then
		function = "sub"
	else
		function = "function"
	end if
end function

private sub emitParamListAndResultType( byref x as integer, byref ln as string )
	dim as integer y = any, count = any

	'' The parameter tokens, if any, are following behind the main token,
	'' grouped in a BEGIN/END:
	''    TK_PROC/...
	''    TK_BEGIN
	''        TK_PARAM
	''        ...
	''    TK_END

	ln += "("

	y = x + 1
	if( tkGet( y ) = TK_BEGIN ) then
		'' Begin
		y += 1

		count = 0
		while( tkGet( y ) <> TK_END )
			if( count > 0 ) then
				ln += ","
			end if
			ln += " "

			if( tkGet( y ) = TK_PARAMVARARG ) then
				ln += "..."
				y += 1
			else
				emitDecl( y, ln, TK_PARAM )
			end if

			count += 1
		wend

		'' End
		y += 1
	end if

	ln += " )"

	'' Function result type
	if( tkGetType( x ) <> TYPE_ANY ) then
		ln += " as " + emitType( tkGetType( x ), tkGetSubtype( x ) )
	end if

	'' Skip over the main token and its parameters, if any
	x = y
end sub

private sub emitDecl _
	( _
		byref x as integer, _
		byref ln as string, _
		byval decl as integer, _
		byval belongs_to_dimshared as integer _
	)

	dim as zstring ptr s = any

	select case( decl )
	case TK_GLOBAL
		ln += "dim shared "
	case TK_EXTERNGLOBAL
		ln += "extern "
		if( belongs_to_dimshared ) then
			ln += "    "
		end if
	case TK_PARAM
		ln += "byval "
	case TK_FIELD

	case TK_TYPEDEF
		ln += "type "
	case else
		assert( FALSE )
	end select

	s = tkGetText( x )
	if( len( *s ) > 0 ) then
		ln += *s + " "
	end if

	ln += "as "

	if( tkIsProcPtr( x ) ) then
		ln += emitSubOrFunction( x )
		emitParamListAndResultType( x, ln )
	else
		ln += emitType( tkGetType( x ), tkGetSubtype( x ) )
		x += 1
	end if
end sub

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

	emitStmt( ln, "" )
	stuff.todocount += 1
end sub

private function emitTk( byval x as integer ) as integer
	dim as string ln, comment
	dim as integer y = any
	dim as zstring ptr s = any

	assert( tkGet( x ) <> TK_EOF )
	assert( tkGet( x ) <> TK_BEGIN )
	assert( tkGet( x ) <> TK_END )

	s = tkGetComment( x )
	if( s ) then
		comment = *s
	end if

	select case as const( tkGet( x ) )
	case TK_NOP
		x += 1

	case TK_PPINCLUDE
		emitStmt( "#include """ + *tkGetText( x ) + """", comment )
		x += 1

	case TK_PPDEFINE
		emitStmtBegin( "#define " + *tkGetText( x ) + " " )

		'' PPDefine
		x += 1

		'' Begin?
		if( tkGet( x ) = TK_BEGIN ) then
			x += 1

			while( tkGet( x ) <> TK_END )
				x = emitTk( x )
			wend

			'' End
			x += 1
		end if

		emitEol( )

	case TK_PPIFDEF
		emitStmt( "#ifdef " + *tkGetText( x ), comment )
		x += 1

	case TK_PPIFNDEF
		emitStmt( "#ifndef " + *tkGetText( x ), comment )
		x += 1

	case TK_PPELSE
		emitStmt( "#else", comment )
		x += 1

	case TK_PPENDIF
		emitStmt( "#endif", comment )
		x += 1

	case TK_STRUCT
		ln = "type"
		s = tkGetText( x )
		if( len( *s ) > 0 ) then
			ln += " " + *s
		end if
		emitStmt( ln, comment )

		'' Struct
		x += 1

		'' Begin
		assert( tkGet( x ) = TK_BEGIN )
		x += 1

		emitIndent( )
		while( tkGet( x ) <> TK_END )
			x = emitTk( x )
		wend
		emitUnindent( )

		'' End
		x += 1

		emitStmt( "end type", "" )

	case TK_TYPEDEF, TK_TYPEDEFPROCPTR
		emitDecl( x, ln, TK_TYPEDEF )
		emitStmt( ln, comment )

	case TK_FIELD, TK_FIELDPROCPTR
		emitDecl( x, ln, TK_FIELD )
		emitStmt( ln, comment )

	case TK_GLOBAL, TK_GLOBALPROCPTR
		y = x
		emitDecl( x, ln, TK_EXTERNGLOBAL, TRUE )
		emitStmt( ln, comment )

		x = y
		ln = ""
		emitDecl( x, ln, TK_GLOBAL )
		emitStmt( ln, "" )

	case TK_EXTERNGLOBAL, TK_EXTERNGLOBALPROCPTR
		emitDecl( x, ln, TK_EXTERNGLOBAL )
		emitStmt( ln, comment )

	case TK_STATICGLOBAL, TK_STATICGLOBALPROCPTR
		emitDecl( x, ln, TK_GLOBAL )
		emitStmt( ln, comment )

	case TK_PROC
		ln = "declare "
		ln += emitSubOrFunction( x )
		ln += " "
		ln += *tkGetText( x )
		emitParamListAndResultType( x, ln )
		emitStmt( ln, comment )

	case TK_EOL
		emitEol( )
		x += 1

	case TK_DIVIDER
		emitStmt( "", comment )
		x += 1

	case TK_TODO
		emitTodo( x )
		x += 1

		'' Begin?
		if( tkGet( x ) = TK_BEGIN ) then
			x += 1

			while( tkGet( x ) <> TK_END )
				x = emitTk( x )
			wend

			'' End
			x += 1
		end if

	case TK_COMMENT
		emit( "/'" )
		emit( tkGetText( x ) )
		emit( "'/" )
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

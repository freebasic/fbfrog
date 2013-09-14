''
'' *.fbfrog preset files parsing
''

#include once "fbfrog.bi"

dim shared as integer x

private sub hOops( byval message as zstring ptr )
	tkOops( x, message )
end sub

private sub hSkip( )
	x += 1
end sub

private sub hExpect( byval tk as integer, byval whatfor as zstring ptr )
	tkExpect( x, tk, whatfor )
end sub

private sub hExpectSkip( byval tk as integer, byval whatfor as zstring ptr )
	hExpect( tk, whatfor )
	hSkip( )
end sub

private function hMatch( byval tk as integer ) as integer
	if( tkGet( x ) = tk ) then
		function = TRUE
		hSkip( )
	end if
end function

private function hExpectSkipString( byval whatfor as zstring ptr ) as string
	hExpect( TK_STRING, whatfor )
	function = *tkGetText( x )
	hSkip( )
end function

'' VersionId = StringLiteral | Identifier | WIN32 | LINUX | DOS | '*'
private function hVersionId( ) as ASTNODE ptr
	select case( tkGet( x ) )
	case TK_STRING
		function = astNew( ASTCLASS_STRING, tkGetText( x ) )
	case is >= TK_ID
		select case( lcase( *tkGetText( x ) ) )
		case "dos"   : function = astNew( ASTCLASS_DOS )
		case "linux" : function = astNew( ASTCLASS_LINUX )
		case "win32" : function = astNew( ASTCLASS_WIN32 )
		case else    : function = astNewID( tkGetText( x ) )
		end select
	case TK_STAR
		function = astNew( ASTCLASS_WILDCARD )
	case else
		tkOopsExpected( x, "version identifier (""1.2.3"", 'foo', one of dos|linux|win32, '*')", NULL )
	end select
	hSkip( )
end function

private sub hLoadFile( byval file as ASTNODE ptr )
	lexLoadFile( x, file, LEXMODE_FBFROG, FALSE )
end sub

#if 0
private sub hIncludeFile( byref incfile as string )
	do include file search
	hLoadFile( ... )
end sub
#endif

sub presetParse( byval pre as FROGPRESET ptr, byref presetfile as string )
	var presetf = astNewFROGFILE( presetfile, presetfile )
	tkInit( )

	hLoadFile( presetf )

	const MAXVERSPACES = 16
	dim verspacestack(0 to MAXVERSPACES-1) as ASTNODE ptr
	var verlevel = 0
	verspacestack(0) = pre->code

	x = 0
	do
		select case( tkGet( x ) )
		case TK_EOF
			exit do

		case TK_EOL

#if 0
		'' '#' INCLUDE "filename"
		case TK_HASH
			hSkip( )

			'' INCLUDE
			hExpectSkip( KW_INCLUDE, "as in '#include ""...""'" )

			'' "filename"
			var incfile = hExpectSkipString( "containing the #include file name" )

			hExpectSkip( TK_EOL, "behind #include statement" )
			hIncludeFile( incfile )
			continue do
#endif

		'' VERSION VersionId ('.' VersionId)*
		case KW_VERSION
			hSkip( )

			var context = verspacestack(verlevel)
			verlevel += 1
			if( verlevel = MAXVERSPACES ) then
				hOops( "VERSION block stack too small, MAXVERSPACES=" & MAXVERSPACES )
			end if

			do
				'' VersionId
				var id = hVersionId( )

				var newver = astNewVERSION( id, NULL )
				astAppend( context, newver )
				context = newver

				'' '.'?
			loop while( hMatch( TK_DOT ) )

			verspacestack(verlevel) = context

		'' END VERSION
		case KW_END
			hSkip( )
			hExpectSkip( KW_VERSION, "as in 'END VERSION" )

			if( verlevel < 1 ) then
				hOops( "END VERSION without corresponding VERSION block begin" )
			end if
			verlevel -= 1

		'' DOWNLOAD "URL" "output file name"
		case KW_DOWNLOAD
			hSkip( )
			var url = hExpectSkipString( "containing the download URL" )
			var outfile = hExpectSkipString( "containing the output file name" )

			var download = astNew( ASTCLASS_DOWNLOAD, url )
			astSetComment( download, outfile )
			astAppend( verspacestack(verlevel), download )

		'' EXTRACT "tarball file name" ["output directory name"]
		case KW_EXTRACT
			hSkip( )
			var tarball = hExpectSkipString( "containing the archive file name" )

			dim outdir as zstring ptr
			if( tkGet( x ) = TK_STRING ) then
				outdir = tkGetText( x )
				hSkip( )
			end if

			var extract = astNew( ASTCLASS_EXTRACT, tarball )
			astSetComment( extract, outdir )
			astAppend( verspacestack(verlevel), extract )

		'' COPYFILE "old name" "new name"
		case KW_COPYFILE
			hSkip( )
			var oldname = hExpectSkipString( "containing the original file name" )
			var newname = hExpectSkipString( "containing the new file name" )

			var copyfile = astNew( ASTCLASS_COPYFILE, oldname )
			astSetComment( copyfile, newname )
			astAppend( verspacestack(verlevel), copyfile )

		'' FILE "file name"
		case KW_FILE
			hSkip( )
			astAppend( verspacestack(verlevel), astNew( ASTCLASS_FILE, _
				hExpectSkipString( "containing the file name" ) ) )

		'' DIR "dir name"
		case KW_DIR
			hSkip( )
			astAppend( verspacestack(verlevel), astNew( ASTCLASS_DIR, _
				hExpectSkipString( "containing the directory name" ) ) )

		'' DEFINE Identifier
		case KW_DEFINE
			hSkip( )

			hExpect( TK_ID, "(the symbol that should be pre-#defined)" )
			astAppend( verspacestack(verlevel), astNew( ASTCLASS_DEFINE, tkGetText( x ) ) )
			hSkip( )

		'' UNDEF Identifier
		case KW_UNDEF
			hSkip( )

			hExpect( TK_ID, "(the symbol that should be initially un-#defined)" )
			astAppend( verspacestack(verlevel), astNew( ASTCLASS_PPUNDEF, tkGetText( x ) ) )
			hSkip( )

		'' NOEXPAND Identifier
		case KW_NOEXPAND
			hSkip( )

			hExpect( TK_ID, "(a #defined symbol)" )
			astAppend( verspacestack(verlevel), astNew( ASTCLASS_NOEXPAND, tkGetText( x ) ) )
			hSkip( )

		'' MACRO Identifier ['(' MacroParameters ')'] [MacroBody]
		case KW_MACRO
			hSkip( )

			hExpect( TK_ID, "(this macro's name)" )
			var macro = astNew( ASTCLASS_PPDEFINE, tkGetText( x ) )

			hMacroParamList( x, macro )

			assert( macro->expr = NULL )
			macro->expr = astNew( ASTCLASS_MACROBODY )
			do
				select case( tkGet( x ) )
				case TK_EOL, TK_EOF
					exit do
				end select

				astAppend( macro->expr, astNewTK( x ) )

				x += 1
			loop

			astAppend( verspacestack(verlevel), macro )

		'' REMOVE (DEFINE|STRUCT|PROC|...) Identifier
		case KW_REMOVE
			hSkip( )

			var astclass = -1
			select case( tkGet( x ) )
			case KW_DEFINE
				astclass = ASTCLASS_REMOVE
			case else
				hOops( "unknown REMOVE command" )
			end select
			hSkip( )

			'' Identifier
			hExpect( TK_ID, "(the symbol to remove by name)" )
			astAppend( verspacestack(verlevel), astNew( astclass, tkGetText( x ) ) )
			hSkip( )

		'' OPTION optionid
		case KW_OPTION
			hSkip( )

			var opt = -1
			if( tkGet( x ) >= TK_ID ) then
				select case( lcase( *tkGetText( x ) ) )
				case "nomerge"      : opt = PRESETOPT_NOMERGE
				case "comments"     : opt = PRESETOPT_COMMENTS
				case "nopp"         : opt = PRESETOPT_NOPP
				case "noppfold"     : opt = PRESETOPT_NOPPFOLD
				case "noautoextern" : opt = PRESETOPT_NOAUTOEXTERN
				case "windowsms"    : opt = PRESETOPT_WINDOWSMS
				end select
			end if
			if( opt < 0 ) then
				hOops( "unknown option" )
			end if
			hSkip( )

			pre->options or= opt

		case else
			hOops( "unknown construct" )
		end select

		hExpectSkip( TK_EOL, "behind this statement" )
	loop

	if( verlevel > 0 ) then
		hOops( "missing END VERSION" )
	end if

	tkEnd( )
	astDelete( presetf )
end sub

sub presetInit( byval pre as FROGPRESET ptr )
	pre->code = astNewGROUP( )
	pre->options = 0
end sub

sub presetEnd( byval pre as FROGPRESET ptr )
	astDelete( pre->code )
end sub

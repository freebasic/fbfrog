''
'' *.fbfrog preset files parsing
''

#include once "fbfrog.bi"

declare function hBody( byval body as integer ) as ASTNODE ptr

dim shared as integer x
dim shared as ASTNODE ptr presetfiles
dim shared as integer presetoptions

private function hKeyword( ) as string
	if( tkGet( x ) >= TK_ID ) then
		function = lcase( *tkGetText( x ) )
	end if
end function

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

private sub hLoadFile( byref file as string )
	var f = astNewFROGFILE( file, file )
	astAppend( presetfiles, f )
	lexLoadFile( x, f, FALSE, FALSE )
	tkTurnCPPTokensIntoCIds( )
end sub

enum
	BODY_TOPLEVEL = 0
	BODY_VERSION
	BODY_TARGET
end enum

private function hStatementOrBlock( byval body as integer ) as ASTNODE ptr
	'' '#' INCLUDE "filename"
	'' '#'?
	if( tkGet( x ) = TK_HASH ) then
		hSkip( )

		'' INCLUDE
		hExpectSkip( KW_INCLUDE, "as in '#include ""...""'" )

		'' "filename"
		hExpect( TK_STRING, "containing the #include file name" )
		var incfile = *tkGetText( x )
		var location = tkGetLocation( x )
		'' Try to open the #include relative to the parent file's directory
		incfile = pathAddDiv( pathOnly( *location->file->text ) ) + incfile
		if( hFileExists( incfile ) = FALSE ) then
			hReportLocation( location, "file not found: '" + incfile + "'" )
			end 1
		end if
		hSkip( )

		hExpectSkip( TK_EOL, "behind #include statement" )
		hLoadFile( incfile )
		exit function
	end if

	if( tkGet( x ) = TK_EOL ) then
		hSkip( )
		exit function
	end if

	var code = astNewGROUP( )

	select case( hKeyword( ) )

	'' VERSION VersionId
	''     VersionBody
	'' ENDVERSION
	case "version"
		if( body <> BODY_TOPLEVEL ) then
			hOops( "nested VERSION blocks are not supported, sorry" )
		end if
		hSkip( )

		'' VersionId
		dim as ASTNODE ptr id
		select case( tkGet( x ) )
		case TK_STRING
			id = astNew( ASTCLASS_STRING, tkGetText( x ) )
		case TK_DECNUM
			id = astNewCONST( vallng( *tkGetText( x ) ), 0, TYPE_LONGINT )
		case else
			hOops( "expected ""..."" string or 123 number literal" )
		end select
		hSkip( )

		'' EOL
		hExpectSkip( TK_EOL, "behind this statement" )

		'' VersionBody
		astAppend( code, astNewVERBLOCK( id, NULL, hBody( BODY_VERSION ) ) )

		'' ENDVERSION
		if( hKeyword( ) <> "endversion" ) then
			hOops( "missing ENDVERSION" )
		end if
		hSkip( )

	'' TARGET <target>
	''     TargetBody
	'' ENDTARGET
	case "target"
		if( body = BODY_TARGET ) then
			hOops( "nested target blocks are not supported, sorry" )
		end if
		hSkip( )

		'' <target>
		dim as integer attrib
		select case( hKeyword( ) )
		case "dos"   : attrib = ASTATTRIB_DOS
		case "linux" : attrib = ASTATTRIB_LINUX
		case "win32" : attrib = ASTATTRIB_WIN32
		case else
			hOops( "expected dos|linux|win32" )
		end select

		'' EOL
		hExpectSkip( TK_EOL, "behind this statement" )

		'' TargetBody
		var block = astNew( ASTCLASS_TARGETBLOCK, hBody( BODY_TARGET ) )
		block->attrib or= attrib
		astAppend( code, block )

		'' ENDTARGET
		if( hKeyword( ) <> "endtarget" ) then
			hOops( "missing ENDTARGET" )
		end if
		hSkip( )

	'' DOWNLOAD "URL" "output file name"
	case "download"
		hSkip( )
		var url = hExpectSkipString( "containing the download URL" )
		var outfile = hExpectSkipString( "containing the output file name" )

		var download = astNew( ASTCLASS_DOWNLOAD, url )
		astSetComment( download, outfile )
		astAppend( code, download )

	'' EXTRACT "tarball file name" ["output directory name"]
	case "extract"
		hSkip( )
		var tarball = hExpectSkipString( "containing the archive file name" )

		dim outdir as zstring ptr
		if( tkGet( x ) = TK_STRING ) then
			outdir = tkGetText( x )
			hSkip( )
		end if

		var extract = astNew( ASTCLASS_EXTRACT, tarball )
		astSetComment( extract, outdir )
		astAppend( code, extract )

	'' COPYFILE "old name" "new name"
	case "copyfile"
		hSkip( )
		var oldname = hExpectSkipString( "containing the original file name" )
		var newname = hExpectSkipString( "containing the new file name" )

		var copyfile = astNew( ASTCLASS_COPYFILE, oldname )
		astSetComment( copyfile, newname )
		astAppend( code, copyfile )

	'' FILE "file name"
	case "file"
		hSkip( )
		astAppend( code, astNew( ASTCLASS_FILE, _
			hExpectSkipString( "containing the file name" ) ) )

	'' DIR "dir name"
	case "dir"
		hSkip( )
		astAppend( code, astNew( ASTCLASS_DIR, _
			hExpectSkipString( "containing the directory name" ) ) )

	'' UNDEF Identifier
	case "undef"
		hSkip( )

		hExpect( TK_ID, "(a symbol that's initially un-#defined)" )
		astAppend( code, astNew( ASTCLASS_PPUNDEF, tkGetText( x ) ) )
		hSkip( )

	'' DEFINE Identifier ['(' MacroParameters ')'] [MacroBody]
	case "define"
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

		astAppend( code, macro )

	'' NOEXPAND Identifier
	case "noexpand"
		hSkip( )

		hExpect( TK_ID, "(a #defined symbol)" )
		astAppend( code, astNew( ASTCLASS_NOEXPAND, tkGetText( x ) ) )
		hSkip( )

	'' REMOVE (DEFINE|STRUCT|PROC|...) Identifier
	case "remove"
		hSkip( )

		var astclass = -1
		select case( hKeyword( ) )
		case "define"
			astclass = ASTCLASS_REMOVE
		case else
			hOops( "unknown REMOVE command" )
		end select
		hSkip( )

		'' Identifier
		hExpect( TK_ID, "(the symbol to remove by name)" )
		astAppend( code, astNew( astclass, tkGetText( x ) ) )
		hSkip( )

	'' OPTION OptionId+
	case "option"
		do
			hSkip( )
			select case( hKeyword( ) )
			case "nomerge"      : presetoptions or= PRESETOPT_NOMERGE
			case "whitespace"   : presetoptions or= PRESETOPT_WHITESPACE
			case "nopp"         : presetoptions or= PRESETOPT_NOPP
			case "noppfold"     : presetoptions or= PRESETOPT_NOPPFOLD
			case "noautoextern" : presetoptions or= PRESETOPT_NOAUTOEXTERN
			case "windowsms"    : presetoptions or= PRESETOPT_WINDOWSMS
			case else
				hOops( "unknown option" )
			end select
			hSkip( )
		loop while( tkGet( x ) <> TK_EOL )

	case else
		hOops( "unknown construct" )
	end select

	hExpectSkip( TK_EOL, "behind this statement" )
	function = code
end function

private function hBody( byval body as integer ) as ASTNODE ptr
	var code = astNewGROUP( )

	while( tkGet( x ) <> TK_EOF )
		select case( body )
		case BODY_VERSION
			if( hKeyword( ) = "endversion" ) then exit while
		case BODY_TARGET
			if( hKeyword( ) = "endtarget" ) then exit while
		end select

		astAppend( code, hStatementOrBlock( body ) )
	wend

	function = code
end function

sub presetParse( byval pre as FROGPRESET ptr, byref presetfile as string )
	x = 0
	presetfiles = astNewGROUP( )
	presetoptions = 0
	tkInit( )

	hLoadFile( presetfile )
	astAppend( pre->code, hBody( BODY_TOPLEVEL ) )
	pre->options or= presetoptions

	tkEnd( )
	astDelete( presetfiles )
end sub

sub presetInit( byval pre as FROGPRESET ptr )
	pre->code = astNewGROUP( )
	pre->options = 0
end sub

sub presetEnd( byval pre as FROGPRESET ptr )
	astDelete( pre->code )
end sub

private function hHasInputFiles( byval n as ASTNODE ptr ) as integer
	var child = n->head
	while( child )
		select case( child->class )
		case ASTCLASS_VERBLOCK
			if( hHasInputFiles( child ) ) then
				return TRUE
			end if
		case ASTCLASS_FILE, ASTCLASS_DIR
			return TRUE
		end select
		child = child->next
	wend
	function = FALSE
end function

function presetHasInputFiles( byval pre as FROGPRESET ptr ) as integer
	function = hHasInputFiles( pre->code )
end function

private sub hRemoveInputFiles( byval n as ASTNODE ptr )
	var child = n->head
	while( child )
		select case( child->class )
		case ASTCLASS_VERBLOCK
			hRemoveInputFiles( child )
		case ASTCLASS_FILE, ASTCLASS_DIR
			child->class = ASTCLASS_NOP
		end select
		child = child->next
	wend
end sub

private sub hCopyInputFiles _
	( _
		byval code as ASTNODE ptr, _
		byval source as ASTNODE ptr _
	)

	var child = source->head
	while( child )
		select case( child->class )
		case ASTCLASS_VERBLOCK
			'' Copying versioned input files would be more difficult,
			'' assuming the version info should be preserved, but
			'' for the command line this isn't needed anyways,
			'' because no version can be specified there.
			assert( FALSE )
		case ASTCLASS_FILE, ASTCLASS_DIR
			astAppend( code, astClone( child ) )
		end select
		child = child->next
	wend

end sub

sub presetOverrideInputFiles _
	( _
		byval pre as FROGPRESET ptr, _
		byval source as FROGPRESET ptr _
	)

	hRemoveInputFiles( pre->code )
	hCopyInputFiles( pre->code, source->code )

end sub

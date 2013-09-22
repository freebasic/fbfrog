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

'' VersionId = '*' | (Identifier ['(' String|Number ')'])
private function hVersionId( ) as ASTNODE ptr
	select case( tkGet( x ) )
	case is >= TK_ID
		var id = tkGetIdOrKw( x )
		hSkip( )

		'' '('
		if( tkGet( x ) = TK_LPAREN ) then
			hSkip( )

			var n = astNew( ASTCLASS_VERVAL, id )
			function = n

			select case( tkGet( x ) )
			case TK_STRING
				n->l = astNew( ASTCLASS_STRING, tkGetText( x ) )
			case TK_DECNUM
				n->l = astNewCONST( vallng( *tkGetText( x ) ), 0, TYPE_LONGINT )
			case else
				hOops( "expected ""..."" string or 123 number literal" )
			end select
			hSkip( )

			'' ')'
			hExpectSkip( TK_RPAREN, "corresponding to the '('" )
		else
			function = astNewID( id )
		end if
	case TK_STAR
		function = astNew( ASTCLASS_WILDCARD )
		hSkip( )
	case else
		tkOopsExpected( x, "'*' or version #define identifier", NULL )
	end select
end function

private sub hLoadFile _
	( _
		byval files as ASTNODE ptr, _
		byref normed as string, _
		byref pretty as string _
	)

	var f = astNewFROGFILE( normed, pretty )
	astAppend( files, f )

	lexLoadFile( x, f, FALSE, FALSE )

end sub

sub presetParse( byval pre as FROGPRESET ptr, byref presetfile as string )
	var presetfiles = astNewGROUP( )
	tkInit( )

	hLoadFile( presetfiles, presetfile, presetfile )

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

		'' '#' INCLUDE "filename"
		case TK_HASH
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
			hLoadFile( presetfiles, incfile, incfile )
			continue do

		'' UNDEF Identifier
		case KW_UNDEF
			hSkip( )

			hExpect( TK_ID, "(a symbol that's initially un-#defined)" )
			astAppend( verspacestack(verlevel), astNew( ASTCLASS_PPUNDEF, tkGetText( x ) ) )
			hSkip( )

		'' DEFINE Identifier ['(' MacroParameters ')'] [MacroBody]
		case KW_DEFINE
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

		case is >= TK_ID
			select case( *tkGetText( x ) )
			'' VERSION VersionId ('.' VersionId)*
			case "version"
				hSkip( )

				var context = verspacestack(verlevel)
				verlevel += 1
				if( verlevel = MAXVERSPACES ) then
					hOops( "VERSION block stack too small, MAXVERSPACES=" & MAXVERSPACES )
				end if

				do
					'' VersionId
					var id = hVersionId( )

					var newver = astNewVERBLOCK( id, NULL, NULL )
					astAppend( context, newver )
					context = newver

					'' '.'?
				loop while( hMatch( TK_DOT ) )

				verspacestack(verlevel) = context

			'' ENDVERSION
			case "endversion"
				hSkip( )

				if( verlevel < 1 ) then
					hOops( "ENDVERSION without corresponding VERSION block begin" )
				end if
				verlevel -= 1

			'' DOWNLOAD "URL" "output file name"
			case "download"
				hSkip( )
				var url = hExpectSkipString( "containing the download URL" )
				var outfile = hExpectSkipString( "containing the output file name" )

				var download = astNew( ASTCLASS_DOWNLOAD, url )
				astSetComment( download, outfile )
				astAppend( verspacestack(verlevel), download )

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
				astAppend( verspacestack(verlevel), extract )

			'' COPYFILE "old name" "new name"
			case "copyfile"
				hSkip( )
				var oldname = hExpectSkipString( "containing the original file name" )
				var newname = hExpectSkipString( "containing the new file name" )

				var copyfile = astNew( ASTCLASS_COPYFILE, oldname )
				astSetComment( copyfile, newname )
				astAppend( verspacestack(verlevel), copyfile )

			'' FILE "file name"
			case "file"
				hSkip( )
				astAppend( verspacestack(verlevel), astNew( ASTCLASS_FILE, _
					hExpectSkipString( "containing the file name" ) ) )

			'' DIR "dir name"
			case "dir"
				hSkip( )
				astAppend( verspacestack(verlevel), astNew( ASTCLASS_DIR, _
					hExpectSkipString( "containing the directory name" ) ) )

			'' NOEXPAND Identifier
			case "noexpand"
				hSkip( )

				hExpect( TK_ID, "(a #defined symbol)" )
				astAppend( verspacestack(verlevel), astNew( ASTCLASS_NOEXPAND, tkGetText( x ) ) )
				hSkip( )

			'' REMOVE (DEFINE|STRUCT|PROC|...) Identifier
			case "remove"
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
			case "option"
				hSkip( )

				var opt = -1
				if( tkGet( x ) >= TK_ID ) then
					select case( lcase( *tkGetText( x ) ) )
					case "nomerge"      : opt = PRESETOPT_NOMERGE
					case "whitespace"   : opt = PRESETOPT_WHITESPACE
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

		case else
			hOops( "unknown construct" )
		end select

		hExpectSkip( TK_EOL, "behind this statement" )
	loop

	if( verlevel > 0 ) then
		hOops( "missing ENDVERSION" )
	end if

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

''
'' *.fbfrog preset files parsing
''

#include once "fbfrog.bi"

dim shared as integer x, verlevel
const MAXVERBLOCKS = 8
dim shared as ASTNODE ptr verstack(0 to MAXVERBLOCKS-1)

private sub hOops( byref message as string )
	tkOops( x, message )
end sub

private sub hOopsExpected( byref message as string )
	tkOopsExpected( x, message )
end sub

private sub hSkip( )
	x += 1
end sub

private sub hExpect( byval tk as integer )
	tkExpect( x, tk )
end sub

private sub hExpectSkip( byval tk as integer )
	hExpect( tk )
	hSkip( )
end sub

private function hMatch( byval tk as integer ) as integer
	if( tkGet( x ) = tk ) then
		function = TRUE
		hSkip( )
	end if
end function

private function hExpectSkipString( ) as string
	hExpect( TK_STRING )
	function = *tkGetText( x )
	hSkip( )
end function

private function hFindVersion _
	( _
		byval pre as FROGPRESET ptr, _
		byval versionid as zstring ptr _
	) as ASTNODE ptr

	var version = pre->versions
	while( version )
		if( *version->text = *versionid ) then
			exit while
		end if
		version = version->next
	wend

	function = version
end function

private sub hLoadFile( byval file as FROGFILE ptr )
	lexLoadFile( x, file, LEXMODE_FBFROG, FALSE )
end sub

#if 0
private sub hIncludeFile( byref incfile as string )
	do include file search
	hLoadFile( ... )
end sub
#endif

sub presetParse( byval pre as FROGPRESET ptr, byref presetfile as string )
	tkInit( )

	x = 0
	verlevel = -1

	dim as FROGFILE presetf
	presetf.pretty = presetfile
	presetf.normed = presetfile
	hLoadFile( @presetf )

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
			hExpectSkip( KW_INCLUDE )

			'' "filename"
			var incfile = hExpectSkipString( )

			hExpectSkip( TK_EOL )
			hIncludeFile( incfile )
			continue do
#endif

		'' DECLARE VERSION "version id" [Identifier(version #define) ['=' NumberLiteral(version #define value)]]
		case KW_DECLARE
			hSkip( )

			'' VERSION
			hExpectSkip( KW_VERSION )

			'' "version id"
			hExpect( TK_STRING )
			var versionid = *tkGetText( x )
			if( hFindVersion( pre, versionid ) ) then
				hOops( "duplicate version" )
			end if
			astAppend( pre->versions, astNew( ASTCLASS_VERSION, versionid ) )
			hSkip( )

			'' Identifier?
			if( tkGet( x ) = TK_ID ) then
				var defineid = *tkGetText( x )
				hSkip( )

				'' '='?
				if( hMatch( TK_EQ ) ) then
					hExpectSkip( TK_DECNUM )
				end if
			end if

		'' VERSION "version id"
		case KW_VERSION
			verlevel += 1
			if( verlevel = MAXVERBLOCKS ) then
				hOops( "VERSION block stack too small, MAXVERBLOCKS=" & MAXVERBLOCKS )
			end if
			hSkip( )

			'' "version id"
			hExpect( TK_STRING )
			var version = hFindVersion( pre, tkGetText( x ) )
			if( version = NULL ) then
				hOops( "undeclared version" )
			end if
			verstack(verlevel) = version
			hSkip( )

		'' END VERSION
		case KW_END
			hSkip( )
			hExpectSkip( KW_VERSION )
			hExpectSkip( TK_EOL )

			if( verlevel < 0 ) then
				hOops( "END VERSION without corresponding VERSION block begin" )
			end if
			verlevel -= 1

		'' DOWNLOAD "URL" "output file name"
		case KW_DOWNLOAD
			hSkip( )
			var url = hExpectSkipString( )
			var outfile = hExpectSkipString( )

			var download = astNewTEXT( url )
			astSetComment( download, outfile )
			astAppend( pre->downloads, download )

		'' EXTRACT "tarball file name" "output directory name"
		case KW_EXTRACT
			hSkip( )
			var tarball = hExpectSkipString( )
			var outdir = hExpectSkipString( )

			var extract = astNewTEXT( tarball )
			astSetComment( extract, outdir )
			astAppend( pre->extracts, extract )

		'' COPYFILE "old name" "new name"
		case KW_COPYFILE
			hSkip( )
			var oldname = hExpectSkipString( )
			var newname = hExpectSkipString( )

			var copyfile = astNewTEXT( oldname )
			astSetComment( copyfile, newname )
			astAppend( pre->copyfiles, copyfile )

		'' FILE "file name"
		case KW_FILE
			hSkip( )
			presetAddFile( pre, hExpectSkipString( ) )

		'' DIR "dir name"
		case KW_DIR
			hSkip( )
			presetAddDir( pre, hExpectSkipString( ) )

		'' DEFINE Identifier
		case KW_DEFINE
			hSkip( )

			hExpect( TK_ID )
			astAppend( pre->defines, astNewID( tkGetText( x ) ) )
			hSkip( )

		'' UNDEF Identifier
		case KW_UNDEF
			hSkip( )

			hExpect( TK_ID )
			astAppend( pre->undefs, astNewID( tkGetText( x ) ) )
			hSkip( )

		'' EXPAND Identifier
		case KW_EXPAND
			hSkip( )

			hExpect( TK_ID )
			astAppend( pre->expands, astNewID( tkGetText( x ) ) )
			hSkip( )

		'' MACRO Identifier ['(' MacroParameters ')'] [MacroBody]
		case KW_MACRO
			hSkip( )

			hExpect( TK_ID )
			var macro = astNew( ASTCLASS_PPDEFINE, tkGetText( x ) )

			hMacroParamList( x, macro )

			hRecordMacroBody( x, macro )

			astAppend( pre->macros, macro )

		'' REMOVE (DEFINE|STRUCT|PROC|...) Identifier
		case KW_REMOVE
			hSkip( )

			var astclass = -1
			select case( tkGet( x ) )
			case KW_DEFINE
				astclass = ASTCLASS_PPDEFINE
			case else
				hOops( "unknown REMOVE command" )
			end select
			hSkip( )

			'' Identifier
			hExpect( TK_ID )
			astAppend( pre->removes, astNew( astclass, tkGetText( x ) ) )
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

		hExpectSkip( TK_EOL )
	loop

	if( verlevel >= 0 ) then
		hOops( "missing END VERSION" )
	end if

	tkEnd( )
end sub

sub presetInit( byval pre as FROGPRESET ptr )
	pre->versions = astNewGROUP( )

	pre->downloads = astNewGROUP( )
	pre->extracts  = astNewGROUP( )
	pre->copyfiles = astNewGROUP( )
	pre->files     = astNewGROUP( )
	pre->dirs      = astNewGROUP( )

	pre->defines = astNewGROUP( )
	pre->undefs  = astNewGROUP( )
	pre->expands = astNewGROUP( )
	pre->macros  = astNewGROUP( )

	pre->removes = astNewGROUP( )

	pre->options = 0
end sub

sub presetEnd( byval pre as FROGPRESET ptr )
	astDelete( pre->versions )

	astDelete( pre->downloads )
	astDelete( pre->extracts )
	astDelete( pre->copyfiles )
	astDelete( pre->files )
	astDelete( pre->dirs )

	astDelete( pre->defines )
	astDelete( pre->undefs )
	astDelete( pre->expands )
	astDelete( pre->macros )

	astDelete( pre->removes )
end sub

sub presetAddFile( byval pre as FROGPRESET ptr, byref filename as string )
	astAppend( pre->files, astNewTEXT( filename ) )
end sub

sub presetAddDir( byval pre as FROGPRESET ptr, byref dirname as string )
	astAppend( pre->dirs, astNewTEXT( dirname ) )
end sub

function presetHasInput( byval pre as FROGPRESET ptr ) as integer
	function = _
		(pre->downloads->head <> NULL) or _
		(pre->extracts->head <> NULL) or _
		(pre->copyfiles->head <> NULL) or _
		(pre->files->head <> NULL) or _
		(pre->dirs->head <> NULL)
end function

sub presetOverrideInput( byval a as FROGPRESET ptr, byval b as FROGPRESET ptr )
	astDelete( a->downloads )
	astDelete( a->extracts  )
	astDelete( a->copyfiles )
	astDelete( a->files     )
	astDelete( a->dirs      )
	a->downloads = astClone( b->downloads )
	a->extracts  = astClone( b->extracts  )
	a->copyfiles = astClone( b->copyfiles )
	a->files     = astClone( b->files     )
	a->dirs      = astClone( b->dirs      )
end sub

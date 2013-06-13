#include once "fbfrog.bi"

dim shared as FROGSTUFF frog

sub oops( byref message as string )
	print "oops, " & message
	end 1
end sub

function strReplace _
	( _
		byref text as string, _
		byref a as string, _
		byref b as string _
	) as string

	dim as string keep, result
	dim as integer alen = any, blen = any, i = any

	result = text

	alen = len( a )
	blen = len( b )

	i = 0
	do
		'' Does result contain an occurence of a?
		i = instr( i + 1, result, a )
		if( i = 0 ) then
			exit do
		end if

		'' Cut out a and insert b in its place
		'' result  =  front  +  b  +  back
		keep = right( result, len( result ) - ((i - 1) + alen) )
		result = left( result, i - 1 )
		result += b
		result += keep

		i += blen - 1
	loop

	function = result
end function

private sub frogEnd( )
	tkStats( )
	end 0
end sub

private sub hPrintHelp( )
	print "fbfrog 0.1 from " + __DATE_ISO__
	print "usage: fbfrog *.h"
	print "The given *.h file will be translated into a *.bi file. It needs reviewing"
	print "and editing afterwards, so watch out for TODOs and C/FB differences like"
	print "procedure calling conventions."
	print "options:"
	print "  -merge       Combine all files into one"
	print "  -verbose     Show debugging info"
	print "  -help, -version      Help and version output"
	end 0
end sub

private sub hAddFromDir( byref d as string )
	dim as TLIST list = any
	dim as string ptr s = any

	listInit( @list, sizeof( string ) )

	hScanDirectoryForH( d, @list )

	s = listGetHead( @list )
	while( s )
		depAdd( *s )
		*s = ""
		s = listGetNext( s )
	wend

	listEnd( @list )
end sub

private sub hParseArgs( byval argc as integer, byval argv as zstring ptr ptr )
	dim as string arg

	for i as integer = 1 to argc-1
		arg = *argv[i]

		'' option?
		if( left( arg, 1 ) = "-" ) then
			'' Strip all preceding '-'s
			do
				arg = right( arg, len( arg ) - 1 )
			loop while( left( arg, 1 ) = "-" )

			select case( arg )
			case "help", "version"
				hPrintHelp( )
			case "merge"
				frog.merge = TRUE
			case "verbose"
				frog.verbose = TRUE
			case "dep"
				frog.dep = TRUE
			case else
				'' "-l<name>" or "-l <name>"
				if( left( arg, 1 ) = "l" ) then
					'' Cut off the l
					arg = right( arg, len( arg ) - 1 )

					'' Now empty? then it was just "-l"
					if( len( arg ) = 0 ) then
						'' Use the following arg, if any, as <name>
						i += 1
						if( i < argc ) then
							arg = *argv[i]
						else
							oops( "missing argument for -l option, please use -l<name> or -l <name>" )
						end if
					end if

					frog.preset = arg
				elseif( len( arg ) > 0 ) then
					oops( "unknown option: '" + arg + "', try --help" )
				end if
			end select
		else
			select case( pathExtOnly( arg ) )
			case "h", "hh", "hxx", "hpp", "c", "cc", "cxx", "cpp"
				'' File from command line, search in current directory
				depAdd( arg )
			case ""
				'' No extension? Treat as directory...
				hAddFromDir( arg )
			case else
				oops( "'" + arg + "' is not a *.h file" )
			end select
		end if
	next
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	depInit( )
	fsInit( )

	hParseArgs( __FB_ARGC__, __FB_ARGV__ )

	if( fsGetHead( ) = NULL ) then
		oops( "no input files" )
	end if

	if( frog.dep ) then
		depScan( )
		depPrintFlat( )
		frogEnd( )
	end if

	''
	'' By default, all input files are translated 1:1. With -merge however,
	'' files with refcount = 1 are merged into their parent (in place of
	'' the #include statement), and files with refcount = 0 are appended.
	''
	'' Concat: Load & parse all files with refcount = 0 into the token buffer,
	'' one after another, so #includes (when merging) can be handled
	'' in the context of their parent, the current file.
	''
	'' Regular translation, 1:1, possibly with merges
	'' Pass 1: Translate everything that doesn't look like it'll be merged
	'' Pass 2: Translate files that looked like they should be merged,
	''         but weren't. (recursive #includes, all refcount > 0)
	''

	dim as FSFILE ptr f = any
	dim as ASTNODE ptr ast = any

	f = fsGetHead( )
	while( f )

		fsPush( f )
		tkInit( )
		lexLoadFile( 0, f->normed )
		print "translating: ";f->normed

		ppComments( )
		ppDividers( )
		ppDirectives1( )

		select case( frog.preset )
		case "zip"
			tkRemoveAllOf( TK_ID, "ZIP_EXTERN" )
		end select

		ppDirectives2( )

		'ppEvalIfs( )

		ast = cToplevel( )
		astDump( ast )
		emitWriteFile( pathStripExt( f->normed ) + ".bi", emitAst( ast ) )
		astDelete( ast )

		tkEnd( )
		fsPop( )

		f = listGetNext( f )
	wend

	frogEnd( )

#include once "fbfrog.bi"

dim shared as FROGSTUFF frog

sub oops( byref message as string )
	print "oops, " & message
	end 1
end sub

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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	dim as string arg

	depInit( )
	fsInit( )

	for i as integer = 1 to __FB_ARGC__-1
		arg = *__FB_ARGV__[i]

		if( len( arg ) = 0 ) then
			continue for
		end if

		if( arg[0] <> asc( "-" ) ) then
			'' file names will be handled later
			continue for
		end if

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
			if( len( arg ) > 0 ) then
				oops( "unknown option: '" + arg + "', try --help" )
			end if
		end select
	next

	'' Now that all options are known -- start adding the files
	for i as integer = 1 to __FB_ARGC__-1
		arg = *__FB_ARGV__[i]

		if( len( arg ) = 0 ) then
			continue for
		end if

		if( arg[0] = asc( "-" ) ) then
			continue for
		end if

		select case( pathExtOnly( arg ) )
		case "h", "hh", "hxx", "hpp", "c", "cc", "cxx", "cpp"
			'' File from command line, search in current directory
			depAdd( arg )
		case ""
			'' No extension? Treat as directory...
			hAddFromDir( arg )
		case else
			oops( "not a .h file: '" + arg + "'" )
		end select
	next

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

	print "translating..."
	f = fsGetHead( )
	while( f )

		fsPush( f )
		tkInit( )
		lexLoadFile( 0, f->normed )

		cPurgeInlineComments( )
		cPPDirectives( )
		cInsertDividers( )
		cToplevel( )

		emitWriteFile( pathStripExt( f->normed ) + ".bi" )

		tkEnd( )
		fsPop( )

		f = listGetNext( f )
	wend

	print "done: ";
	emitStats( )

	frogEnd( )

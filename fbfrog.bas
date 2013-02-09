#include once "fbfrog.bi"

dim shared as FROGSTUFF frog

sub oops( byref message as string )
	print "oops, " & message
	end 1
end sub

private sub frogInit( )
	hashInit( @frog.definehash, 6 )
end sub

function frogAddDefine _
	( _
		byval id as zstring ptr, _
		byval flags as uinteger _
	) as uinteger

	dim as integer length = any, dat = any
	dim as uinteger hash = any
	dim as HashItem ptr item = any

	length = len( *id )
	assert( length > 0 )

	hash = hashHash( id, length )
	item = hashLookup( @frog.definehash, id, length, hash )

	if( item->s ) then
		'' Already exists, add the flags. For lookups only, flags
		'' should be 0, so nothing changes.
		item->data = cast( any ptr, cuint( item->data ) or flags )
		return cuint( item->data )
	end if

	'' If no information, don't bother adding, allowing this function
	'' to be used for lookups only.
	if( flags ) then
		'' Add new
		dat = -1
		hashAdd( @frog.definehash, item, hash, _
		         storageStore( id, length, @dat ), length, cast( any ptr, flags ) )
	end if

	function = flags
end function

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
	dim as FROGFILE ptr first = any
	dim as integer x = any

	depInit( )
	frogInit( )
	fsInit( )
	storageInit( )

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

	depScan( )

	if( frog.dep ) then
		depPrintFlat( )
		end 0
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

	print "done: ";
	emitStats( )
	if( frog.verbose ) then
		hashStats( @frog.definehash, "define" )
		storageStats( )
	end if
	end 0

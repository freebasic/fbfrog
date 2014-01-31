#include once "dir.bi"

const NULL = 0
const FALSE = 0
const TRUE = -1

dim shared as string exe_path, cur_dir

function strStripPrefix( byref s as string, byref prefix as string ) as string
	if( left( s, len( prefix ) ) = prefix ) then
		function = right( s, len( s ) - len( prefix ) )
	else
		function = s
	end if
end function

#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
	const PATHDIV = $"\"
#else
	const PATHDIV = "/"
#endif

function pathAddDiv( byref path as string ) as string
	var s = path
	var length = len( s )

	if( length > 0 ) then
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
		select case( s[length-1] )
		case asc( "\" ), asc( "/" )

		case else
			s += "\"
		end select
#else
		if( s[length-1] <> asc( "/" ) ) then
			s += "/"
		end if
#endif
	end if

	function = s
end function

private function hFindFileName( byref path as string ) as integer
	for i as integer = len( path )-1 to 0 step -1
		select case( path[i] )
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
		case asc( "\" ), asc( "/" )
#else
		case asc( "/" )
#endif
			return i + 1
		end select
	next
end function

function pathOnly( byref path as string ) as string
	function = left( path, hFindFileName( path ) )
end function

function pathStripLastComponent( byref path as string ) as string
	function = pathOnly( left( path, len( path ) - 1 ) )
end function

sub hShell( byref ln as string )
	print "$ " + ln
	var result = shell( ln )
	if( result <> 0 ) then
		if( result = -1 ) then
			print "command not found: '" + ln + "'"
		else
			print "'" + ln + "' terminated with exit code " + str( result )
		end if
		end 1
	end if
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub hTest( byref fbfrogfile as string )
	var fbfrog = pathStripLastComponent( exe_path ) + "fbfrog"
	fbfrogfile = strStripPrefix( fbfrogfile, cur_dir )
	if( cur_dir + "tests" + PATHDIV = exe_path ) then
		fbfrog = "./fbfrog"
	end if
	hShell( fbfrog + " @" + fbfrogfile )
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' Directory tree search

type DIRNODE
	next		as DIRNODE ptr
	path		as string
end type

type DIRQUEUE
	head		as DIRNODE ptr
	tail		as DIRNODE ptr
end type

dim shared as DIRQUEUE dirs

private sub dirsAppend( byref path as string )
	dim as DIRNODE ptr node = callocate( sizeof( DIRNODE ) )
	node->path = pathAddDiv( path )
	if( dirs.tail ) then
		dirs.tail->next = node
	end if
	dirs.tail = node
	if( dirs.head = NULL ) then
		dirs.head = node
	end if
end sub

private sub dirsDropHead( )
	if( dirs.head ) then
		var node = dirs.head
		dirs.head = node->next
		if( dirs.head = NULL ) then
			dirs.tail = NULL
		end if
		node->path = ""
		deallocate( node )
	end if
end sub

private sub hScanParent( byref parent as string, byref filepattern as string )
	'' Scan for files
	var found = dir( parent + filepattern, fbNormal )
	while( len( found ) > 0 )
		hTest( parent + found )
		found = dir( )
	wend

	'' Scan for subdirectories
	found = dir( parent + "*", fbDirectory or fbReadOnly )
	while( len( found ) > 0 )
		select case( found )
		case ".", ".."
			'' Ignore these special subdirectories

		case else
			'' Remember the subdirectory for further scanning
			dirsAppend( parent + found )
		end select

		found = dir( )
	wend
end sub

sub hScanDirectory( byref rootdir as string, byref filepattern as string )
	dirsAppend( rootdir )

	'' Work off the queue -- each subdir scan can append new subdirs
	while( dirs.head )
		hScanParent( dirs.head->path, filepattern )
		dirsDropHead( )
	wend
end sub

exe_path = pathAddDiv( exepath( ) )
cur_dir = pathAddDiv( curdir( ) )
hScanDirectory( exe_path, "*.fbfrog" )

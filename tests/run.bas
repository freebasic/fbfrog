#include once "dir.bi"

const NULL = 0
const FALSE = 0
const TRUE = -1

type TESTCALLBACK as sub( byref as string )

type STATS
	oks as integer
	fails as integer
end type

dim shared stat as STATS
dim shared as string exe_path, cur_dir, fbfrog

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

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

function hConsoleWidth( ) as integer
	dim as integer w = loword( width( ) )
	if( w < 0 ) then
		w = 0
	end if
	function = w
end function

sub hShell _
	( _
		byref prefix as string, _
		byref ln as string, _
		byval expectedresult as integer _
	)

	'print "$ " + ln
	var result = shell( ln )

	if( result = -1 ) then
		print "command not found: '" + ln + "'"
		end 1
	end if

	dim suffix as string
	if( result = expectedresult ) then
		suffix = "[ ok ]"
		stat.oks += 1
	else
		suffix = "[fail]"
		stat.fails += 1
	end if

	print prefix + space( hConsoleWidth( ) - len( prefix ) - len( suffix ) ) + suffix

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

private sub hScanParent _
	( _
		byref parent as string, _
		byref filepattern as string, _
		byval testcallback as TESTCALLBACK _
	)

	'' Scan for files
	var found = dir( parent + filepattern, fbNormal )
	while( len( found ) > 0 )
		testcallback( strStripPrefix( parent + found, cur_dir ) )
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

sub hScanDirectory _
	( _
		byref rootdir as string, _
		byref filepattern as string, _
		byval testcallback as TESTCALLBACK _
	)

	dirsAppend( rootdir )

	'' Work off the queue -- each subdir scan can append new subdirs
	while( dirs.head )
		hScanParent( dirs.head->path, filepattern, testcallback )
		dirsDropHead( )
	wend

end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub hDelete( byref file as string )
	var dummy = kill( file )
end sub

sub hTestPreset( byref fbfrogfile as string )
	hShell( "PRESET " + fbfrogfile, _
		fbfrog + " @" + fbfrogfile + " > " + fbfrogfile + ".txt 2>&1", 0 )
end sub

sub hTestError( byref hfile as string )
	hShell( "ERROR " + hfile, _
		fbfrog + " " + hfile + " > " + hfile + ".txt 2>&1", 1 )
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

exe_path = pathAddDiv( exepath( ) )
cur_dir = pathAddDiv( curdir( ) )
fbfrog = pathStripLastComponent( exe_path ) + "fbfrog"
if( cur_dir + "tests" + PATHDIV = exe_path ) then
	fbfrog = "./fbfrog"
end if

for i as integer = 1 to __FB_ARGC__-1
	select case( *__FB_ARGV__[i] )
	case "-clean"
		hScanDirectory( exe_path, "*.txt", @hDelete )
		hScanDirectory( exe_path, "*.bi", @hDelete )
		end 0
	case else
		print "unknown option: " + *__FB_ARGV__[i]
		end 1
	end select
next

hScanDirectory( exe_path, "*.fbfrog", @hTestPreset )
hScanDirectory( exe_path + "errors" + PATHDIV, "*.h", @hTestError )
print "  " & stat.oks & " tests ok, " & stat.fails & " failed"

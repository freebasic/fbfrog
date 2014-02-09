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

const MAXFILES = 1 shl 10

namespace files
	dim shared as string list(0 to MAXFILES-1)
	dim shared as integer count
end namespace

sub hForgetFiles( )
	files.count = 0
end sub

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

		if( files.count >= MAXFILES ) then
			print "MAXFILE is too small"
			end 1
		end if
		files.list(files.count) = strStripPrefix( parent + found, cur_dir )
		files.count += 1

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

private function hPartition _
	( _
		byval l as integer, _
		byval m as integer, _
		byval r as integer _
	) as integer

	dim as string pivot = files.list(m)
	dim as integer store = l

	swap files.list(m), files.list(r)

	for i as integer = l to r - 1
		if( lcase( files.list(i) ) <= lcase( pivot ) ) then
			swap files.list(i), files.list(store)
			store += 1
		end if
	next

	swap files.list(store), files.list(r)

	function = store
end function

private sub hQuickSort( byval l as integer, byval r as integer )
	if( l < r ) then
		dim as integer m = l + ((r - l) \ 2)
		m = hPartition( l, m, r )
		hQuickSort( l, m - 1 )
		hQuickSort( m + 1, r )
	end if
end sub

sub hSortFiles( )
	hQuickSort( 0, files.count - 1 )
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub hDeleteFiles( )
	for i as integer = 0 to files.count-1
		var dummy = kill( files.list(i) )
	next
end sub

exe_path = pathAddDiv( exepath( ) )
cur_dir = pathAddDiv( curdir( ) )
fbfrog = pathStripLastComponent( exe_path ) + "fbfrog"
if( cur_dir + "tests" + PATHDIV = exe_path ) then
	fbfrog = "./fbfrog"
end if

var clean_only = FALSE
for i as integer = 1 to __FB_ARGC__-1
	select case( *__FB_ARGV__[i] )
	case "-clean"
		clean_only = TRUE
	case else
		print "unknown option: " + *__FB_ARGV__[i]
		end 1
	end select
next

'' Clean test directories: Delete all *.txt and *.bi files
hScanDirectory( exe_path, "*.txt" )
hScanDirectory( exe_path, "*.bi" )
hDeleteFiles( )
hForgetFiles( )

if( clean_only ) then
	end 0
end if

'' Run fbfrog on all found *.fbfrog files (testing that parsing/AST stuff works)
hScanDirectory( exe_path, "*.fbfrog" )
hSortFiles( )
for i as integer = 0 to files.count-1
	var f = files.list(i)
	hShell( "PRESET " + f, fbfrog + " @" + f + " > " + f + ".txt 2>&1", 0 )
next
hForgetFiles( )

'' Run fbfrog on all found errors/*.h files (testing for error messages)
hScanDirectory( exe_path + "errors" + PATHDIV, "*.h" )
hSortFiles( )
for i as integer = 0 to files.count-1
	var f = files.list(i)
	hShell( "ERROR " + f, fbfrog + " " + f + " > " + f + ".txt 2>&1", 1 )
next
hForgetFiles( )

print "  " & stat.oks & " tests ok, " & stat.fails & " failed"

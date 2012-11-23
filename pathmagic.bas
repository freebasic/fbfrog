#include once "fbfrog.bi"
#include once "crt.bi"
#include once "dir.bi"

'' Searches backwards for the last '.' while still behind '/' or '\'.
private function hFindExtBegin( byref path as string ) as integer
	for i as integer = len( path )-1 to 0 step -1
		select case( path[i] )
		case asc( "." )
			return i
		case asc( "/" ), asc( "\" )
			exit for
		end select
	next
	function = len( path )
end function

function pathStripExt( byref path as string ) as string
	function = left( path, hFindExtBegin( path ) )
end function

function pathExtOnly( byref path as string ) as string
	function = right( path, len( path ) - hFindExtBegin( path ) - 1 )
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

function pathAddDiv( byref path as string ) as string
	dim as string s
	dim as integer length = any

	s = path
	length = len( s )

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

private function pathGetRootLength( byref s as string ) as integer
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
	if( len( s ) >= 3 ) then
		'' x:\...
		if( s[1] = asc( ":" ) ) then
			select case( s[2] )
			case asc( "/" ), asc( "\" )
				return 3
			end select
		end if
	end if
#ifdef __FB_WIN32__
	'' UNC paths
	if( len( s ) >= 2 ) then
		'' \\...
		if( s[0] = asc( "\" ) ) then
			if( s[1] = asc( "\" ) ) then
				return 2
			end if
		end if
	end if
#endif
#else
	if( len( s ) >= 1 ) then
		'' /...
		if( s[0] = asc( "/" ) ) then
			return 1
		end if
	end if
#endif
end function

function pathStripLastComponent( byref path as string ) as string
	function = pathOnly( left( path, len( path ) - 1 ) )
end function

function pathFindCommonBase _
	( _
		byref aorig as string, _
		byref borig as string _
	) as string

	dim as string a, b
	dim as integer aroot = any, broot = any, alen = any, blen = any

	a = aorig
	b = borig
	aroot = pathGetRootLength( a )
	broot = pathGetRootLength( b )

	do
		alen = len( a )
		blen = len( b )

		if( (alen < aroot) or (blen < broot) ) then
			exit do
		end if

		if( alen = blen ) then
			if( a = b ) then
				return a
			end if
		end if

		if( alen > blen ) then
			a = pathStripLastComponent( a )
		else
			b = pathStripLastComponent( b )
		end if
	loop

end function

function pathStripCommonBase _
	( _
		byref a as string, _
		byref b as string _
	) as string
	function = right( a, len( a ) - len( pathFindCommonBase( a, b ) ) )
end function

'' Component stack for the path solver
type PATHSOLVER
	p	as integer ptr
	room	as integer
	top	as integer
end type

dim shared as PATHSOLVER solver

private sub solverInit( )
	solver.p = NULL
	solver.room = 0
	solver.top = -1
end sub

private sub solverEnd( )
	deallocate( solver.p )
end sub

private sub solverPush( byval w as integer )
	solver.top += 1
	if( solver.top >= solver.room ) then
		solver.room += 128
		solver.p = reallocate( solver.p, sizeof( integer ) * solver.room )
	end if
	solver.p[solver.top] = w
end sub

private function solverPop( ) as integer
	if( solver.top > 0 ) then
		solver.top -= 1
	end if
	function = solver.p[solver.top]
end function

'' Turns a relative path into an absolute path
function pathMakeAbsolute( byref path as string ) as string
	if( pathGetRootLength( path ) = 0 ) then
		function = pathAddDiv( curdir( ) ) + path
	else
		function = path
	end if
end function

'' Resolves .'s and ..'s in the path,
'' normalizes path separators to the host standard.
function pathNormalize( byref path as string ) as string
	dim as integer rootlen = any, dots = any, chars = any, r = any, w = any
	dim as string s

	rootlen = pathGetRootLength( path )
	if( rootlen = 0 ) then
		return path
	end if

	'' r: read position, w: write position
	'' r reads ahead, while w slowly writes out the result.
	'' First r and w stay together, but as soon as r finds a .., w is set
	'' back a bit, right in front of the path component it wrote last, so
	'' that the component is dropped (it will be overwritten by following
	'' components).
	'' The stack is needed to be able to skip back over multiple components
	'' in succession, for example in 'aa/bb/../..'.

	'' r and w start out behind the root path (/ or C:\ etc.) so that it's
	'' not touched. The begin of the first component after the root path
	'' must be on the stack to be able to skip back to it (although the
	'' begin of the root path itself, 0, is not on the stack, so it can't
	'' be removed with a '..').
	solverInit( )
	solverPush( rootlen )

	s = path
	dots = 0 '' Number of .'s in the current component
	chars = 0 '' Number of chars in the current component
	w = rootlen

	for r = rootlen to len( s )-1
		select case( s[r] )
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
		case asc( "\" ), asc( "/" )
#else
		case asc( "/" )
#endif
			'' Component closed: check whether it was /./ or /../
			select case( dots )
			case 1    '' /./
				'' Ignore: don't write this /, and remove the .
				w -= 1

			case 2    '' /../
				'' Go back to the begin of the component this
				'' '..' refers to
				w = solverPop( )

			case else
				if( chars = 0 ) then
					'' // (Ignore: don't write this /)
				else
					'' Write this /
#if defined( __FB_WIN32__ ) or defined( __FB_DOS__ )
					s[w] = asc( "\" ) '' This also normalizes / to \
#else
					s[w] = asc( "/" )
#endif
					'' New component starts behind this /
					w += 1
					'' Remember this begin position so
					'' w can be reset to it during '..'.
					solverPush( w )
				end if

			end select

			dots = 0
			chars = 0

		case asc( "." )
			dots += 1
			chars += 1
			s[w] = s[r]
			w += 1

		case else
			dots = 0
			chars += 1
			s[w] = s[r]
			w += 1

		end select
	next

	solverEnd( )
	function = left( s, w )
end function

function hFileExists( byref file as string ) as integer
	dim as integer f = any
	f = freefile( )
	if( open( file, for binary, access read, as #f ) = 0 ) then
		close #f
		function = TRUE
	else
		function = FALSE
	end if
end function

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
	dim as DIRNODE ptr node = any

	node = callocate( sizeof( DIRNODE ) )
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
	dim as DIRNODE ptr node = any
	if( dirs.head ) then
		node = dirs.head
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
		byval resultlist as LINKEDLIST ptr _
	)

	dim as string found

	'' Scan for *.h files
	found = dir( parent + "*.h", fbNormal )
	while( len( found ) > 0 )
		'' Add the file name to the result list
		*cptr( string ptr, listAppend( resultlist ) ) = parent + found

		found = dir( )
	wend

	'' Scan for subdirectories
	found = dir( parent + "*", fbDirectory )
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

sub hScanDirectoryForH _
	( _
		byref rootdir as string, _
		byval resultlist as LINKEDLIST ptr _
	)

	dirsAppend( rootdir )

	print "scanning tree for *.h files: '" + dirs.head->path + "'"

	'' Work off the queue -- each subdir scan can append new subdirs
	while( dirs.head )
		hScanParent( dirs.head->path, resultlist )
		dirsDropHead( )
	wend

end sub

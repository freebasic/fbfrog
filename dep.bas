'' Dependency graph

#include once "fbfrog.bi"

type DEPSTUFF
	nodes as TLIST  '' DEPNODE's
end type

dim shared as DEPSTUFF dep

sub depInit( )
	listInit( @dep.nodes, sizeof( DEPNODE ) )
end sub

function depLookup( byval f as FSFILE ptr ) as DEPNODE ptr
	dim as DEPNODE ptr node = any

	node = listGetHead( @dep.nodes )
	while( node )

		if( node->f = f ) then
			exit while
		end if

		node = listGetNext( node )
	wend

	function = node
end function

function depAdd( byref pretty as string ) as DEPNODE ptr
	dim as FSFILE ptr f = any
	dim as DEPNODE ptr node = any

	f = fsAdd( pretty )
	if( f = NULL ) then
		return NULL
	end if

	'' No node for that file yet?
	node = depLookup( f )
	if( node = NULL ) then
		'' Add new node with refcount = 0
		node = listAppend( @dep.nodes )
		node->f = f
	end if

	function = node
end function

sub depOn( byval node as DEPNODE ptr, byval child as DEPNODE ptr )
	if( node->childcount = node->childroom ) then
		node->childroom += 4
		node->children = reallocate( node->children, sizeof( DEPNODE ptr ) * node->childroom )
	end if
	node->children[node->childcount] = child
	node->childcount += 1
end sub

private sub hParseFileForIncludes( byval node as DEPNODE ptr )
	dim as DEPNODE ptr incnode = any
	dim as integer x = any

	if( len( node->f->normed ) = 0 ) then
		node->missing = TRUE
		exit sub
	end if

	fsPush( node->f )
	tkInit( )
	lexLoadFile( 0, node->f->normed )
	cPPDirectives( )

	x = 0
	do
		select case( tkGet( x ) )
		case TK_PPINCLUDE
			incnode = depAdd( *tkGetText( x ) )
			if( incnode ) then
				depOn( node, incnode )
			end if
		case TK_EOF
			exit do
		end select

		x += 1
	loop

	tkEnd( )
	fsPop( )
end sub

sub depScan( )
	dim as DEPNODE ptr node = any

	node = listGetHead( @dep.nodes )
	while( node )
		hParseFileForIncludes( node )
		node = listGetNext( node )
	wend
end sub

function depCalcRefcount( byval node as DEPNODE ptr ) as integer
	dim as DEPNODE ptr i = any
	dim as integer refcount = any

	refcount = 0
	i = listGetHead( @dep.nodes )
	while( i )

		for j as integer = 0 to i->childcount - 1
			if( i->children[j] = node ) then
				refcount += 1
			end if
		next

		i = listGetNext( i )
	wend

	function = refcount
end function

sub depPrintFlat( )
	dim as DEPNODE ptr node = any

	node = listGetHead( @dep.nodes )
	while( node )
		print "(" & depCalcRefcount( node ) & ")";" ";node->f->pretty;":"

		if( node->missing ) then
			print , "(file not found)"
		elseif( node->childcount = 0 ) then
			print , "(no #includes)"
		else
			for i as integer = 0 to node->childcount - 1
				print , node->children[i]->f->pretty
			next
		end if


		node = listGetNext( node )
	wend
end sub

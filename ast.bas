#include once "fbfrog.bi"

type ASTSTUFF
	statements	as TLIST '' ASTNODE ptr's
end type

dim shared as ASTSTUFF ast

sub astInit( )
	listInit( @ast.statements, sizeof( ASTNODE ptr ) )
end sub

sub astEnd( )
	dim as ASTNODE ptr ptr stmt = any

	stmt = listGetHead( @ast.statements )
	while( stmt )
		astDelete( *stmt )
		stmt = listGetNext( stmt )
	wend

	listEnd( @ast.statements )
end sub

sub astAdd( byval t as ASTNODE ptr )
	dim as ASTNODE ptr ptr stmt = any

	stmt = listAppend( @ast.statements )
	*stmt = t
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function astNew( byval class_ as integer ) as ASTNODE ptr
	dim as ASTNODE ptr n = any

	n = callocate( sizeof( ASTNODE ) )
	n->class = class_

	function = n
end function

sub astDelete( byval t as ASTNODE ptr )
	deallocate( t )
end sub

function astNewVARDECL _
	( _
		byval id as zstring ptr, _
		byval dtype as integer, _
		byval subtype as zstring ptr _
	) as ASTNODE ptr

	dim as ASTNODE ptr n = any

	n = astNew( ASTCLASS_VARDECL )
	n->vardecl.id = strDuplicate( id )
	n->vardecl.dtype = dtype
	n->vardecl.subtype = strDuplicate( subtype )

	function = n
end function

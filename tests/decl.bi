'' variables
extern     a as long
dim shared a as long
extern     a as long ptr
dim shared a as long ptr
extern     a as long ptr ptr
dim shared a as long ptr ptr
extern     a as long ptr ptr ptr
dim shared a as long ptr ptr ptr
extern     a as long ptr ptr ptr ptr
dim shared a as long ptr ptr ptr ptr
extern     a as long
dim shared a as long
extern     b as long
dim shared b as long
extern     a as long
dim shared a as long
extern     b as long
dim shared b as long
extern     c as long
dim shared c as long
extern     d as long
dim shared d as long
extern     a as long ptr
dim shared a as long ptr
extern     b as long
dim shared b as long
extern     a as long
dim shared a as long
extern     b as long ptr
dim shared b as long ptr
extern     a as long ptr
dim shared a as long ptr
extern     b as long ptr
dim shared b as long ptr
extern     a as long ptr
dim shared a as long ptr
extern     b as long
dim shared b as long
extern     c as long ptr ptr
dim shared c as long ptr ptr
extern     a as long ptr ptr
dim shared a as long ptr ptr
extern     b as long
dim shared b as long
extern     c as long ptr
dim shared c as long ptr
extern     a as long
dim shared a as long
extern     b as long ptr
dim shared b as long ptr
extern     c as long
dim shared c as long
extern     a as long ptr
dim shared a as long ptr
extern     b as long ptr ptr
dim shared b as long ptr ptr
extern     c as long ptr ptr ptr
dim shared c as long ptr ptr ptr
extern     a as long
dim shared a as long
extern     b as long ptr
dim shared b as long ptr
extern     c as long ptr ptr ptr
dim shared c as long ptr ptr ptr
extern     d as long
dim shared d as long
extern     a as long ptr ptr
dim shared a as long ptr ptr
extern     b as long
dim shared b as long
extern     c as long
dim shared c as long
extern     d as long ptr ptr
dim shared d as long ptr ptr
extern     a as long
dim shared a as long
extern     b as long ptr
dim shared b as long ptr
extern     c as long
dim shared c as long
extern     d as long ptr
dim shared d as long ptr
extern     a as long ptr
dim shared a as long ptr
extern     b as long
dim shared b as long
extern     c as long ptr
dim shared c as long ptr
extern     d as long ptr ptr ptr
dim shared d as long ptr ptr ptr

'' sub/function return types
declare sub f( )
declare function f( ) as long
declare function f( ) as long ptr
declare function f( ) as UDT
declare function f( ) as UDT ptr ptr

'' no parameters
declare sub f( )
declare sub f( )

'' parameters
declare sub f( byval a as long )
declare sub f( byval a as long, byval b as long )
declare sub f( byval a as long, byval b as long, byval c as long )
declare sub f( byval a as long ptr, byval b as long ptr ptr ptr )

'' anonymous parameters
declare sub f( byval as long )
declare sub f( byval as long, byval as long )
declare sub f( byval as long, byval as long, byval as long )
declare sub f( byval as long ptr, byval as long ptr ptr ptr )

declare sub f( byval a as long, ... ) '' vararg

'' procptr variables
extern     a as sub( )
dim shared a as sub( )
extern     a as function( byval as long ) as long
dim shared a as function( byval as long ) as long
extern     a as function( byval a as long ) as long
dim shared a as function( byval a as long ) as long
extern     b as function( byval a as long ) as long
dim shared b as function( byval a as long ) as long
extern     c as long
dim shared c as long
extern     a as long
dim shared a as long
extern     b as function( byval a as long ) as long
dim shared b as function( byval a as long ) as long
extern     c as long
dim shared c as long
extern     d as function( byval a as long ) as long
dim shared d as function( byval a as long ) as long
extern     a as function( byval as long ) as long ptr ptr
dim shared a as function( byval as long ) as long ptr ptr

'' TODO: unknown construct (sorry)
int (*f(void))(void);

'' procptr as param
declare sub f( byval a as sub( ) )
declare sub f( byval as sub( ) )

'' procptr with procptr as param
extern     a as sub( byval a as sub( ) )
dim shared a as sub( byval a as sub( ) )
extern     p as function( byval as function( byval as long ptr ptr ptr ) as long ptr ptr ptr ) as long ptr ptr ptr
dim shared p as function( byval as function( byval as long ptr ptr ptr ) as long ptr ptr ptr ) as long ptr ptr ptr

extern     a as long '' variable/procptr/function
dim shared a as long
extern     a as function( ) as long ptr
dim shared a as function( ) as long ptr
declare function a( ) as long
declare function a( ) as long ptr

'' static/extern
dim shared a as long
extern a as long
declare sub f( )
declare sub f( )

'' typedefs
type A as UDT
type A as long
type A as long ptr
type A as long
type B as long
type C as long
type A as sub( )
type A as function( byval as UDT ) as UDT
type A as UDT ptr ptr
type B as UDT
type C as function( byval as long ) as UDT

type UDT

	a as long '' fields
	a as long ptr ptr
	a as long
	b as long
	a as long ptr
	b as long
	c as long ptr
	d as long ptr ptr ptr

	declare sub f( ) '' methods
	declare function f( ) as long
	declare sub f( )
	declare function f( ) as UDT ptr ptr
	declare sub f( byval a as long ptr, byval b as long ptr ptr ptr )

	a as sub( ) '' procptr fields
	a as function( byval as long ) as long
	a as function( byval a as long ) as long
	b as function( byval a as long ) as long
	c as long
	a as long
	b as function( byval a as long ) as long
	c as long
	d as function( byval a as long ) as long
	a as function( byval as long ) as long ptr ptr

	declare sub f( byval a as sub( ) ) '' procptr as param
	declare sub f( byval as sub( ) )

	a as sub( byval a as sub( ) ) '' procptr with procptr as param
	p as function( byval as function( byval as long ptr ptr ptr ) as long ptr ptr ptr ) as long ptr ptr ptr
end type

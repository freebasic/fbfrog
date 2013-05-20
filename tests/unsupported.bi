
declare function f( byval as long ) as function( byval as long ) as function( byval as long ) as long

extern     p as function( byval as long ) as function( byval as long ) as long
dim shared p as function( byval as long ) as function( byval as long ) as long

declare function f( byval as long ) as long
extern     p as function( byval as long ) as function( byval as long ) as long
dim shared p as function( byval as long ) as function( byval as long ) as long
declare function f( byval as long ) as function( byval as long ) as function( byval as long ) as long

extern     pp as typeof( function( ) as long ) ptr
dim shared pp as typeof( function( ) as long ) ptr

type T as long

'' TODO: unknown construct
enum {#if 1	A    #endif};

'' TODO: unknown construct
enum {	A =	1#if 1	+	2#endif	,	B};

type T
	'' TODO: unknown construct
	union {		int a;	} foo;
end type


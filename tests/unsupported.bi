
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

enum
	#if 1
	'' TODO: unknown construct
	A    
	
	#endif
end enum

enum
	'' TODO: unknown construct
	A =
	 1
	
	#if 1
	'' TODO: unknown construct
	+
	 2
	
	#endif
	'' TODO: unknown construct
	,
	 
	B
end enum

type T
	'' TODO: unknown construct
	union {
	  int a;
	 } foo;
	
end type

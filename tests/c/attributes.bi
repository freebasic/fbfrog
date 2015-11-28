#pragma once

extern "C"

declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare function malloc(byval as uinteger) as any ptr
declare function calloc(byval as uinteger, byval as uinteger) as any ptr
declare function realloc(byval as any ptr, byval as uinteger) as any ptr
declare sub f()
declare sub f()
declare sub printf(byval as zstring ptr, ...)
declare sub f()
declare sub f()
type int_may_alias as long
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()

#ifdef __FB_WIN32__
	declare sub f()
	declare sub f stdcall()
#endif

declare sub f stdcall()
declare sub f()
extern import i as long

#ifdef __FB_WIN32__
	extern import i as long
	extern import i as long
	extern import i as long
	'' TODO: __attribute__((dllimport)) static int a;
	'' TODO: static __attribute__((dllimport)) int b;
	extern i as long
	declare sub f()
#endif

declare sub f()
declare sub f()
declare sub f()
dim shared attribute_defines as long

type alignment_
	i as long
end type

type alignment as alignment_
dim shared before_base_type as long
declare sub f stdcall()
dim shared behind_base_type as long
declare sub f stdcall()
dim shared in_nested_declarator as long
declare sub f stdcall()
dim shared at_end_of_declarator as long
declare sub f stdcall()
declare sub f(byval i as long = 0)
dim shared before_extern as long
declare sub f stdcall()
dim shared between_extern_and_base_type as long
declare sub f stdcall()
dim shared in_middle_of_base_type as long
declare function f stdcall() as const short
dim shared all_should_be_stdcall as long
declare sub f1 stdcall()
declare sub f2 stdcall()
declare sub f1 stdcall()
declare sub f2 stdcall()
dim shared only_f1_should_be_stdcall as long
declare sub f1 stdcall()
declare sub f2()
declare sub f1 stdcall()
declare sub f2()
dim shared only_f2_should_be_stdcall as long
declare sub f1()
declare sub f2 stdcall()
declare sub f1()
declare sub f2 stdcall()
declare sub f1()
declare sub f2 stdcall()

dim shared all_stdcall_function_pointers as long
extern f1 as sub stdcall()
extern f1 as sub stdcall()
extern f1 as sub stdcall()
extern f1 as sub stdcall()
extern f1 as sub stdcall()
extern separator1 as long
extern _function_f__byval_a_as_long___as_sub_stdcall__byval_b_as_long as long
declare function f(byval a as long) as sub stdcall(byval b as long)
extern separator2 as long
extern _function_f__byval_a_as_long___as_sub_stdcall__byval_b_as_long as long
declare function f(byval a as long) as sub stdcall(byval b as long)
extern separator3 as long
extern _function_f_stdcall__byval_a_as_long___as_sub__byval_b_as_long as long
declare function f stdcall(byval a as long) as sub(byval b as long)
extern separator4 as long
extern _function_f_stdcall__byval_a_as_long___as_sub__byval_b_as_long as long
declare function f stdcall(byval a as long) as sub(byval b as long)
extern separator5 as long
extern _function_f_stdcall__byval_a_as_long___as_sub__byval_b_as_long as long
declare function f stdcall(byval a as long) as sub(byval b as long)
extern separator6 as long
extern _function_f_stdcall__byval_a_as_long___as_function__byval_b_as_long___as_sub__byval_c_as_long as long
declare function f stdcall(byval a as long) as function(byval b as long) as sub(byval c as long)
extern separator7 as long
extern _function_f_stdcall__byval_a_as_long___as_function__byval_b_as_long___as_sub__byval_c_as_long as long
declare function f stdcall(byval a as long) as function(byval b as long) as sub(byval c as long)
extern separator8 as long
extern _function_f_stdcall__byval_a_as_long___as_function__byval_b_as_long___as_sub__byval_c_as_long as long
declare function f stdcall(byval a as long) as function(byval b as long) as sub(byval c as long)
extern separator9 as long
extern _function_f__byval_a_as_long___as_function__byval_b_as_long___as_sub_stdcall__byval_c_as_long as long
declare function f(byval a as long) as function(byval b as long) as sub stdcall(byval c as long)
extern separator10 as long
extern _function_f__byval_a_as_long___as_function_stdcall__byval_b_as_long___as_sub__byval_c_as_long as long
declare function f(byval a as long) as function stdcall(byval b as long) as sub(byval c as long)
extern separator11 as long
extern p_as_function_stdcall__byval_a_as_long___as_sub__byval_b_as_long as long
extern p as function stdcall(byval a as long) as sub(byval b as long)
extern p as function stdcall(byval a as long) as sub(byval b as long)
extern p as function stdcall(byval a as long) as sub(byval b as long)
extern separator12 as long
extern p_as_function__byval_a_as_long___as_sub_stdcall__byval_b_as_long as long
extern p as function(byval a as long) as sub stdcall(byval b as long)
extern separator13 as long
extern _sub_f_stdcall as long

declare sub f stdcall()
declare sub f stdcall()
declare sub f stdcall()
extern separator14 as long
extern _function_f_stdcall____as_sub as long
declare function f stdcall() as sub()
declare function f stdcall() as sub()
declare function f stdcall() as sub()

extern separator15 as long
extern p_as_sub_stdcall as long
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()
extern p as sub stdcall()

declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()
declare sub f()

type A1 field = 1
	a as short
	b as long
end type

type A2 field = 1
	a as short
	b as long
end type

type A3 field = 1
	a as short
	b as long
end type

type A4 field = 1
	a as short
	b as long
end type

extern     A4_ as A4
dim shared A4_ as A4

type A5 field = 1
	a as short
	b as long
end type

extern     A5_ as A5
dim shared A5_ as A5

type B1 field = 1
	a as short
	b as long
end type

type UDT4 as B1

type B2 field = 1
	a as short
	b as long
end type

type UDT5 as B2

type B3 field = 1
	a as short
	b as long
end type

type PFUDT6 as function() as B3

type C1
	a as short
	b as long
end type

type PFC1 as function() as C1

type C2 field = 1
	a as short
	b as long
end type

type PFC2 as function() as C2

type C3 field = 1
	a as short
	b as long
end type

type PFC3 as function() as C3

type C4
	a as short
	b as long
end type

type C5 field = 1
	a as short
	b as long
end type

type C6 field = 1
	a as short
	b as long
end type

type D1
	a as short
	b as long
end type

extern     PFD1 as function() as D1
dim shared PFD1 as function() as D1

type D2 field = 1
	a as short
	b as long
end type

extern     PFD2 as function() as D2
dim shared PFD2 as function() as D2

type D3 field = 1
	a as short
	b as long
end type

extern     PFD3 as function() as D3
dim shared PFD3 as function() as D3

type D4
	a as short
	b as long
end type

declare function FD4() as D4

type D5 field = 1
	a as short
	b as long
end type

declare function FD5() as D5

type D6 field = 1
	a as short
	b as long
end type

declare function FD6() as D6
type myint64 as longint
extern a as const long ptr

private sub f()
end sub

private sub f()
end sub

private sub f()
end sub

declare sub f()

private sub f()
end sub

private sub f()
end sub

private sub f()
end sub

private sub f(byval p as any ptr)
end sub

extern p as sub stdcall(byval p as sub stdcall())

type T
	i as long
end type

type ShouldBePacked field = 1
	i as long
end type

private sub ShouldBeStdcall stdcall()
end sub

end extern

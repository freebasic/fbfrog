declare sub f()
declare sub f()

declare sub f()
declare sub f()

declare sub f()
declare sub f()

declare sub f()
declare sub f()

declare sub f cdecl()
declare sub f cdecl()
declare sub f cdecl()

declare sub f stdcall()
declare sub f stdcall()
declare sub f stdcall()

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

''      function f( byval a as long ) as sub stdcall( byval b as long )
extern _function_f__byval_a_as_long___as_sub_stdcall__byval_b_as_long as long
declare function f(byval a as long) as sub stdcall(byval b as long)

extern separator2 as long

''      function f( byval a as long ) as sub stdcall( byval b as long )
extern _function_f__byval_a_as_long___as_sub_stdcall__byval_b_as_long as long
declare function f(byval a as long) as sub stdcall(byval b as long)

extern separator3 as long

''      function f stdcall( byval a as long ) as sub( byval b as long )
extern _function_f_stdcall__byval_a_as_long___as_sub__byval_b_as_long as long
declare function f stdcall(byval a as long) as sub(byval b as long)

extern separator4 as long

''      function f stdcall( byval a as long ) as sub( byval b as long )
extern _function_f_stdcall__byval_a_as_long___as_sub__byval_b_as_long as long
declare function f stdcall(byval a as long) as sub(byval b as long)

extern separator5 as long

''      function f stdcall( byval a as long ) as sub( byval b as long )
extern _function_f_stdcall__byval_a_as_long___as_sub__byval_b_as_long as long
declare function f stdcall(byval a as long) as sub(byval b as long)

extern separator6 as long

''      function f stdcall( byval a as long ) as function( byval b as long ) as sub( byval c as long )
extern _function_f_stdcall__byval_a_as_long___as_function__byval_b_as_long___as_sub__byval_c_as_long as long
declare function f stdcall(byval a as long) as function(byval b as long) as sub(byval c as long)

extern separator7 as long

''      function f stdcall( byval a as long ) as function( byval b as long ) as sub( byval c as long )
extern _function_f_stdcall__byval_a_as_long___as_function__byval_b_as_long___as_sub__byval_c_as_long as long
declare function f stdcall(byval a as long) as function(byval b as long) as sub(byval c as long)

extern separator8 as long

''      function f stdcall( byval a as long ) as function( byval b as long ) as sub( byval c as long )
extern _function_f_stdcall__byval_a_as_long___as_function__byval_b_as_long___as_sub__byval_c_as_long as long
declare function f stdcall(byval a as long) as function(byval b as long) as sub(byval c as long)

extern separator9 as long

''      function f( byval a as long ) as function( byval b as long ) as sub stdcall( byval c as long )
extern _function_f__byval_a_as_long___as_function__byval_b_as_long___as_sub_stdcall__byval_c_as_long as long
declare function f(byval a as long) as function(byval b as long) as sub stdcall(byval c as long)

extern separator10 as long

''      function f( byval a as long ) as function stdcall( byval b as long ) as sub( byval c as long )
extern _function_f__byval_a_as_long___as_function_stdcall__byval_b_as_long___as_sub__byval_c_as_long as long
declare function f(byval a as long) as function stdcall(byval b as long) as sub(byval c as long)

extern separator11 as long

''     p as function stdcall( byval a as long ) as sub( byval b as long )
extern p_as_function_stdcall__byval_a_as_long___as_sub__byval_b_as_long as long
extern p as function stdcall(byval a as long) as sub(byval b as long)
extern p as function stdcall(byval a as long) as sub(byval b as long)
extern p as function stdcall(byval a as long) as sub(byval b as long)

extern separator12 as long

''     p as function( byval a as long ) as sub stdcall( byval b as long )
extern p_as_function__byval_a_as_long___as_sub_stdcall__byval_b_as_long as long
extern p as function(byval a as long) as sub stdcall(byval b as long)

extern separator13 as long

''      sub f stdcall( )
extern _sub_f_stdcall as long
declare sub f stdcall()
declare sub f stdcall()
declare sub f stdcall()

extern separator14 as long

''      function f stdcall( ) as sub( )
extern _function_f_stdcall____as_sub as long
declare function f stdcall() as sub()
declare function f stdcall() as sub()
declare function f stdcall() as sub()

extern separator15 as long

''     p as sub stdcall( )
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

extern     __a_normal_variable as long
dim shared __a_normal_variable as long

dim shared __a_static_variable as long

dim shared __extern_variables as long
extern a as long
extern a as long ptr
extern a as long ptr ptr ptr ptr
extern b as long
extern c as long ptr
extern d as long
extern e as long

dim shared __nested_id as long
extern a as long
extern a as long
extern a as long
extern a as long ptr
extern a as long ptr ptr ptr ptr
extern b as long
extern c as long ptr
extern d as long
extern e as long

dim shared __nested_declarator as long
extern a as long ptr
extern a as long ptr ptr ptr ptr
extern c as long ptr

dim shared __arrays as long
dim shared a(0 to 1 - 1) as long
dim shared a(0 to 10 - 1) as long
dim shared a(0 to 2 - 1, 0 to 2 - 1) as long
dim shared a(0 to 2 - 1, 0 to 2 - 1, 0 to 2 - 1) as long
dim shared a(0 to 2 - 1, 0 to 3 - 1, 0 to 4 - 1, 0 to 5 - 1, 0 to 6 - 1) as long
dim shared p(0 to 4 - 1) as sub( )
dim shared p(0 to 2 - 1, 0 to 3 - 1) as sub( )
extern     a(0 to 10 - 1) as long
dim shared a(0 to 10 - 1) as long
extern a(0 to 10 - 1) as long
dim shared a(0 to 41 - 1) as long

dim shared __various_procptr_vars as long
extern a as sub( )
extern a as function( byval as long ) as long
extern a as function( byval a as long ) as long
extern b as function( byval a as long ) as long
extern c as long
extern a as long
extern b as function( byval a as long ) as long
extern c as long
extern d as function( byval a as long ) as long
extern a as function( byval as long ) as long ptr ptr
extern a as sub( byval a as sub( ) )
extern p as function( byval as function( byval as long ptr ptr ptr ) as long ptr ptr ptr ) as long ptr ptr ptr

dim shared __procptr_nested_declarator as long
extern p as sub( )
extern p as sub( )
extern p as sub( )
extern p as sub( )
extern p as sub( )
extern p as sub( )
extern p as sub( )
extern p as sub( )
extern p as sub( )
extern p as sub( )
extern p as sub( )
extern p as sub( )

dim shared __procptr_nested_declarator_with_consts as long
extern p as const sub( )
extern p as const sub( )
extern p as const sub( )
extern p as const sub( )
extern p as const sub( )
extern p as const sub( )
extern p as const sub( )
extern p as const sub( )
extern p as const sub( )
extern p as const sub( )
extern p as const sub( )
extern p as const sub( )

dim shared __procptrptr_nested_declarator as long
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr
extern p as typeof( sub( ) ) ptr

dim shared __procptrptr_nested_declarator_consts_1 as long
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr
extern p as typeof( sub( ) ) const ptr

dim shared __procptrptr_nested_declarator_consts_2 as long
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr
extern p as typeof( const sub( ) ) ptr

dim shared __procptrptr_nested_declarator_consts_3 as long
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr
extern p as typeof( const sub( ) ) const ptr

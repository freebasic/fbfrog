/'*
 * Variables
 '/

dim shared as integer a
dim shared as integer ptr p
dim shared as integer a, b
dim shared as integer ptr p : dim shared as integer ptr ptr ptr p : dim shared as integer x

/' Sub pointer '/
dim shared as sub() p

/' Function pointer, with anonymous function pointer param '/
dim shared as function(byval as function (byval as byte ptr ptr ptr ) as integer ptr ptr ptr) as double ptr ptr ptr p

dim shared as integer a
extern as integer a

/' Complex toplevel decl -- vardecl, procptr vardecl, procdecl '/
dim shared as integer a : dim shared as function() as integer ptr a : declare function a() as integer : declare function a() as integer ptr

/'*
 * Typedefs
 '/

type as T A
type as function(byval i as integer) as T A
type as T ptr ptr A : type as T B : type as function(byval as integer) as T C
type as E ptr PE
type as U /'boo'/ ptr ptr ptr ptr A


/'*
 * Procedures
 '/

declare function f() as integer
declare function f() as integer

/' Function as any ptr '/
declare function f() as any ptr

/' taking an int (but the id is omitted), returning an int '/
declare function f(byval as integer) as integer

/' some more params, and even ellipsis '/
declare function f(byval x as integer, byval as short, byval as byte ptr , ...) as T ptr

#define MY_EMPTY
#define MY_EXPORT __declspec(dllexport)
#define MY_CALL __attribute__((__stdcall__))/'__stdcall'/

/' some #defines in front, as is pretty common '/
declare function f() as T
declare function f MY_CALL () as T
'' TODO: unknown construct
MY_EMPTY MY_CALL T f(void);
dim shared as T a

/' Wrapped '/
declare function f(byval a as integer, byval b as integer, _
      byval c as integer, byval d as integer) as integer

/' Taking a procptr param '/
declare sub f(byval foo as sub())

/' Sub '/
declare sub test1()
declare sub test2(byval a as integer, byval b as single ptr, byval c as TT, byval d as T ptr ptr)

declare function f() as integer
declare function f() as integer

/'*
 * Fields
 '/

type T
	as integer i
	as ulongint j
	as uinteger k
	as double a,b,c

	as integer ptr ptr a : as integer ptr a : as integer a
	as integer a : as integer ptr ptr a, a
	as integer ptr a : as integer a, a : as integer ptr ptr a, a : as integer a

	as T ptr ptr ptr ptr y
	as T ptr a : as T ptr ptr ptr ptr a : as T a

	as function(byval as integer ptr ) as integer ptr p
	declare function f(byval as integer, byval as integer) as integer
	declare sub proc()

	as integer a
#if 1
	as integer b
#endif
	as integer c
end type

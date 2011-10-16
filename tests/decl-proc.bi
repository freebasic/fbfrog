declare function f() as integer

declare function f() as integer

/' function as any ptr '/
declare function f() as any ptr

/' taking an int (but the id is omitted), returning an int '/
declare function f(byval as integer) as integer

/' some more params, and even ellipsis '/
declare function f(byval x as integer, byval as short, byval as byte ptr , ...) as T ptr

#define MY_EXTERN /'__declspec(dllexport)'/
#define MY_CALL __attribute__((__stdcall__))/'__stdcall'/

/' some #defines in front, as is pretty common '/
/'TODO: unknown construct'/
MY_EXTERN MY_CALL TT1 f7(void);

/' wrapped '/
declare function f(byval a as integer, byval b as integer, _
      byval c as integer, byval d as integer) as integer

/' taking a procptr param '/
declare sub f(byval foo as sub())

/' sub '/
declare sub test1()

declare sub test2(byval a as integer, byval b as single ptr, byval c as TT, byval d as T ptr ptr)

declare function f() as integer

declare function f() as integer

declare function f1() as integer

/' function as any ptr '/
declare function f2() as any ptr

/' taking an int (but the id is omitted), returning an int '/
declare function f3(byval as integer) as integer

/' some more params, and even ellipsis '/
declare function f4(byval x as integer, byval as short, byval as byte ptr, ...) as integer ptr

/' typedef '/
declare function f5() as TT1 ptr

/' struct '/
declare function f6(byval as T1 ptr, byval as TT1 ptr ptr ptr ptr ptr ptr) as T1

#define MY_EXTERN /'__declspec(dllexport)'/
#define MY_CALL __attribute__((__stdcall__))/'__stdcall'/

/' some #defines in front, as is pretty common '/
MY_EXTERN MY_CALL declare function f7() as TT1

/' wrapped '/
declare function f8(byval a as integer, byval b as integer, _
       byval c as integer, byval d as integer) as integer

declare function f1() as integer

/' function as any ptr '/
declare function f2() as any ptr

/' taking an int, returning an int '/
/' TODO: token 93 '/ f3(/' TODO: token 93 '/)/' TODO: token 44 '/

/' some more params, and even ellipsis '/
declare function f4(byval x as integer, byval y as short, byval z as byte ptr, ...) as integer ptr

/' typedef '/
declare function f5() as TT1 ptr

/' struct '/
/' TODO: token 103 '/ T1 f6(/' TODO: token 103 '/ T1 *, TT1 ******)/' TODO: token 44 '/

#define MY_EXTERN /'__declspec(dllexport)'/
#define MY_CALL __attribute__((__stdcall__))/'__stdcall'/

/' some #defines in front, as is pretty common '/
MY_EXTERN MY_CALL declare function f7() as TT1

/' wrapped '/
declare function f8(byval a as integer, byval b as integer, _
       byval c as integer, byval d as integer) as integer

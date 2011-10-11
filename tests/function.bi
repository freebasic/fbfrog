declare function f1() as integer

/' function as any ptr '/
/' TODO: token 109 '/ *f2()/' TODO: token 44 '/

/' taking an int, returning an int '/
/' TODO: token 93 '/ f3(/' TODO: token 93 '/)/' TODO: token 44 '/

/' some more params, and even ellipsis '/
/' TODO: token 93 '/ *f4(/' TODO: token 93 '/ x, short y, /' TODO: token 72 '/ *z, ...)/' TODO: token 44 '/

/' typedef '/
TT1 *f5()/' TODO: token 44 '/

/' struct '/
/' TODO: token 103 '/ T1 f6(/' TODO: token 103 '/ T1 *, TT1 ******)/' TODO: token 44 '/

#define MY_EXTERN /'__declspec(dllexport)'/
#define MY_CALL __attribute__((__stdcall__))/'__stdcall'/

/' some #defines in front, as is pretty common '/
MY_EXTERN MY_CALL declare function f7() as TT1

/' wrapped '/
declare function f8(byval a as integer, byval b as integer, _
       byval c as integer, byval d as integer) as integer

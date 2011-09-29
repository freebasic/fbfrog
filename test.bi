#ifndef TEST_H
#define TEST_H

#ifdef __cplusplus
extern _
"C"              '' oh look, this is on a new line!
#endif

'' type T2 as T2_
'' (this way we only need to translate the <struct T2 ...> body as
'' <type T2_ ...>, that's easier than inserting T2_ fwdref in place of T2
'' before the T2 body)
/' TODO: token 103 '/ T2/' TODO: token 44 '/

'' type T2 : ... : end type
'' type TT2 as T2
'' (Both ids might be needed)
type T2 
	as integer a
	as double x
end type : type TT2 as T2

'' type TT3 : ... : end type
type TT3
	as integer a
	as double x
end type

'' type T1 : ... : end type
'' (also, any places using <struct T1> will become just <T1>, so they work ok)
type T1 
	as integer i
	as ulongint j
	as uinteger k
	as double a,b,c
	as T2 ptr x : as T2 ptr ptr ptr ptr y : as T2 z
	as integer ptr aa : as integer bb : as integer cc, dd : as integer ptr ptr ee : as integer ff
	/' TODO: token 93 '/ *(*p)(/' TODO: token 93 '/*)/' TODO: token 44 '/
	as integer a, b, c
end type

'' type TT1 as T1
'' (typedef needed, since it's a different id)
type TT1 as T1

'' type typedef_T1 as T1
'' (typedef not needed, since any places using T1 will work anyways)
type T1 as T1

type T3 : as integer a, b, c : end type

enum 
	A = &o               /' This is A '/
	B, C = (1 shl 4)     /' This is B and C '/
	D                    /' This is D '/
end enum

/' sub '/
/' TODO: token 109 '/ f01()/' TODO: token 44 '/

/' function as any ptr '/
/' TODO: token 109 '/ *f02()/' TODO: token 44 '/

/' taking an int, returning an int '/
/' TODO: token 93 '/ f03(/' TODO: token 93 '/)/' TODO: token 44 '/

/' some more params, and even ellipsis '/
/' TODO: token 93 '/ *f04(/' TODO: token 93 '/ x, short y, /' TODO: token 72 '/ *z, ...)/' TODO: token 44 '/

/' typedef '/
TT1 *f05()/' TODO: token 44 '/

/' struct '/
/' TODO: token 103 '/ T1 f06(/' TODO: token 103 '/ T1 *, TT1 ******)/' TODO: token 44 '/

#define MY_EXTERN /'__declspec(dllexport)'/
#define MY_CALL __attribute__((__stdcall__))/'__stdcall'/

/' some #defines in front, as is pretty common '/
MY_EXTERN MY_CALL TT1 f07()/' TODO: token 44 '/

/' wrapped '/
/' TODO: token 93 '/ f08(/' TODO: token 93 '/ a, /' TODO: token 93 '/ b,
        /' TODO: token 93 '/ c, /' TODO: token 93 '/ d)/' TODO: token 44 '/

/' sub ptr '/
/' TODO: token 109 '/ (*fp01)()/' TODO: token 44 '/

/' function pointer, with function pointer param '/
double *(*fpo02)(/' TODO: token 93 '/ ***(*)(/' TODO: token 72 '/**))/' TODO: token 44 '/

/' PP expressions '/
#if (/' TODO: token 18 '/defined(FOO_BAR) andalso THIS_IS_INSANE >= 123) _
    orelse (OH_MAN_WHATS_THE_PRECEDENCE < 5 andalso (defined(OK) _
                                            orelse defined(I_DONT_KNOW)))
	#define PPMERGE(a, b) a##b
	#define PPSTRINGIZE(a) #a
	/' TODO: token 105 '/ /' TODO: token 108 '/ __int8 uint8_t/' TODO: token 44 '/
	/' TODO: token 105 '/ /' TODO: token 108 '/ __int32 uint32_t/' TODO: token 44 '/
	/' TODO: token 105 '/ /' TODO: token 108 '/ __int64 uint64_t/' TODO: token 44 '/

#	if X = 4294967295UL orelse X = &o.1e+1
#		define HOORAY
#	endif
#endif

#ifdef __cplusplus
end extern
#endif

#endif

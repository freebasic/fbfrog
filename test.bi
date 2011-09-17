#ifndef TEST_H
#define TEST_H

#ifdef  __cplusplus
extern "C" 
#endif

type T 
	/' TODO: token 92 '/ i: /' TODO: token 92 '/ j
end type

/' TODO: token 104 '/ /' TODO: token 102 '/ T TT

enum 
	A,      /' This is A '/
	B,      /' This is B '/
	C       /' This is C '/
end enum

/' sub '/
/' TODO: token 108 '/ f01()

/' function as any ptr '/
/' TODO: token 108 '/ *f02()

/' taking an int, returning an int '/
/' TODO: token 92 '/ f03(/' TODO: token 92 '/)

/' some more params, and even ellipsis '/
/' TODO: token 92 '/ *f04(/' TODO: token 92 '/ x, short y, /' TODO: token 71 '/ *z, ...)

/' typedef '/
TT *f05()

/' struct '/
/' TODO: token 102 '/ T f06(/' TODO: token 102 '/ T *, TT ******)

/' some #defines in front, as is pretty common '/
MY_EXTERN MY_CALL TT f07()

/' wrapped '/
/' TODO: token 92 '/ f08(/' TODO: token 92 '/ a, /' TODO: token 92 '/ b,
        /' TODO: token 92 '/ c, /' TODO: token 92 '/ d)

/' sub ptr '/
/' TODO: token 108 '/ (*fp01)()

/' function pointer, with function pointer param '/
double *(*fpo02)(/' TODO: token 92 '/ ***(*)(/' TODO: token 71 '/**))

/' PP expressions '/
#if (/' TODO: token 18 '/defined(FOO_BAR) andalso THIS_IS_INSANE >= 123) \    orelse (OH_MAN_WHATS_THE_PRECEDENCE < 5 andalso (defined(OK) \                                            orelse defined(I_DONT_KNOW)))
	#define PPMERGE(a, b) a##b
	#define PPSTRINGIZE(a) #a
	/' TODO: token 104 '/ /' TODO: token 107 '/ __int8 uint8_t
	/' TODO: token 104 '/ /' TODO: token 107 '/ __int32 uint32_t
	/' TODO: token 104 '/ /' TODO: token 107 '/ __int64 uint64_t

#	if X = 4294967295UL orelse X = &o.1e+1
#		define HOORAY
#	endif
#endif

#ifdef  __cplusplus
end extern
#endif

#endif

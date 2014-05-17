// @fbfrog -removedefine CALLCONV -version 1 -define A -version 2

#ifdef A
	#define CALLCONV __attribute__((stdcall))
#else
	#define CALLCONV __attribute__((cdecl))
#endif

CALLCONV void f1(void);
CALLCONV void f2(void);

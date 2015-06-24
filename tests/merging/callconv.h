// @fbfrog -removedefine CALLCONV -declareversions VER 1 2 -selectversion -case 1 -define A -endselect

#ifdef A
	#define CALLCONV __attribute__((stdcall))
#else
	#define CALLCONV __attribute__((cdecl))
#endif

CALLCONV void f1(void);
CALLCONV void f2(void);

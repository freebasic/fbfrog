// @fbfrog <dir>callconv-3.fbfrog

#ifdef __x86_64__
	#define CALLCONV
#else
	#define CALLCONV __attribute__((stdcall))
#endif
typedef void (*T1) (void);
CALLCONV void f1(void);
CALLCONV void f2(void);

// @fbfrog -removedefine CALLCONV

void f1(void);

#ifdef _WIN32
	#define CALLCONV __attribute__((stdcall))
	__attribute__((cdecl)) void f2(void);
#else
	#define CALLCONV
	void f2(void);
#endif

CALLCONV void f3(void);
CALLCONV void f4(void);
CALLCONV void f5(void);

#pragma once

#if defined(UNICODE) and (defined(__FB_DOS__) or (defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_WIN32__) or defined(__FB_CYGWIN__)))
	#define __MINGW_NAME_AW(s) s##W
	#define __MINGW_NAME_AW_EXT(func, ext) func##W##ext
	#define __MINGW_NAME_UAW(func) func##_W
	#define __MINGW_NAME_UAW_EXT(func, ext) func##_W_##ext
	#define __TEXT(s) L##s
#else
	#define __MINGW_NAME_AW(s) s##A
	#define __MINGW_NAME_AW_EXT(func, ext) func##A##ext
	#define __MINGW_NAME_UAW(func) func##_A
	#define __MINGW_NAME_UAW_EXT(func, ext) func##_A_##ext
	#define __TEXT(s) s
#endif

#define WINELIB_NAME_AW __MINGW_NAME_AW
#define TEXT(s) __TEXT(s)
'' TODO: #define __MINGW_TYPEDEF_AW(type) typedef __MINGW_NAME_AW(type) type;
'' TODO: #define __MINGW_TYPEDEF_UAW(type) typedef __MINGW_NAME_UAW(type) type;

#if defined(UNICODE) and (defined(__FB_DOS__) or (defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_WIN32__) or defined(__FB_CYGWIN__)))
	#define CreateWindowEx CreateWindowExW
	#define SendMessage SendMessageW
	#define Function1 Function1W123
	#define Function2 Function2_W
	#define Function3 Function3_W_123
	#define Function4 Function4W
#else
	#define CreateWindowEx CreateWindowExA
	#define SendMessage SendMessageA
	#define Function1 Function1A123
	#define Function2 Function2_A
	#define Function3 Function3_A_123
	#define Function4 Function4A
#endif

const CONST1A = 1
const CONST1W = 2

#if defined(UNICODE) and (defined(__FB_DOS__) or (defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_WIN32__) or defined(__FB_CYGWIN__)))
	#define CONST1 CONST1W
#else
	#define CONST1 CONST1A
#endif

#define STR1 __TEXT("1")
#define STR2 __TEXT("2")

#if defined(UNICODE) and (defined(__FB_DOS__) or (defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_WIN32__) or defined(__FB_CYGWIN__)))
	type myType as myTypeW
	#define LookupAccountName LookupAccountNameW
	#define LookupAccountNameLocal(n, s, cs, d, cd, u) LookupAccountNameW(NULL, n, s, cs, d, cd, u)
#else
	type myType as myTypeA
	#define LookupAccountName LookupAccountNameA
	#define LookupAccountNameLocal(n, s, cs, d, cd, u) LookupAccountNameA(NULL, n, s, cs, d, cd, u)
#endif

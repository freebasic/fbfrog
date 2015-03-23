// @fbfrog <dir>fixmingwaw.fbfrog

#ifdef UNICODE
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
#define __MINGW_TYPEDEF_AW(type) typedef __MINGW_NAME_AW(type) type;
#define __MINGW_TYPEDEF_UAW(type) typedef __MINGW_NAME_UAW(type) type;

#define CreateWindowEx __MINGW_NAME_AW(CreateWindowEx)
#define SendMessage __MINGW_NAME_AW(SendMessage)
#define Function1 __MINGW_NAME_AW_EXT(Function1, 123)
#define Function2 __MINGW_NAME_UAW(Function2)
#define Function3 __MINGW_NAME_UAW_EXT(Function3, 123)
#define Function4 WINELIB_NAME_AW(Function4)

#define CONST1A 1
#define CONST1W 2
#define CONST1 __MINGW_NAME_AW(CONST1)

#define STR1 TEXT("1")
#define STR2 TEXT("2")

__MINGW_TYPEDEF_AW(myType);

#define LookupAccountName __MINGW_NAME_AW(LookupAccountName)
#define LookupAccountNameLocal(n, s, cs, d, cd, u) __MINGW_NAME_AW(LookupAccountName) (NULL, n, s, cs, d, cd, u)

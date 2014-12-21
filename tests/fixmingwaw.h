// @fbfrog -fixmingwaw -declarebool UNICODE -ifdef UNICODE -define UNICODE 1 -endif

#ifdef UNICODE
	#define __MINGW_NAME_AW(s) s##W
#else
	#define __MINGW_NAME_AW(s) s##A
#endif

#define CreateWindowEx __MINGW_NAME_AW(CreateWindowEx)
#define SendMessage __MINGW_NAME_AW(SendMessage)

#define CONST1A 1
#define CONST1W 2
#define CONST1 __MINGW_NAME_AW(CONST1)

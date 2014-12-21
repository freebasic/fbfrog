#pragma once

#ifdef UNICODE
	#define __MINGW_NAME_AW(s) s##W
	#define CreateWindowEx CreateWindowExW
	#define SendMessage SendMessageW
#else
	#define __MINGW_NAME_AW(s) s##A
	#define CreateWindowEx CreateWindowExA
	#define SendMessage SendMessageA
#endif

#define CONST1A 1
#define CONST1W 2

#ifdef UNICODE
	#define CONST1 CONST1W
#else
	#define CONST1 CONST1A
#endif

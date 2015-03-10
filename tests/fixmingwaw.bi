#pragma once

#ifdef UNICODE
	#define __MINGW_NAME_AW(s) s##W
	#define __MINGW_NAME_AW_EXT(func, ext) func##W##ext
	#define __MINGW_NAME_UAW(func) func##_W
	#define __MINGW_NAME_UAW_EXT(func, ext) func##_W_##ext
#else
	#define __MINGW_NAME_AW(s) s##A
	#define __MINGW_NAME_AW_EXT(func, ext) func##A##ext
	#define __MINGW_NAME_UAW(func) func##_A
	#define __MINGW_NAME_UAW_EXT(func, ext) func##_A_##ext
#endif

#define WINELIB_NAME_AW __MINGW_NAME_AW

#ifdef UNICODE
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

#ifdef UNICODE
	#define CONST1 CONST1W
#else
	#define CONST1 CONST1A
#endif

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

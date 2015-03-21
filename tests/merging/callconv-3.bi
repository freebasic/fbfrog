#pragma once

#ifdef __FB_64BIT__
	extern "C"

	#define CALLCONV
#else
	extern "Windows"
#endif

type T1 as sub cdecl()
declare sub f1()
declare sub f2()

end extern

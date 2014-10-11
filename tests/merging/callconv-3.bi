#pragma once

#ifdef __FB_64BIT__
	extern "C"

	#define CALLCONV

	type T1 as sub()
#else
	extern "Windows"

	type T1 as sub cdecl()
#endif

declare sub f1()
declare sub f2()

end extern

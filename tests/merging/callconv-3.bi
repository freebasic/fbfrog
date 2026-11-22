#pragma once

#ifdef __FB_64BIT__
	extern "C"
#else
	extern "Windows"
#endif

' @fbfrog -target windows -declarebool UNICODE -ifdef UNICODE -define UNICODE -endif

#ifdef __FB_64BIT__
	#define CALLCONV
#endif

type T1 as sub cdecl()
declare sub f1()
declare sub f2()

end extern

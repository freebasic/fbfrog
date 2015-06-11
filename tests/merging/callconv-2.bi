#pragma once

#ifdef __FB_WIN32__
	extern "Windows"
#else
	extern "C"
#endif

declare sub f1 cdecl()
declare sub f2 cdecl()
declare sub f3()
declare sub f4()
declare sub f5()

end extern

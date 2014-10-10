#pragma once

#ifdef __FB_WIN32__
	extern "Windows"

	declare sub f1 cdecl()
	declare sub f2 cdecl()
#else
	extern "C"

	declare sub f1()
	declare sub f2()
#endif

declare sub f3()
declare sub f4()
declare sub f5()

end extern

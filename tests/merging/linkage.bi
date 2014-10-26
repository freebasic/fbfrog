#pragma once

extern "C"

declare sub f0()

#ifdef __FB_WIN32__
	declare sub f1()
	declare sub f2()
#else
	declare sub f1()
	declare sub f2()
#endif

end extern

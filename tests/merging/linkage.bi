#pragma once

extern "C"

declare sub f0()
declare sub f1()

#ifdef __FB_WIN32__
	declare sub f2()
#else
	declare sub f2()
#endif

end extern

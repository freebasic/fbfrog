#pragma once

extern "C"

declare sub f0()
declare sub f1()

#ifdef __FB_WIN32__
	' can really happen, same as <void f1(void);>
#endif

declare sub f2()

#ifdef __FB_WIN32__
	' doesn't make much sense, but nevertheless, different from <void f2(void);>
#endif

end extern

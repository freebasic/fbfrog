#pragma once

extern "C"

declare sub f0()
declare sub f1()

#if defined(__FB_DOS__) or defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_CYGWIN__)
	declare sub f2()
#else
	declare sub f2()
#endif

end extern

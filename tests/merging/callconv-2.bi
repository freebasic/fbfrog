#pragma once

#if defined(__FB_DOS__) or defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_CYGWIN__)
	extern "C"
#else
	extern "Windows"
#endif

declare sub f1 cdecl()
declare sub f2 cdecl()
declare sub f3()
declare sub f4()
declare sub f5()

end extern

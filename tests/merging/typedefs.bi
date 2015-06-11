#pragma once

#if defined(__FB_DOS__) or defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_CYGWIN__)
	#include once "crt/long.bi"
#endif

extern "C"

#ifdef __FB_WIN32__
	type myint64 as longint
#else
	type myint64 as clong
#endif

extern i as myint64

end extern

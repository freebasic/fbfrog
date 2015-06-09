#pragma once

#if defined(__FB_DOS__) or (defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_CYGWIN__))
	#include once "crt/long.bi"
#endif

extern "C"

#if defined(__FB_DOS__) or (defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_CYGWIN__))
	type myint64 as clong
#else
	type myint64 as longint
#endif

extern i as myint64

end extern

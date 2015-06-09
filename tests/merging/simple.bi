#pragma once

extern "C"

extern a1 as long

#if (VER = 1) and (defined(__FB_DOS__) or (defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_WIN32__) or defined(__FB_CYGWIN__)))
	extern a2 as long
#endif

extern a3 as long

end extern

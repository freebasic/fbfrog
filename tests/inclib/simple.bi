#pragma once

#inclib "main"

#if (VER = 1) and (defined(__FB_DOS__) or (defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_WIN32__) or defined(__FB_CYGWIN__)))
	#inclib "cool1"
#else
	#inclib "cool-2.0"
#endif

extern "C"

declare sub f()

end extern

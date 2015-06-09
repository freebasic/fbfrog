#pragma once

#if defined(A) and (defined(__FB_DOS__) or (defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_WIN32__) or defined(__FB_CYGWIN__)))
	dim shared iA as long
#elseif defined(B) and (defined(__FB_DOS__) or (defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_WIN32__) or defined(__FB_CYGWIN__)))
	dim shared iB as long
#else
	dim shared iC as long
#endif

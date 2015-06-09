#pragma once

#if defined(__FB_64BIT__) and (defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_CYGWIN__))
	const MY_WORD_SIZE = 64
#else
	const MY_WORD_SIZE = 32
#endif

#pragma once

extern "C"

#if defined(__FB_64BIT__) and (defined(__FB_WIN32__) or defined(__FB_UNIX__))
	extern my_64bit as long
#else
	extern my_32bit as long
#endif

end extern

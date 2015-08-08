#pragma once

#ifdef __FB_WIN32__
	extern "C"

	#ifdef __FB_64BIT__
		extern win64 as long
	#else
		extern win32 as long
	#endif

	end extern
#endif

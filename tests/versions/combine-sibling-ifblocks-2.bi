#pragma once

#ifdef __FB_WIN32__
	extern "C"
#endif

#if defined(__FB_WIN32__) and (not defined(__FB_64BIT__))
	extern win32 as long
#elseif defined(__FB_WIN32__) and defined(__FB_64BIT__)
	extern win64 as long
#endif

#ifdef __FB_WIN32__
	end extern
#endif

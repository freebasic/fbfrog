#pragma once

#ifdef __FB_WIN32__
	extern "C"

	#ifndef __FB_64BIT__
		extern win32 as long
	#elseif defined(__FB_64BIT__)
		extern win64 as long
	#endif

	end extern
#endif

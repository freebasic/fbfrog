#pragma once

#ifdef __FB_LINUX__
	extern "C"

	extern linux1 as long
#endif

#if defined(__FB_LINUX__) and defined(__FB_64BIT__)
	extern linux64bit as long
#endif

#ifdef __FB_LINUX__
	extern linux2 as long

	end extern
#endif

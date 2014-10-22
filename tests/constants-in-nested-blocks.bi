#pragma once

#if (defined(__FB_LINUX__) and (not defined(__FB_64BIT__))) or defined(__FB_DOS__) or defined(__FB_WIN32__)
	#define MY_WORD_SIZE 32
#else
	#define MY_WORD_SIZE 64
#endif

#pragma once

#if defined(__FB_64BIT__) and defined(__FB_UNIX__)
	const MY_WORD_SIZE = 64
#else
	const MY_WORD_SIZE = 32
#endif

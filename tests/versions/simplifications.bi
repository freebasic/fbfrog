#pragma once

#if V = 1
	#define exists_only_in_V1
#else
	#define exists_only_in_V2
#endif

#define exists_in_all_versions

#if (defined(__FB_64BIT__) and defined(__FB_UNIX__)) or ((not defined(__FB_64BIT__)) and (defined(__FB_WIN32__) or defined(__FB_UNIX__))) or defined(__FB_DOS__)
	#define exists_not_for_win64
#endif

#pragma once

extern "C"

#if defined(__FB_DOS__) or defined(__FB_WIN32__)
	extern my_windos as long
#else
	extern my_unix as long
#endif

end extern

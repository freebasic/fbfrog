#pragma once

extern "C"

' @fbfrog -iftarget unix -define OS my_unix -else -define OS my_windos -endif

#ifdef __FB_UNIX__
	extern my_unix as long
#else
	extern my_windos as long
#endif

end extern

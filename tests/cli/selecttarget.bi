#pragma once

extern "C"

' @fbfrog -selecttarget -case windows -define TARGET ITS_WIN32 -endselect

#ifdef __FB_WIN32__
	extern ITS_WIN32 as long
#else
	extern TARGET as long
#endif

end extern

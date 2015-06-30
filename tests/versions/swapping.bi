#pragma once

extern "C"

#ifdef __FB_WIN32__
	declare sub win32_1()
#else
	declare sub rest_1()
#endif

extern separator as long

#ifdef __FB_WIN32__
	declare sub rest_2()
#else
	declare sub unixdos_2()
#endif

extern separator as long

#ifdef __FB_UNIX__
	declare sub rest_3()
#elseif defined(__FB_WIN32__)
	declare sub win32_3()
#else
	declare sub dos_3()
#endif

extern separator as long

#if defined(__FB_LINUX__) or defined(__FB_WIN32__) or defined(__FB_CYGWIN__)
	declare sub linux_win32_cygwin_4()
#elseif defined(__FB_DARWIN__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__)
	declare sub unix_4()
#else
	declare sub rest_4()
#endif

end extern

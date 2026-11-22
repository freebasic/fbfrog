#pragma once

extern "C"

' The #if block generation should be smart enough to prefer simple #if#
' expressions over complex ones. (in case of a #if/#else block there are
' two code paths, and we can choose which of them to test for in the #if)

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
' But not if there's #elseif's involved, because then the order of the blocks matters.
' (maybe not if only checking exclusively #defined symbols, but it can matter in other cases,
' so for now it's just safer not to do anything)

#ifdef __FB_UNIX__
	declare sub rest_3()
#elseif defined(__FB_WIN32__)
	declare sub win32_3()
#else
	declare sub dos_3()
#endif

extern separator as long
' E.g. here it matters, because we check for Linux before checking for Unix,
' but Linux defines __unix__ too. So here those blocks really mustn't be swapped.

#if defined(__FB_LINUX__) or defined(__FB_WIN32__) or defined(__FB_CYGWIN__)
	declare sub linux_win32_cygwin_4()
#elseif defined(__FB_DARWIN__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__)
	declare sub unix_4()
#else
	declare sub rest_4()
#endif

end extern

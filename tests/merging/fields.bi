#pragma once

type UDT1
	field1 as long
	field2 as long
end type

type UDT2
	field1 as long

	#if (VER = 1) and (defined(__FB_DOS__) or (defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_WIN32__) or defined(__FB_CYGWIN__)))
		field2 as long
	#else
		field3 as long
	#endif

	field4 as long
end type

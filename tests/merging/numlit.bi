#pragma once

const A1 = &o55

#if defined(__FB_DOS__) or defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_CYGWIN__)
	const A2 = 55
	const A3 = &h55
	const B1 = &o55
#else
	const A2 = &o55
	const A3 = &o55
	const B1 = 55
#endif

const B2 = 55

#if defined(__FB_DOS__) or defined(__FB_LINUX__) or defined(__FB_FREEBSD__) or defined(__FB_OPENBSD__) or defined(__FB_NETBSD__) or defined(__FB_DARWIN__) or defined(__FB_CYGWIN__)
	const B3 = &h55
	const C1 = &o55
	const C2 = 55
#else
	const B3 = 55
	const C1 = &h55
	const C2 = &h55
#endif

const C3 = &h55

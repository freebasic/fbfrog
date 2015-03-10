#pragma once

const A1 = &o55

#ifdef __FB_WIN32__
	const A2 = &o55
	const A3 = &o55
	const B1 = 55
#else
	const A2 = 55
	const A3 = &h55
	const B1 = &o55
#endif

const B2 = 55

#ifdef __FB_WIN32__
	const B3 = 55
	const C1 = &h55
	const C2 = &h55
#else
	const B3 = &h55
	const C1 = &o55
	const C2 = 55
#endif

const C3 = &h55

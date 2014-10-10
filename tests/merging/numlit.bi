#pragma once

#define A1 &o55

#ifdef __FB_WIN32__
	#define A2 &o55
	#define A3 &o55
	#define B1 55
#else
	#define A2 55
	#define A3 &h55
	#define B1 &o55
#endif

#define B2 55

#ifdef __FB_WIN32__
	#define B3 55
	#define C1 &h55
	#define C2 &h55
#else
	#define B3 &h55
	#define C1 &o55
	#define C2 55
#endif

#define C3 &h55

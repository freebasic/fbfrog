#ifdef __FB_DOS__
	#ifndef VER
		#define VER 3
	#endif

	#if VER = 1
	#elseif VER = 2
	#elseif VER = 3
	#else
		#error "'VER' is #defined to an unsupported value; expected one of: 1, 2, 3"
	#endif
#else
	#ifndef VER
		#define VER 5
	#endif

	#if VER = 1
	#elseif VER = 2
	#elseif VER = 3
	#elseif VER = 4
	#elseif VER = 5
	#else
		#error "'VER' is #defined to an unsupported value; expected one of: 1, 2, 3, 4, 5"
	#endif
#endif

#pragma once

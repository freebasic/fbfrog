#pragma once

#ifdef A
#else
	#ifndef VER
		#define VER 2
	#endif

	#if VER = 1
	#elseif VER = 2
	#else
		#error "'VER' is #defined to an unsupported value; expected one of: 1, 2"
	#endif
#endif

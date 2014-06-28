#pragma once

#ifdef A
#elseif defined(B)
#elseif defined(C)
#else
	#error "Not one of these symbols is #defined: A, B, C"
#endif

#ifdef A
#elseif defined(B)
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

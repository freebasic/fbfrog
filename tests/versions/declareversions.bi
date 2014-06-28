#pragma once

#ifndef VER
	#define VER 3
#endif

#if VER = 1
#elseif VER = 2
#elseif VER = 3
#else
	#error "'VER' is #defined to an unsupported value; expected one of: 1, 2, 3"
#endif

#if VER = 1
	dim shared i1 as long
#elseif VER = 2
	dim shared i2 as long
#else
	dim shared i3 as long
#endif

#pragma once

#ifndef VER
	#define VER 2
#endif

#if VER = 1
#elseif VER = 2
#else
	#error "'VER' is #defined to an unsupported value; expected one of: 1, 2"
#endif

extern "C"

extern a1 as long

#if VER = 1
	extern a2 as long
#endif

extern a3 as long

end extern

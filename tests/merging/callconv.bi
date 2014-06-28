#ifndef VER
	#define VER 2
#endif

#if VER = 1
#elseif VER = 2
#else
	#error "'VER' is #defined to an unsupported value; expected one of: 1, 2"
#endif

#pragma once

#if VER = 1
	extern "Windows"
#else
	extern "C"
#endif

declare sub f1()
declare sub f2()

end extern

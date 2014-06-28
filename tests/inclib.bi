#pragma once

#ifndef VER
	#define VER 2
#endif

#if VER = 1
#elseif VER = 2
#else
	#error "'VER' is #defined to an unsupported value; expected one of: 1, 2"
#endif

#inclib "main"

#if VER = 1
	#inclib "cool1"
#else
	#inclib "cool-2.0"
#endif

extern "C"

declare sub f()

end extern

#pragma once

#ifndef VER
	#define VER 2
#endif

#if VER = 1
#elseif VER = 2
#else
	#error "'VER' is #defined to an unsupported value; expected one of: 1, 2"
#endif

type __UDT_a
	#if VER = 1
		v1_a as long
	#else
		v2_a as long
	#endif
end type

#if VER = 1
	type __UDT_b
		v1_b as long
	end type
#endif

type UDT
	a as __UDT_a

	#if VER = 1
		b as __UDT_b
	#endif
end type

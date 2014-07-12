#pragma once

#ifndef VER
	#define VER 2
#endif

#if VER = 1
#elseif VER = 2
#else
	#error "'VER' is #defined to an unsupported value; expected one of: 1, 2"
#endif

#if VER = 1
	type __freebasic_dummyid_0
		v1_a as long
	end type
#endif

type __freebasic_dummyid_1
	#if VER = 1
		v1_b as long
	#else
		v2_a as long
	#endif
end type

type UDT
	a as __freebasic_dummyid_0

	#if VER = 1
		b as __freebasic_dummyid_1
	#endif
end type

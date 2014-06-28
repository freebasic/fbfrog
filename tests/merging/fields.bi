#ifndef VER
	#define VER 2
#endif

#if VER = 1
#elseif VER = 2
#else
	#error "'VER' is #defined to an unsupported value; expected one of: 1, 2"
#endif

#pragma once

type UDT1
	field1 as long
	field2 as long
end type

type UDT2
	field1 as long

	#if VER = 1
		field2 as long
	#else
		field3 as long
	#endif

	field4 as long
end type

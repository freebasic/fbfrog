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

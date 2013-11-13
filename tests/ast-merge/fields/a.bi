#ifndef __TEST_VERSION__
	#define __TEST_VERSION__ 1
#endif

#if (__TEST_VERSION__ <> 1) and (__TEST_VERSION__ <> 2)
	#error "'__TEST_VERSION__' is #defined to an unsupported value; expected one of: 1, 2"
#endif

type UDT
	field1 as long
	field2 as long
end type

type UDT
	field1 as long

	#if __TEST_VERSION__ = 1
		field2 as long
	#else
		field3 as long
	#endif

	field4 as long
end type

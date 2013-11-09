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

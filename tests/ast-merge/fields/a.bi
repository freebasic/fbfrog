type UDT
	field1 as long
	field2 as long
end type
type UDT
	field1 as long
	#if ver = 1
		field2 as long
	#endif
	#if ver = 2
		field3 as long
	#endif
	field4 as long
end type

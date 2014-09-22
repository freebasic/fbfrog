#pragma once

extern "C"

type __dummyid_0_tests_c_structbody_basetype
	a as long
end type

declare function f1() as __dummyid_0_tests_c_structbody_basetype

type UDT2
	a as long
end type

declare function f2() as UDT2

type __dummyid_1_tests_c_structbody_basetype
	a as long
end type

declare function f3() as __dummyid_1_tests_c_structbody_basetype

type UDT4
	a as long
end type

declare function f4() as UDT4

type __dummyid_2_tests_c_structbody_basetype
	a as long
end type

dim shared a5 as __dummyid_2_tests_c_structbody_basetype
dim shared b5 as __dummyid_2_tests_c_structbody_basetype

type UDT6
	a as long
end type

dim shared a6 as UDT6
dim shared b6 as UDT6

union __dummyid_3_tests_c_structbody_basetype
	a as long
end union

declare function f7() as __dummyid_3_tests_c_structbody_basetype

union UDT8
	a as long
end union

declare function f8() as UDT8

type __dummyid_4_tests_c_structbody_basetype as long
enum
	A = 0
end enum

declare function f9() as __dummyid_4_tests_c_structbody_basetype

type UDT10 as long
enum
	B = 0
end enum

declare function f10() as UDT10

type __dummyid_5_tests_c_structbody_basetype
	a as long
end type

type UDT12
	a as long
end type

type __dummyid_6_tests_c_structbody_basetype
	a as long
end type

type UDT11
	field1 as __dummyid_5_tests_c_structbody_basetype
	field2 as UDT12
	a as const __dummyid_6_tests_c_structbody_basetype
	b as const __dummyid_6_tests_c_structbody_basetype
end type

end extern

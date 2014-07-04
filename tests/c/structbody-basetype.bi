#pragma once

extern "C"

type __freebasic_dummyid_0
	a as long
end type

declare function f1() as __freebasic_dummyid_0

type UDT2
	a as long
end type

declare function f2() as UDT2

type __freebasic_dummyid_1
	a as long
end type

declare function f3() as __freebasic_dummyid_1

type UDT4
	a as long
end type

declare function f4() as UDT4

type __freebasic_dummyid_2
	a as long
end type

dim shared a5 as __freebasic_dummyid_2
dim shared b5 as __freebasic_dummyid_2

type UDT6
	a as long
end type

dim shared a6 as UDT6
dim shared b6 as UDT6

union __freebasic_dummyid_3
	a as long
end union

declare function f7() as __freebasic_dummyid_3

union UDT8
	a as long
end union

declare function f8() as UDT8

type __freebasic_dummyid_4 as long
enum
	A = 0
end enum

declare function f9() as __freebasic_dummyid_4

type UDT10 as long
enum
	B = 0
end enum

declare function f10() as UDT10

type __freebasic_dummyid_5
	a as long
end type

type UDT12
	a as long
end type

type __freebasic_dummyid_6
	a as long
end type

type UDT11
	field1 as __freebasic_dummyid_5
	field2 as UDT12
	a as const __freebasic_dummyid_6
	b as const __freebasic_dummyid_6
end type

end extern

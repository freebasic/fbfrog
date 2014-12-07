#pragma once

extern "C"

type __f1
	a as long
end type

declare function f1() as __f1

type UDT2
	a as long
end type

declare function f2() as UDT2

type __f3
	a as long
end type

declare function f3() as __f3

type UDT4
	a as long
end type

declare function f4() as UDT4

type __a5
	a as long
end type

dim shared a5 as __a5
dim shared b5 as __a5

type UDT6
	a as long
end type

dim shared a6 as UDT6
dim shared b6 as UDT6

union __f7
	a as long
end union

declare function f7() as __f7

union UDT8
	a as long
end union

declare function f8() as UDT8

type __f9 as long
enum
	A = 0
end enum

declare function f9() as __f9

type UDT10 as long
enum
	B = 0
end enum

declare function f10() as UDT10

type __UDT11_field1
	a as long
end type

type UDT12
	a as long
end type

type __UDT11_0
	a as long
end type

type UDT11
	field1 as __UDT11_field1
	field2 as UDT12
	a as const __UDT11_0
	b as const __UDT11_0
end type

end extern

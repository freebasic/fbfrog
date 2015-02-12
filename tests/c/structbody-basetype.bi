#pragma once

extern "C"

type f1
	a as long
end type

declare function f1() as f1

type UDT2
	a as long
end type

declare function f2() as UDT2

type f3
	a as long
end type

declare function f3() as f3

type UDT4
	a as long
end type

declare function f4() as UDT4

type a5
	a as long
end type

dim shared a5 as a5
dim shared b5 as a5

type UDT6
	a as long
end type

dim shared a6 as UDT6
dim shared b6 as UDT6

union f7
	a as long
end union

declare function f7() as f7

union UDT8
	a as long
end union

declare function f8() as UDT8

type f9 as long
enum
	A = 0
end enum

declare function f9() as f9

type UDT10 as long
enum
	B = 0
end enum

declare function f10() as UDT10

type UDT11_field1
	a as long
end type

type UDT12
	a as long
end type

type UDT11_a
	a as long
end type

type UDT11
	field1 as UDT11_field1
	field2 as UDT12
	a as const UDT11_a
	b as const UDT11_a
end type

end extern

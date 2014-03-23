extern "C"

type __dummyid0
	a as long
end type

declare function f1() as __dummyid0

type UDT2
	a as long
end type

declare function f2() as UDT2

type __dummyid1
	a as long
end type

declare function f3() as __dummyid1

type UDT4
	a as long
end type

declare function f4() as UDT4

type __dummyid2
	a as long
end type

dim shared a5 as __dummyid2
dim shared b5 as __dummyid2

type UDT6
	a as long
end type

dim shared a6 as UDT6
dim shared b6 as UDT6

union __dummyid3
	a as long
end union

declare function f7() as __dummyid3

union UDT8
	a as long
end union

declare function f8() as UDT8

enum __dummyid4
	A = 0
end enum

declare function f9() as __dummyid4

enum UDT10
	B = 0
end enum

declare function f10() as UDT10

type __dummyid5
	a as long
end type

type UDT12
	a as long
end type

type __dummyid6
	a as long
end type

type UDT11
	field1 as __dummyid5
	field2 as UDT12
	a as const __dummyid6
	b as const __dummyid6
end type

end extern

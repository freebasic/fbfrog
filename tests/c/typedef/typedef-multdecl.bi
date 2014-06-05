#pragma once

extern "C"

type A1
	a as long
end type

type PA1 as A1 ptr

type UDT
	a as long
end type

type A2 as UDT
type B2 as UDT
type C2 as UDT ptr
type D2 as function() as UDT

end extern

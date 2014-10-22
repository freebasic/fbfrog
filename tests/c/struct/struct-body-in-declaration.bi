#pragma once

extern "C"

type UDT1
	a as long
end type

declare function f1() as UDT1

type UDT2_1
	a as long
end type

type UDT2
	a as UDT2_1
end type

declare function f2() as UDT2

end extern

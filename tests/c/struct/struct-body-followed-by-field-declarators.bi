#pragma once

extern "C"

type __UDT_a
	a as long
end type

type __UDT_b
	a as long
end type

type __UDT_0
	a as long
end type

type __UDT_1
	a as long
end type

type UDT
	a as __UDT_a
	b as __UDT_b
	c as __UDT_b
	d as __UDT_b
	e as __UDT_0 ptr
	f as function() as __UDT_1
end type

end extern

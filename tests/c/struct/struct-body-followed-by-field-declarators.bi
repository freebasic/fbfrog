#pragma once

extern "C"

type UDT_a
	a as long
end type

type UDT_b
	a as long
end type

type UDT__0
	a as long
end type

type UDT__1
	a as long
end type

type UDT
	a as UDT_a
	b as UDT_b
	c as UDT_b
	d as UDT_b
	e as UDT__0 ptr
	f as function() as UDT__1
end type

end extern

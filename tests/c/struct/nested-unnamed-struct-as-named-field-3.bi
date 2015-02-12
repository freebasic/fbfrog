#pragma once

type UDT_b_a
end type

type UDT_b
	union
		a as UDT_b_a
	end union
end type

type UDT
	union
		b as UDT_b
	end union
end type

#pragma once

type __UDT_a
end type

type __UDT_b
	union
		a as __UDT_a
	end union
end type

type UDT
	union
		b as __UDT_b
	end union
end type

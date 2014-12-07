#pragma once

type __UDT_a
	a as long
end type

type UDT
	union
		a as __UDT_a
	end union
end type

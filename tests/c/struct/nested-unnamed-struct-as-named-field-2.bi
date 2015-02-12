#pragma once

type UDT_a
	a as long
end type

type UDT
	union
		a as UDT_a
	end union
end type

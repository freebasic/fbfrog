#pragma once

union __UDT_a
	a as long
end union

type UDT
	union
		type
			a as __UDT_a
		end type
	end union
end type

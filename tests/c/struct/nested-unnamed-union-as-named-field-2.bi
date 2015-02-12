#pragma once

union UDT_a
	a as long
end union

type UDT
	union
		type
			a as UDT_a
		end type
	end union
end type

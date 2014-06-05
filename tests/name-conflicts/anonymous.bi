#pragma once

extern "C"

declare sub f(byval as long)

enum
	A
	B
end enum

type UDT1
	a as long

	union
		b as long

		type
			c as long
		end type
	end union
end type

union UDT2
	a as long

	type
		b as long

		union
			c as long
		end union
	end type
end union

end extern

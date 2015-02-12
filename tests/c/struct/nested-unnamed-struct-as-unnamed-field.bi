#pragma once

type UDT1
	union
		type
			a as long
		end type
	end union
end type

type UDT2
	union
		type field = 1
			a as long
			b as byte
		end type
	end union
end type

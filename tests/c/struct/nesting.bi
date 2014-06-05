#pragma once

union Nested
	a as long
	b as long

	type
		c as long

		union
			d as long
			e as long
		end union

		f as long
	end type

	g as long

	type
		h as long
	end type

	type
		union
			type
				union
				end union
			end type
		end union
	end type
end union

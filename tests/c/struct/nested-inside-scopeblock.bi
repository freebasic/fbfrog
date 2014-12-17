#pragma once

extern "C"

private sub scopeblock1()
	type __UDT_inner
		i as long
	end type
	type UDT
		inner as __UDT_inner
	end type
end sub

#macro scopeblock2
	scope
		type __scopeblock2_UDT_inner
			i as long
		end type
		type UDT
			inner as __scopeblock2_UDT_inner
		end type
	end scope
#endmacro

end extern

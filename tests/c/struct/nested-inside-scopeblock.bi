#pragma once

extern "C"

private sub scopeblock1()
	type UDT_inner
		i as long
	end type
	type UDT
		inner as UDT_inner
	end type
end sub

#macro scopeblock2
	scope
		type UDT_inner
			i as long
		end type
		type UDT
			inner as UDT_inner
		end type
	end scope
#endmacro

end extern

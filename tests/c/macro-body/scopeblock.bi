#pragma once

#macro M1
	scope
		if 1 then
		end if
	end scope
#endmacro
#macro M2
	scope
		if 1 then
		end if
	end scope
#endmacro
#macro M3
	scope
		do
		loop while 1
	end scope
#endmacro
#macro M4
	scope
		do
		loop while 1
	end scope
#endmacro
#macro M5
	scope
		while 1
		wend
	end scope
#endmacro
#macro M6
	scope
		while 1
		wend
	end scope
#endmacro

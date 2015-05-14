#pragma once

extern "C"

'' TODO: static int i1 = i1 = 1, 2;

private sub f()
	dim a as long
	dim b as long
	a = 1
	b = 2
end sub

if 1 then
	a = 1
	b = 2
end if

if 1 then
	a = 1
	b = 2
end if

#macro M1(x, y)
	scope
		(x) = 1
		(y) = 2
	end scope
#endmacro
#macro M2
	if 1 then
		a = 1
		b = 2
	end if
#endmacro
#macro M3
	if 1 then
		a = 1
		b = 2
	end if
#endmacro
#macro M4
	if 1 then
		a = 1
		b = 2
	end if
#endmacro
#macro M5
	if 1 then
		a = 1
		b = 2
	end if
#endmacro

end extern

#pragma once

extern "C"

'' TODO: static int i1 = 1, 2;

private function f(byval as long) as long
	f(1)
	f(2)
	dim a as long
	dim b as long
	a
	b
	'' TODO: return f(1), f(2);
end function

if 1 then
	f(1)
	f(2)
end if

if 1 then
	f(1)
	f(2)
end if

#macro M1
	scope
		1
		2
	end scope
#endmacro
#macro M2(x, y)
	scope
		f(x)
		f(y)
	end scope
#endmacro
'' TODO: #define M3 1, 2, 3
#macro M4
	if 1 then
		f(1)
		f(2)
	end if
#endmacro
#macro M5
	if 1 then
		f(1)
		f(2)
	end if
#endmacro
#macro M6
	if 1 then
		f(1)
		f(2)
	end if
#endmacro
#macro M7
	if 1 then
		f(1)
		f(2)
	end if
#endmacro

end extern

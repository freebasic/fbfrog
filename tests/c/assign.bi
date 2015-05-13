#pragma once

extern "C"

'' TODO: static int i1 = i1 = 0;

private sub f1()
	dim a as long
	a = 1
	'' TODO: a = a = 2;
end sub

if 1 then
	a = 1
end if

if 1 then
	a = 1
end if

#macro M1(x)
	scope
		x = 1
	end scope
#endmacro
#macro M2(x)
	scope
		(x) = 1
	end scope
#endmacro
'' TODO: #define M3(x) ((x) = (x) = 1)
#macro M4
	if 1 then
		a = 1
	end if
#endmacro
#macro M5
	if 1 then
		a = 1
	end if
#endmacro
#macro M6
	scope
		if 1 then
			a = 1
		end if
	end scope
#endmacro
#macro M7
	scope
		if 1 then
			a = 1
		end if
	end scope
#endmacro
scope
	a = 1
	a or= 1
	a xor= 1
	a and= 1
	a shl= 1
	a shr= 1
	a += 1
	a -= 1
	a *= 1
	a /= 1
	a mod= 1
end scope

end extern

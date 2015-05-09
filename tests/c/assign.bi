#pragma once

extern "C"

'' TODO: static int i1 = i1 = 0;

private sub f1()
	dim a as long
	a = 1
	'' TODO: a = a = 2;
end sub

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

end extern

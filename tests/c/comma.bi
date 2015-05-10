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

end extern

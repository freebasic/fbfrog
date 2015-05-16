#pragma once

const A1 = 1
#define A2() 1
#define A3(x) 1
#macro B1
	scope
		f(1)
		f(2)
	end scope
#endmacro
#macro B2()
	scope
		f(1)
		f(2)
	end scope
#endmacro
#macro B3(x)
	scope
		f(1)
		f(2)
	end scope
#endmacro
#define C1 scope : a = b : end scope
#define C2() scope : a = b : end scope
#define C3(x) scope : a = b : end scope

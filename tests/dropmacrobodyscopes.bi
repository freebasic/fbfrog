#pragma once

#define A ()
#define B
#define C f1(0)
#macro D
	scope
		f1(0)
		f2(0)
	end scope
#endmacro

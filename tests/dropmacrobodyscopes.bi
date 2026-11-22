#pragma once

' @fbfrog -dropmacrobodyscopes
#define A () ' this is currently treated as struct initializer
#define B ' empty scope block - should be removed
#define C f1(0) ' just 1 statement - should remove scope block here
#macro D
	scope
		f1(0)
		f2(0)
	end scope
#endmacro ' multiple statements - should keep scope block here

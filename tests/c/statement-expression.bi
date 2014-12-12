#pragma once

extern "C"

#define A1 '' TODO: ({ int i = foo(0); i; })
#macro A2
	scope
		'' TODO: int a[({ f(); })];
	end scope
#endmacro

end extern

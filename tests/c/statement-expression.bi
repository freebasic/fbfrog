#pragma once

'' TODO: #define A1 ({ int i = foo(0); i; })
#macro A2
	scope
		'' TODO: int a[({ f(); })];
	end scope
#endmacro

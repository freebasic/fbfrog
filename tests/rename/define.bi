#pragma once

'' The following symbols have been renamed:
''     constant A1 => A2
''     #ifdef __FB_WIN32__
''         constant E1 => E2
''     #endif

#undef A2
const A2 = 1
'' TODO: #define B1 ??? provoke TODO ???
#define C(x2) x2
#define D x1
dim shared i as long = x1

#ifdef __FB_WIN32__
	const E2 = &he
#endif

#pragma once

'' The following symbols have been renamed:
''     constant A1 => A2
''     #ifdef __FB_WIN32__
''         constant E1 => E2
''     #endif

' @fbfrog -renamedefine A1 A2 -renamedefine B1 B2 -renamemacroparam x1 x2 -renamedefine E1 E2
#undef A2
const A2 = 1
'' TODO: #define B1 ??? provoke TODO ???
#define C(x2) x2
' Here, x1 isn't a macro parameter, and shouldn't be renamed by -renamemacroparam
#define D x1
dim shared i as long = x1

#ifdef __FB_WIN32__
	const E2 = &he
#endif

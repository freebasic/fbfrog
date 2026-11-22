#pragma once

extern "C"

' @fbfrog -declareversions VER 1 2 -selectversion -case 1 -define A -endselect
extern a1 as long

#if VER = 1
	extern a2 as long
#endif

extern a3 as long

end extern

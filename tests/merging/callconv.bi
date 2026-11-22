#pragma once

#if VER = 1
	extern "Windows"
#else
	extern "C"
#endif

' @fbfrog -removedefine CALLCONV -declareversions VER 1 2 -selectversion -case 1 -define A -endselect
declare sub f1()
declare sub f2()

end extern

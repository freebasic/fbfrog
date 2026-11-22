#pragma once

' @fbfrog -declareversions VER 1 2 -selectversion -case 1 -define V1 -endselect

type UDT_a
	#if VER = 1
		v1_a as long
	#else
		v2_a as long
	#endif
end type

#if VER = 1
	type UDT_b
		v1_b as long
	end type
#endif

type UDT
	a as UDT_a

	#if VER = 1
		b as UDT_b
	#endif
end type

' v1 should have: UDT.a.v1_a, UDT.b.v1_b
' v2 should have: UDT.a.v2_a

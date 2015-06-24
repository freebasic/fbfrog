// @fbfrog -declareversions VER 1 2 -selectversion -case 1 -define V1 -endselect

struct UDT {
	#ifdef V1
		struct { int v1_a; } a;
		struct { int v1_b; } b;
	#else
		struct { int v2_a; } a;
	#endif
};

// v1 should have: UDT.a.v1_a, UDT.b.v1_b
// v2 should have: UDT.a.v2_a

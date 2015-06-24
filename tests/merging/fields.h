// @fbfrog -declareversions VER 1 2 -selectversion -case 1 -define A -endselect

struct UDT1 {
	int field1;
	int field2;
};

struct UDT2 {
	int field1;
	#ifdef A
		int field2;
	#else
		int field3;
	#endif
	int field4;
};

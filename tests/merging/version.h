// @fbfrog -declareversions VER 1 -selectversion -case 1 -define VER 1 -endselect

static int common;

#if VER == 1
	static int v1;
	struct UDT1v1 {
		int fieldv1;
	};
#else
	#error "invalid VER value"
#endif

struct UDT2v1 {
	int fieldv1;
};

struct UDT3v1 {
	#if VER == 1
		int fieldv1;
	#endif
};

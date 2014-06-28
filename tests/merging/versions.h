// @fbfrog <dir>versions.fbfrog

static int common;

#if VER == 1
	static int v1;
	struct UDTv1 {
		int fieldv1;
	};
#elif VER == 2
	static int v2;
	struct UDTv2 {
		int fieldv2;
	};
#elif VER == 3
	static int v3;
	struct UDTv3 {
		int fieldv3;
	};
#else
	#error "invalid VER value"
#endif

#if VER == 1 || VER == 2
	static int v12;

	struct UDT1v12 {
		int fieldv12;
	};

	struct UDT2v12 {
		#if VER == 1
			int fieldv1;
		#else
			int fieldv2;
		#endif
	};
#endif

struct UDT1v123 {
	int fieldv123;
};

struct UDT2v123 {
	#if VER == 1
		int fieldv1;
	#elif VER == 2
		int fieldv2;
	#elif VER == 3
		int fieldv3;
	#endif
};

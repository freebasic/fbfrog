// @fbfrog -version 1 -define VER 1

static int common;

#if VER == 1
	static int v1;
	struct UDTv1 {
		int fieldv1;
	};
#else
	#error "invalid VER value"
#endif

struct UDTv1 {
	int fieldv1;
};

struct UDTv1 {
	#if VER == 1
		int fieldv1;
	#endif
};

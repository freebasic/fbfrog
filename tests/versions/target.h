// @fbfrog -target linux -define LINUX

static int linux;

#ifdef LINUX
	static int linux;
	struct UDTlinux1 {
		int fieldlinux;
	};
#else
	#error "invalid target"
#endif

struct UDTlinux2 {
	int fieldlinux;
	#ifdef LINUX
		int fieldlinux;
	#endif
};

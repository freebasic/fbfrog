static int linux;

#ifdef LINUX
	static int linux;
	struct UDTlinux {
		int fieldlinux;
	};
#else
	#error "invalid target"
#endif

struct UDTlinux {
	int fieldlinux;
	#ifdef LINUX
		int fieldlinux;
	#endif
};

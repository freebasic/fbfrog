// @fbfrog -target dos -define DOS -target linux -define LINUX -target win32 -define WIN32

static int doslinuxwin32;

#ifdef DOS
	static int dos;
	struct UDTdos {
		int fielddos;
	};
#elif defined LINUX
	static int linux;
	struct UDTlinux {
		int fieldlinux;
	};
#elif defined WIN32
	static int win32;
	struct UDTwin32 {
		int fieldwin32;
	};
#else
	#error "invalid target"
#endif

#if defined DOS || defined LINUX
	static int doslinux;

	struct UDTdoslinux1 {
		int fielddoslinux;
	};

	struct UDTdoslinux2 {
		#ifdef DOS
			int fielddos;
		#else
			int fieldlinux;
		#endif
	};
#endif

struct UDTdoslinuxwin32 {
	int fielddoslinuxwin32;
};

struct UDTfielddoslinuxwin32 {
	#ifdef DOS
		int fielddos;
	#elif defined LINUX
		int fieldlinux;
	#elif defined WIN32
		int fieldwin32;
	#endif
};

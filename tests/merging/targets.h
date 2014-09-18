static int vardoslinuxwin32;

#ifdef DJGPP
	static int vardos;
	struct UDTdos {
		int fielddos;
	};
#elif defined __linux__
	static int varlinux;
	struct UDTlinux {
		int fieldlinux;
	};
#elif defined _WIN32
	static int varwin32;
	struct UDTwin32 {
		int fieldwin32;
	};
#else
	#error "invalid target"
#endif

#if defined DJGPP || defined __linux__
	static int vardoslinux;

	struct UDTdoslinux1 {
		int fielddoslinux;
	};

	struct UDTdoslinux2 {
		#ifdef DJGPP
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
	#ifdef DJGPP
		int fielddos;
	#elif defined __linux__
		int fieldlinux;
	#elif defined _WIN32
		int fieldwin32;
	#endif
};

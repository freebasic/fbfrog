static int varall;

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

struct UDTall {
	int fieldall;
};

struct UDTall2 {
	#ifdef DJGPP
		int fielddos;
	#elif defined __linux__
		int fieldlinux;
	#elif defined _WIN32
		int fieldwin32;
	#endif
};

#ifndef DJGPP
	extern int nondos;
#endif

#if defined __LP64__ || defined __x86_64__
	extern int _64bit;
#endif

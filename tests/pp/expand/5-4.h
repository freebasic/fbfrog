#ifdef A
	#ifdef B
		#define EXPANDTHIS
	#else
	#endif
#else
	#ifdef B
		#define EXPANDTHIS
	#else
	#endif
#endif
EXPANDTHIS void f(void);

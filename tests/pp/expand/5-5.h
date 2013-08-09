#ifdef A
	#ifdef B
	#else
		#define EXPANDTHIS
	#endif
#else
	#ifdef B
	#else
		#define EXPANDTHIS
	#endif
#endif
EXPANDTHIS void f(void);

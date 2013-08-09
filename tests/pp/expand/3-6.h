#ifdef A
	#ifdef B
		#define EXPANDTHIS
	#else
		#define EXPANDTHIS
	#endif
#else
#endif
EXPANDTHIS void f(void);

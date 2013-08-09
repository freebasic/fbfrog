#ifdef A
#else
	#ifdef B
		#define EXPANDTHIS
	#else
		#define EXPANDTHIS
	#endif
#endif
EXPANDTHIS void f(void);

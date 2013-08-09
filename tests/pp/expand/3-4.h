#ifdef A
	#ifdef B
		#define EXPANDTHIS
	#else
	#endif
#else
#endif
EXPANDTHIS void f(void);

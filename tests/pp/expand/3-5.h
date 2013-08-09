#ifdef A
	#ifdef B
	#else
		#define EXPANDTHIS
	#endif
#else
#endif
EXPANDTHIS void f(void);

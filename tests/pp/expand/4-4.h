#ifdef A
#else
	#ifdef B
		#define EXPANDTHIS
	#else
	#endif
#endif
EXPANDTHIS void f(void);

#ifdef A
#else
	#ifdef B
	#else
		#define EXPANDTHIS
	#endif
#endif
EXPANDTHIS void f(void);

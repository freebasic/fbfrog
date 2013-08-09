#ifdef A
#else
	#ifdef B
		#define EXPANDTHIS
	#endif
#endif
EXPANDTHIS void f(void);

#ifdef A
	#ifdef B
		#define EXPANDTHIS
	#else
	#endif
	EXPANDTHIS void f(void);
#else
	EXPANDTHIS void f(void);
#endif

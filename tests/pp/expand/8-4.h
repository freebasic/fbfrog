#ifdef A
	EXPANDTHIS void f(void);
#else
	#ifdef B
		#define EXPANDTHIS
	#else
	#endif
	EXPANDTHIS void f(void);
#endif

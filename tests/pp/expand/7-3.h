#ifdef A
	#ifdef B
		#define EXPANDTHIS
	#endif
	EXPANDTHIS void f(void);
#else
	EXPANDTHIS void f(void);
#endif

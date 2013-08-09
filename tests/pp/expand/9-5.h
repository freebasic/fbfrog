#ifdef A
	#ifdef B
	#else
		#define EXPANDTHIS
	#endif
	EXPANDTHIS void f(void);
#else
	#ifdef B
	#else
		#define EXPANDTHIS
	#endif
	EXPANDTHIS void f(void);
#endif

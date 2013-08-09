#ifdef A
	#ifdef B
		#define EXPANDTHIS
	#endif
#else
	#ifdef B
		#define EXPANDTHIS
	#endif
#endif
EXPANDTHIS void f(void);

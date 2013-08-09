#ifdef A
	EXPANDTHIS void f(void);
#else
	#ifdef B
	#else
	#endif
	EXPANDTHIS void f(void);
#endif

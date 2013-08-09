#ifdef A
	EXPANDTHIS void f(void);
#else
	#ifdef B
	#endif
	EXPANDTHIS void f(void);
#endif

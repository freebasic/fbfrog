#ifdef A
	#ifdef B
	#else
	#endif
	EXPANDTHIS void f(void);
#else
	#ifdef B
	#else
	#endif
	EXPANDTHIS void f(void);
#endif

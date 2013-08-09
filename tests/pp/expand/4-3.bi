#ifdef A
	'' TODO: unknown construct
	EXPANDTHIS void f(void);
#else
	#ifdef B
		#define EXPANDTHIS
	#else
	#endif
	'' TODO: unknown construct
	EXPANDTHIS void f(void);
#endif

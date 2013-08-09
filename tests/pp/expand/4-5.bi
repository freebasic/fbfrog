#ifdef A
	'' TODO: unknown construct
	EXPANDTHIS void f(void);
#else
	#ifdef B
	#else
		#define EXPANDTHIS
	#endif
	'' TODO: unknown construct
	EXPANDTHIS void f(void);
#endif

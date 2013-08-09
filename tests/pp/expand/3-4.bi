#ifdef A
	#ifdef B
		#define EXPANDTHIS
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#else
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#endif
#else
	'' TODO: unknown construct
	EXPANDTHIS void f(void);
#endif

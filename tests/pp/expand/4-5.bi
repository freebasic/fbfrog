#ifdef A
	'' TODO: unknown construct
	EXPANDTHIS void f(void);
#else
	#ifdef B
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#else
		#define EXPANDTHIS
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#endif
#endif

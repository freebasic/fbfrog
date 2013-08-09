#ifdef A
	'' TODO: unknown construct
	EXPANDTHIS void f(void);
#else
	#ifdef B
		#define EXPANDTHIS
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#else
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#endif
#endif

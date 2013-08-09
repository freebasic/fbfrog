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
	#ifdef B
		#define EXPANDTHIS
	#else
	#endif
	'' TODO: unknown construct
	EXPANDTHIS void f(void);
#endif

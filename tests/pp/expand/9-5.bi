#ifdef A
	#ifdef B
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#else
		#define EXPANDTHIS
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#endif
#else
	#ifdef B
	#else
		#define EXPANDTHIS
	#endif
	'' TODO: unknown construct
	EXPANDTHIS void f(void);
#endif

#ifdef A
	#ifdef B
		#define EXPANDTHIS
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#else
		#define EXPANDTHIS
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#endif
#else
	#ifdef B
		#define EXPANDTHIS
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#else
		#define EXPANDTHIS
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#endif
#endif

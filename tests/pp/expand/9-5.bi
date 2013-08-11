#ifdef A
	#ifdef B
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#else
		#define EXPANDTHIS
		declare sub f( )
	#endif
#else
	#ifdef B
		'' TODO: unknown construct
		EXPANDTHIS void f(void);
	#else
		#define EXPANDTHIS
		declare sub f( )
	#endif
#endif

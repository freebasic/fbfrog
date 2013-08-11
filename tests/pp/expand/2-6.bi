#ifdef A
	#ifdef B
		#define EXPANDTHIS
		declare sub f( )
	#else
		#define EXPANDTHIS
		declare sub f( )
	#endif
#else
	'' TODO: unknown construct
	EXPANDTHIS void f(void);
#endif

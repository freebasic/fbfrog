#ifdef A
	#ifdef B
		#define EXPANDTHIS
		declare sub f( )
	#else
		#define EXPANDTHIS
		declare sub f( )
	#endif
#else
	#ifdef B
		#define EXPANDTHIS
		declare sub f( )
	#else
		#define EXPANDTHIS
		declare sub f( )
	#endif
#endif

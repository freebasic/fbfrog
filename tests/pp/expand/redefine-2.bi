#define EXPANDME1 f1

#ifdef foo
	declare sub f1( )
	#undef EXPANDME1
	declare sub EXPANDME1( )
	#ifdef bar
		declare sub EXPANDME1( )
		#define EXPANDME1 f2
		declare sub f2( )
		declare sub f2( )

		declare sub f2( )
	#else
		declare sub EXPANDME1( )

		declare sub EXPANDME1( )
	#endif
#else
	declare sub f1( )

	declare sub f1( )
#endif

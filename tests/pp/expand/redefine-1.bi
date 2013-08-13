#define EXPANDME1 f1
declare sub f1( )

#ifdef foo
	declare sub f1( )
	#undef EXPANDME1
	declare sub EXPANDME1( )
	#define EXPANDME1 f2
	declare sub f2( )

	declare sub f2( )
#else
	declare sub f1( )
	#undef EXPANDME1
	declare sub EXPANDME1( )
	#define EXPANDME1 f3
	declare sub f3( )

	declare sub f3( )
#endif

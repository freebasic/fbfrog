#define EXPANDME1 foo

#ifdef foo

	declare sub foo( )
#else
	#undef EXPANDME1

	declare sub EXPANDME1( )
#endif

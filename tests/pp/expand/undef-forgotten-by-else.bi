#define EXPANDME1 foo

#ifdef foo
	#undef EXPANDME1
#else
	declare sub foo( )
#endif

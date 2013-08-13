#define EXPANDME1 foo

#ifdef foo
#else
	#undef EXPANDME1
#endif

void EXPANDME1(void);

#define EXPANDME1 foo

#ifdef foo
	#undef EXPANDME1
#endif

void EXPANDME1(void);

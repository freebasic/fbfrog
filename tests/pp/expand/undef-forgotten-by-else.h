#define EXPANDME1 foo

#ifdef foo
	#undef EXPANDME1
#else
	void EXPANDME1(void);
#endif

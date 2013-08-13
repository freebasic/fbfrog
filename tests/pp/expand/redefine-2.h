#define EXPANDME1 f1

#ifdef foo
	void EXPANDME1(void);
	#undef EXPANDME1
	void EXPANDME1(void);
	#ifdef bar
		void EXPANDME1(void);
		#define EXPANDME1 f2
		void EXPANDME1(void);
	#endif
	void EXPANDME1(void);
#else
	void EXPANDME1(void);
#endif

void EXPANDME1(void);

#ifdef _WIN32
	void f0(void);
	extern void f1(void); // can really happen, same as <void f1(void);>
	static void f2(void); // doesn't make much sense, but nevertheless, different from <void f2(void);>
#else
	void f0(void);
	void f1(void);
	void f2(void);
#endif

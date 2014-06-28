// @fbfrog -declarebool A -ifdef A -define A -endif

#ifdef A
	static int i1;
#else
	static int i2;
#endif

// @fbfrog -version 1 -define A -version 2

extern int a1;

#ifdef A
	extern int a2;
#endif

extern int a3;

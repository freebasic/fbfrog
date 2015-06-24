// @fbfrog -declareversions VER 1 2 -selectversion -case 1 -define A -endselect

extern int a1;

#ifdef A
	extern int a2;
#endif

extern int a3;

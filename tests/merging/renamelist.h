// @fbfrog -declareversions VER 1 2 -select VER -case 1 -define A -endselect

void string(void);
#ifdef A
	void single(void);
#else
	void integer(void);
#endif

#if 0 && defined foo
	extern short no;
#endif
extern short separator1;

#if 1 || defined foo
	extern short yes;
#endif
extern short separator4;

#if 1 + 2 == 3
	extern short yes;
#endif
extern short separator5;

#define UNKNOWNSYM
#ifdef UNKNOWNSYM
	extern short yes;
#endif
extern short separator10;

#undef UNKNOWNSYM
#ifdef UNKNOWNSYM
	extern short no;
#endif
extern short separator11;

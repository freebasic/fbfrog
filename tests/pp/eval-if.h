#if 0
	extern short a1;
#endif
extern short separator1;

#if 1
	extern short a1;
#endif
extern short separator2;

#if 0
	extern short a1;
#else
	extern short b1;
#endif
extern short separator3;

#if 1
	extern short a1;
#else
	extern short b1;
#endif
extern short separator4;

#if 0
	extern short a1;
#elif 0
	extern short b1;
#endif
extern short separator5;

#if 0
	extern short a1;
#elif 1
	extern short b1;
#endif
extern short separator6;

#if 1
	extern short a1;
#elif 0
	extern short b1;
#endif
extern short separator7;

#if 1
	extern short a1;
#elif 1
	extern short b1;
#endif
extern short separator8;

#if 0
	extern short a1;
#elif 0
	extern short b1;
#else
	extern short c1;
#endif
extern short separator9;

#if 0
	extern short a1;
#elif 1
	extern short b1;
#else
	extern short c1;
#endif
extern short separator10;

#if 1
	extern short a1;
#elif 0
	extern short b1;
#else
	extern short c1;
#endif
extern short separator11;

#if 1
	extern short a1;
#elif 1
	extern short b1;
#else
	extern short c1;
#endif
extern short separator12;







#if 1
#if 0
a
#endif
#elif 0
b
#else
c
#endif

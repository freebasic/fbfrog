#if 0
	short a1;
#endif
short separator1;

#if 1
	short a1;
#endif
short separator2;

#if 0
	short a1;
#else
	short b1;
#endif
short separator3;

#if 1
	short a1;
#else
	short b1;
#endif
short separator4;

#if 0
	short a1;
#elseif 0
	short b1;
#endif
short separator5;

#if 0
	short a1;
#elseif 1
	short b1;
#endif
short separator6;

#if 1
	short a1;
#elseif 0
	short b1;
#endif
short separator7;

#if 1
	short a1;
#elseif 1
	short b1;
#endif
short separator8;

#if 0
	short a1;
#elseif 0
	short b1;
#else
	short c1;
#endif
short separator9;

#if 0
	short a1;
#elseif 1
	short b1;
#else
	short c1;
#endif
short separator10;

#if 1
	short a1;
#elseif 0
	short b1;
#else
	short c1;
#endif
short separator11;

#if 1
	short a1;
#elseif 1
	short b1;
#else
	short c1;
#endif
short separator12;







#if 1
#if 0
a
#endif
#elif 0
b
#else
c
#endif

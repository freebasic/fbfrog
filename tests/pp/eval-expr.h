#if 0 && defined foo
	extern short no;
#endif
extern short separator1;

#if 1 && defined foo
#endif
extern short separator2;

#if 0 || defined foo
#endif
extern short separator3;

#if 1 || defined foo
	extern short yes;
#endif
extern short separator4;

#if 1 + 2 == 3
	extern short yes;
#endif
extern short separator5;

#if defined FOO && FOO == 123
#endif
extern short separator6;

#ifdef KNOWNDEFINED1
	extern short yes;
#endif
extern short separator7;

#ifndef KNOWNUNDEFINED1
	extern short yes;
#endif
extern short separator8;

#ifdef UNKNOWNSYM
#endif
extern short separator9;

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

// @fbfrog -whitespace -nonamefixup

// "defined m1" shouldn't be expanded to "defined 123", because no
// expansion should be done for "defined id" expressions. "defined <number>" is
// good for the test case because it would trigger a syntax error.
#define m1 123

#ifdef m1
	extern int yes;
#endif
extern int separator1;

#ifndef m1
	extern int no;
#endif
extern int separator2;

#if defined m1
	extern int yes;
#endif
extern int separator3;

#if defined(m1)
	extern int yes;
#endif
extern int separator4;

#if defined m1 && defined m1
#endif
extern int separator5;

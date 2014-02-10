// "defined EXPANDME1" shouldn't be expanded to "defined 123", because no
// expansion should be done for "defined id" expressions. "defined <number>" is
// good for the test case because it's an invalid expression.
#define EXPANDME1 123

#ifdef EXPANDME1
	extern int yes;
#endif
extern int separator1;

#ifndef EXPANDME1
	extern int no;
#endif
extern int separator2;

#if defined EXPANDME1
	extern int yes;
#endif
extern int separator3;

#if defined(EXPANDME1)
	extern int yes;
#endif
extern int separator4;

#if defined EXPANDME1 && defined EXPANDME1
#endif
extern int separator5;

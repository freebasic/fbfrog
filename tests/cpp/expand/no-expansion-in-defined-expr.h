// @fbfrog -removedefine B_is_defined

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

// Here, the defined operator appears as the result of macro expansion. We
// should still not expand its operand, like gcc.
#define B_is_defined defined B
#define B 1/0
#if B_is_defined
	void f(void);
#endif
extern int separator6;
#undef B_is_defined
#undef B

// defined operator as macro call arg
#define some(x) x
#define B 1/0
#if some(defined) B
	void f(void);
#endif
extern int separator7;
#undef some
#undef B

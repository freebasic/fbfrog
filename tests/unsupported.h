/*
 * typedefs to procdecls (FB doesn't know something like that):
 *
 *    typedef int T(int);
 *    T t;
 *    t(5);
 *
 * Same as this, right?
 *
 *    int t(int);
 *    t(5);
 */
typedef int T(int);

enum {
#if 1
	A    /* next token is '#' instead of ',' or '}', preventing the translation */
#endif
};

/* Cannot have #directives mixed into the expression in FB */
enum {
	A =
	1
#if 1
	+
	2
#endif
	,
	B
};

/* Named nested unions/structs -- not supported in FB */
struct T {
	union {
		int a;
	} foo;
};

/* Array typedef */
typedef int MYINTARRAY[10];

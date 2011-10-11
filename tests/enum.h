enum {
	A = 0,               /* This is A */
	B, C = (1 << 4),     /* This is B and C */
	D,                   /* This is D */
	E = CALC(1,2,3),
	F
};

// enum E : ... : end enum
// type EE as E
// (Both ids might be needed)
typedef enum E { A, B } EE;

typedef enum E { A, B, } EE;

typedef enum E {
A, B} EE;

typedef enum E {A,
B} EE;

typedef enum E {A, B
} EE;


// enum EE : ... : end enum
typedef enum {
	A,
	B,
	C
} EE;

// enum E : ... : end enum
// (also, any places using <enum E> will become just <E>, so they work ok)
enum E {
	A,
#if 1
	B,
#endif
	C
};

/* expression split across multiple lines, but should be fixed up with _ */
enum {
	A =
	1
	+
	CALC(2,3)
	,
	B
};

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

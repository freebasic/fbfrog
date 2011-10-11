enum {
	A = 0,               /* This is A */
	B, C = (1 << 4),     /* This is B and C */
	D,                   /* This is D */
	E = CALC(1,2,3),
	F
};

// enum E2 : ... : end enum
// type EE2 as E2
// (Both ids might be needed)
typedef enum E2 { A, B } EE2;

// enum EE3 : ... : end enum
typedef enum {
	A,
	B,
	C
} EE3;

// enum E1 : ... : end enum
// (also, any places using <enum E1> will become just <E1>, so they work ok)
enum E1 {
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

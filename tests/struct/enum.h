enum {
	A = 0,
	B, C = (1 << 4),
	D,
	E = CALC(1,2,3),
	F
};

enum E {
	A,
#if 1
	B,
#endif
	C
};

enum {
	A =
	1
	+
	CALC(2,3)
	,
	B
};

enum E { A, B, };
enum E{A,B,};
enum E { A, B,
};

enum {
	A = 0,
	B, C = (1 << 4),
	D,
	E = 1, F
	, G
};

enum E {
	A,
#if 1
	B,
#endif
	C
};

enum {
	A
	=
	1
	,
	B
};

enum E { A, B, };
enum E{A,B,};
enum E { A, B,
};

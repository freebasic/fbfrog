enum E1 {
};

enum {
	A2 = 0,
	B2, C2 = (1 << 4),
	D2,
	E2 = 1, F2
	, G2
};

enum E3 {
	A3,
#if 1
	B3,
#endif
	C3
};

enum {
	A4
	=
	1
	,
	B4
};

enum E5 { A5, B5, };
enum E6{A6,B6,};
enum E7 { A7, B7,
};

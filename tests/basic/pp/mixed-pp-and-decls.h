enum E {
#if 1
	A
#endif
};

enum E {
	A =
	1
#if 1
	+
	2
#endif
	,
	B
};

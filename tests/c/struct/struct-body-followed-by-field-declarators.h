struct UDT {
	struct { int a; } a;
	struct { int a; } b, c, d;
	struct { int a; } *e;
	struct { int a; } (*f)(void);
};

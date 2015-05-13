static int i1 = 1, 2;

int f(int) {
	f(1), f(2);
	int a, b;
	a, b;
	return f(1), f(2);
};

if (1)
	f(1), f(2);

if (1) {
	f(1), f(2);
}

#define M1 (1, 2)
#define M2(x, y) (f(x), f(y))
#define M3 1, 2, 3 // chances are this is intended for an array/struct initializers, not comma operator
#define M4 if (1) f(1), f(2);
#define M5 if (1) { f(1), f(2); }
#define M6 { if (1) f(1), f(2); }
#define M7 { if (1) { f(1), f(2); } }

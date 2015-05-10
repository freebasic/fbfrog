static int i1 = 1, 2;

int f(int) {
	f(1), f(2);
	int a, b;
	a, b;
	return f(1), f(2);
};

#define M1 (1, 2)
#define M2(x, y) (f(x), f(y))

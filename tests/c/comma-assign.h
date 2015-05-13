static int i1 = i1 = 1, 2;

void f(void) {
	int a, b;
	a = 1, b = 2;
};

if (1)
	a = 1, b = 2;

if (1) {
	a = 1, b = 2;
}

#define M1(x, y) ((x) = 1, (y) = 2)
#define M2 if (1) a = 1, b = 2;
#define M3 if (1) { a = 1, b = 2; }
#define M4 { if (1) a = 1, b = 2; }
#define M5 { if (1) { a = 1, b = 2; } }

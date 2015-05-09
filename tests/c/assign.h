static int i1 = i1 = 0;

void f1(void) {
	int a;
	a = 1;
	a = a = 2;
};

#define M1(x) x = 1
#define M2(x) ((x) = 1)
#define M3(x) ((x) = (x) = 1)

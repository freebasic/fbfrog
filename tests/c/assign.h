static int i1 = i1 = 0;

void f1(void) {
	int a;
	a = 1;
	a = a = 2;
};

if (1)
	a = 1;

if (1) {
	a = 1;
}

#define M1(x) x = 1
#define M2(x) ((x) = 1)
#define M3(x) ((x) = (x) = 1)
#define M4 if (1) a = 1;
#define M5 if (1) { a = 1; }
#define M6 { if (1) a = 1; }
#define M7 { if (1) { a = 1; } }

{
	a = 1;
	a |= 1;
	a ^= 1;
	a &= 1;
	a <<= 1;
	a >>= 1;
	a += 1;
	a -= 1;
	a *= 1;
	a /= 1;
	a %= 1;
}

#define ASSIGN_01 a = 1
#define ASSIGN_02 a |= 1
#define ASSIGN_03 a ^= 1
#define ASSIGN_04 a &= 1
#define ASSIGN_05 a <<= 1
#define ASSIGN_06 a >>= 1
#define ASSIGN_07 a += 1
#define ASSIGN_08 a -= 1
#define ASSIGN_09 a *= 1
#define ASSIGN_10 a /= 1
#define ASSIGN_11 a %= 1

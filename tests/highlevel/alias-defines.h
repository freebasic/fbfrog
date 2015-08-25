#define C1 123
#define C2 C1

enum {
	E1 = 123
};
#define E2 E1

typedef int T1;
#define T2 T1

void F1(void);
#ifdef _WIN32
	__attribute__((__dllimport__, __stdcall__)) int F3(double, float) asm("__F3__");
#else
	int F3(double, float) asm("__F3__");
#endif
#define F2 F1
#define F4 F3

struct Struct1 {
	int dummy;
};
#define Struct2 Struct1
#define Struct2_ struct Struct1

union Union1 {
	int dummy;
};
#define Union2 Union1
#define Union2_ union Union1

enum Enum1 {
	Enum1_dummy = 0
};
#define Enum2 Enum1
#define Enum2_ enum Enum1

int V1;
#define V2 V1

extern int EV1;
extern int EV3 asm("__EV3__");
#define EV2 EV1
#define EV4 EV3

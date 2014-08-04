// @fbfrog -constants

#define A 1
#define B() 1
#define C(a) 1
#define D f()
#define E A

struct UDT1 {
	int field;
	#define FIELD_1 1
	#define FIELD_2 2
};

#define A1 1
#define A2 1 + 2 * 3

#define B1(x) x
#define B2() x
#define B3

#define C1 x
void cool2(int);
#define C2 cool2(1)

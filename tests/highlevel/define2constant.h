#define A 1
#define B() 1
#define C(a) 1
#define D f()
#define E A
#define F (A + E * 123)

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

#define D1 sizeof(int)
#define D2 sizeof(struct UDT1)

#define D3 ((int)0)
#define D4 ((struct UDT1 *)0)
#define D5 ((struct UDT1 *(*)(void))0)
#define D6 ((void (*)(struct UDT1 *))0)
#define D7 ((struct UndeclaredUdt *)0)

typedef struct AAA {
	int i;
} AAA;

static AAA *p;
void f(AAA *p);

struct BBB {
	AAA aaa;
};

typedef AAA CCC;

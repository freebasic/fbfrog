#define INT(x) x

enum {
	AND = 0,
	OR = AND,
	XOR = INT(1) + OR
};

int array1[INT(1)];

#define A1 INT(1)

struct DOUBLE {
	DOUBLE *a;
};

void f(struct DOUBLE);

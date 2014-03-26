#define INT(x) x

enum {
	AND = 0,
	OR = AND,
	XOR = INT(1) + OR
};

int array1[INT(1)];

#define A1 INT(1)

typedef struct DOUBLE DOUBLE;

struct DOUBLE {
	struct DOUBLE *a;
	DOUBLE *b;
};

void f1(struct DOUBLE);
void f2(DOUBLE);

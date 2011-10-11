struct T {
	int a, **b, **c;
	int *aa, bb, cc, **dd, **ee, ff;
	struct T2 *x, ****y, z;
};

struct T { int   a,   b,   c; };
struct T { int  *a,   b,   c; };
struct T { int **a,   b,   c; };

struct T { int   a,  *b,   c; };
struct T { int  *a,  *b,   c; };
struct T { int **a,  *b,   c; };

struct T { int   a, **b,   c; };
struct T { int  *a, **b,   c; };
struct T { int **a, **b,   c; };

struct T { int   a,   b,  *c; };
struct T { int  *a,   b,  *c; };
struct T { int **a,   b,  *c; };

struct T { int   a,  *b,  *c; };
struct T { int  *a,  *b,  *c; };
struct T { int **a,  *b,  *c; };

struct T { int   a, **b,  *c; };
struct T { int  *a, **b,  *c; };
struct T { int **a, **b,  *c; };

struct T { int   a,   b, **c; };
struct T { int  *a,   b, **c; };
struct T { int **a,   b, **c; };

struct T { int   a,  *b, **c; };
struct T { int  *a,  *b, **c; };
struct T { int **a,  *b, **c; };

struct T { int   a, **b, **c; };
struct T { int  *a, **b, **c; };
struct T { int **a, **b, **c; };

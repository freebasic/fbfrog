// type T2 : ... : end type
// type TT2 as T2
// (Both ids might be needed)
typedef struct T2 {
	int a;
	double x;
} TT2;

// type TT3 : ... : end type
typedef struct {
	int a;
	double x;
} TT3;

// type T1 : ... : end type
// (also, any places using <struct T1> will become just <T1>, so they work ok)
struct T1 {
	signed int i;
	unsigned long long int j;
	unsigned k;
	double a,b,c;
	struct T2 *x, ****y, z;
	int *aa, bb, cc, **dd, **ee, ff;
	int a, **b, **c;
};

typedef struct { int a, **b, **c; } T3;

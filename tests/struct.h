// type T : ... : end type
// type TT as T
// (Both ids might be needed)
typedef struct T {
	int a;
	double x;
} TT;

// type TT : ... : end type
typedef struct {
	int a;
	double x;
} TT;

// type T : ... : end type
// (also, any places using <struct T> will become just <T>, so they work ok)
struct T {
	signed int i;
#if 1
	unsigned long long int j;
#endif
	unsigned k;
	double a,b,c;
	struct T2 ****y;
};

typedef struct { int a; } T;

union U {
	int a;
	int b;
	struct {
		int c;
		union {
			int d;
			int e;
		};
		int f;
	};
	int g;
	struct { int h; };
	struct {
		union {
			struct {
				union {
				};
			};
		};
	};
};

// union U : ... : end union
// type UU as U
// (Both ids might be needed)
typedef union U { int a; } UU;

// union UU : ... : end union
typedef union { int a; } UU;

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

typedef struct { union { struct { int a; int b; }; int c; }; } T;

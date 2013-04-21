// type T : ... : end type
// type as T A
// (Both ids might be needed)
typedef struct T {
	int a;
} A;

// Anonymous struct typedef
// type TT : ... : end type
typedef struct {
	int a;
} TT;

// Anonymous struct typedef triggering the fake id insertion
typedef struct {
	int a;
} A, *PA;

// type T : ... : end type
// type as T A, B : type as T ptr C : type as function() as T D
typedef struct T {
	int a;
} A, B, *C, (*D)();

// type T : ... : end type
// (also, any places using <struct T> will become just <T>, so they work ok)
struct T {
	int a;
};

typedef struct { int a; } T;

struct T { int a; };



/**
 * Unions
 */

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

typedef struct { union { struct { int a; int b; }; int c; }; } T;



/**
 * Enums
 */

// Anonymous enum
// enum : ... : end enum
enum {
	A = 0,               /* This is A */
	B, C = (1 << 4),     /* This is B and C */
	D,                   /* This is D */
	E = CALC(1,2,3),
	F
};

// enum E : ... : end enum
// (also, any places using <enum E> will become just <E>, so they work ok)
enum E {
	A,
#if 1
	B,
#endif
	C
};

/* expression split across multiple lines, but should be fixed up with _ */
enum {
	A =
	1
	+
	CALC(2,3)
	,
	B
};

// enum E : ... : end enum
// type EE as E
// (Both ids might be needed)
typedef enum E { A, B } EE;


// Extra comma
enum E { A, B, };
enum E{A,B,};
enum E { A, B,
};

// enum EE : ... : end enum
typedef enum {
	A,
	B,
	C
} EE;

typedef enum E {
A, B} EE;

typedef enum E {A,
B} EE;

typedef enum E {A, B
} EE;



/**
 * Extern blocks
 */

extern "C" {
}

extern "C"{}

extern "C" { }

extern
"C"{}

extern
"C"
{
}

#ifdef __cplusplus
extern "C" {
#endif

/* foo */

#ifdef __cplusplus
}
#endif

// fields
struct UDT {
	signed int i;
	unsigned long long int j;
	unsigned k;
	double a,b,c;

	int **a, *a, a;
	int a, **a, **a;
	int *a, a, a, **a, **a, a;

	struct T ****y;
	struct T *a, ****a, a;

	int *(*p)(int*);
	int f(int, int);
	void proc(void);

	int a;
#if 1
	int b;
#endif
	int c;
};

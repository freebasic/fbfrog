// variables
int a;
int *a;
int **a;
int ***a;
int ****a;
int a, b;
int a, b, c, d;
int  *a,   b;
int   a,  *b;
int  *a,  *b;
int  *a,   b,  **c;
int **a,   b,   *c;
int   a,  *b,    c;
int  *a, **b, ***c;
int   a,  *b, ***c,    d;
int **a,   b,    c,  **d;
int   a,  *b,    c,   *d;
int  *a,   b,   *c, ***d;

// sub/function return types
void f(void);
int f(void);
int *f(void);
UDT f(void);
UDT **f(void);

// no parameters
void f();
void f(void);

// parameters
void f(int a);
void f(int a, int b);
void f(int a, int b, int c);
void f(int *a, int ***b);

// anonymous parameters
void f(int);
void f(int, int);
void f(int, int, int);
void f(int *, int ***);

// vararg
void f(int a, ...);

// procptr variables
void (*a)(void);
int (*a)(int);
int (*a)(int a), (*b)(int a), c;
int a, (*b)(int a), c, (*d)(int a);
int **(*a)(int);

// function returning a procptr
int (*f(void))(void);

// procptr as param
void f(void (*a)(void));
void f(void (*)(void));

// procptr with procptr as param
void (*a)(void (*a)(void));
int ***(*p)(int ***(*)(int ***));

// variable/procptr/function
int a, *(*a)(void), a(void), *a(void);

// static/extern
static int a;
extern int a;
static void f(void);
extern void f(void);

// typedefs
typedef UDT A;
typedef int A;
typedef int *A;
typedef int A, B, C;
typedef void (*A)(void);
typedef UDT (*A)(UDT);
typedef UDT **A, B, (*C)(int);

struct UDT {
	// fields
	int a;
	int **a;
	int a, b;
	int  *a, b, *c, ***d;

	// methods
	void f(void);
	int f(void);
	void f();
	UDT **f(void);
	void f(int *a, int ***b);

	// procptr fields
	void (*a)(void);
	int (*a)(int);
	int (*a)(int a), (*b)(int a), c;
	int a, (*b)(int a), c, (*d)(int a);
	int **(*a)(int);

	// procptr as param
	void f(void (*a)(void));
	void f(void (*)(void));

	// procptr with procptr as param
	void (*a)(void (*a)(void));
	int ***(*p)(int ***(*)(int ***));
};

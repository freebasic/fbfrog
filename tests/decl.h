/**
 * Variables
 */

int a;
int *p;
int a, b;
int *p, ***p, x;

/* Sub pointer */
void (*p)();

/* Function pointer, with anonymous function pointer param */
double ***(*p)(int ***(*)(char***));

static int a;
extern int a;

/* Complex toplevel decl -- vardecl, procptr vardecl, procdecl */
int a, *(*a)(), a(), *a();

/**
 * Typedefs
 */

typedef struct T A;
typedef struct T (*A)(int i);
typedef T **A, B, (*C)(int);
typedef enum E *PE;
typedef union U /*boo*/ ****A;


/**
 * Procedures
 */

int f();
int f(void);

/* Function as any ptr */
void *f(void);

/* taking an int (but the id is omitted), returning an int */
int f(int);

/* some more params, and even ellipsis */
T *f(int x, short, char *, ...);

#define MY_EXTERN /*__declspec(dllexport)*/
#define MY_CALL __attribute__((__stdcall__))/*__stdcall*/

/* some #defines in front, as is pretty common */
MY_EXTERN MY_CALL TT1 f7(void);

/* Wrapped */
int f(int a, int b,
      int c, int d);

/* Taking a procptr param */
void f(void (*foo)(void));

/* Sub */
void test1();
void test2(int a, float *b, TT c, struct T **d);

static int f(void);
extern int f(void);

/**
 * Fields
 */

struct T {
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

	int a;
#if 1
	int b;
#endif
	int c;
};

#ifndef TEST_H
#define TEST_H

#ifdef __cplusplus
extern
"C" {             // oh look, this is on a new line!
#endif

// type T2 as T2_
// (this way we only need to translate the <struct T2 ...> body as
// <type T2_ ...>, that's easier than inserting T2_ fwdref in place of T2
// before the T2 body)
struct T2;

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
	int *(*p)(int*);
	int a, **b, **c;
};

// type TT1 as T1
// (typedef needed, since it's a different id)
typedef struct T1 TT1;

// type typedef_T1 as T1
// (typedef not needed, since any places using T1 will work anyways)
typedef struct T1 T1;

typedef struct { int a, **b, **c; } T3;

enum {
	A = 0,               /* This is A */
	B, C = (1 << 4),     /* This is B and C */
	D                    /* This is D */
};

/* sub */
void f01();

/* function as any ptr */
void *f02();

/* taking an int, returning an int */
int f03(int);

/* some more params, and even ellipsis */
int *f04(int x, short y, char *z, ...);

/* typedef */
TT1 *f05();

/* struct */
struct T1 f06(struct T1 *, TT1 ******);

#define MY_EXTERN /*__declspec(dllexport)*/
#define MY_CALL __attribute__((__stdcall__))/*__stdcall*/

/* some #defines in front, as is pretty common */
MY_EXTERN MY_CALL TT1 f07();

/* wrapped */
int f08(int a, int b,
        int c, int d);

/* sub ptr */
void (*fp01)();

/* function pointer, with function pointer param */
double *(*fpo02)(int ***(*)(char**));

/* PP expressions */
#if (!defined(FOO_BAR) && THIS_IS_INSANE >= 123) \
    || (OH_MAN_WHATS_THE_PRECEDENCE < 5 && (defined(OK) \
                                            || defined(I_DONT_KNOW)))
	#define PPMERGE(a, b) a##b
	#define PPSTRINGIZE(a) #a
	typedef unsigned __int8 uint8_t;
	typedef unsigned __int32 uint32_t;
	typedef unsigned __int64 uint64_t;

#	if X == 4294967295UL || X == 0.1e+1
#		define HOORAY
#	endif
#endif

#ifdef __cplusplus
}
#endif

#endif

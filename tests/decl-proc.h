int f();

int f(void);

/* function as any ptr */
void *f(void);

/* taking an int (but the id is omitted), returning an int */
int f(int);

/* some more params, and even ellipsis */
T *f(int x, short, char *, ...);

#define MY_EXTERN /*__declspec(dllexport)*/
#define MY_CALL __attribute__((__stdcall__))/*__stdcall*/

/* some #defines in front, as is pretty common */
MY_EXTERN MY_CALL TT1 f7(void);

/* wrapped */
int f(int a, int b,
      int c, int d);

/* taking a procptr param */
void f(void (*foo)(void));

/* sub */
void test1();

void test2(int a, float *b, TT c, struct T **d);

static int f(void);

extern int f(void);

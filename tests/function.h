int f1();

/* function as any ptr */
void *f2();

/* taking an int, returning an int */
int f3(int);

/* some more params, and even ellipsis */
int *f4(int x, short y, char *z, ...);

/* typedef */
TT1 *f5();

/* struct */
struct T1 f6(struct T1 *, TT1 ******);

#define MY_EXTERN /*__declspec(dllexport)*/
#define MY_CALL __attribute__((__stdcall__))/*__stdcall*/

/* some #defines in front, as is pretty common */
MY_EXTERN MY_CALL TT1 f7();

/* wrapped */
int f8(int a, int b,
       int c, int d);

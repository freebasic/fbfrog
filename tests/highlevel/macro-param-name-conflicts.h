typedef int HWND;
#define m1(hwnd) f((HWND)hwnd)
#define m2(and) (and & and)
#define m3(cast) ((HWND)cast)
#define m4(cptr) ((int*)cptr)
#define m5(long) ((int*)long)
#define m6(integer) ((ssize_t*)integer)
#define m7(_) _

#define EXPANDME1(x) void x(foo)(void)
#define EXPANDME2(x) x

EXPANDME1(EXPANDME2);
void EXPANDME2(foo)(void);
void foo(void);

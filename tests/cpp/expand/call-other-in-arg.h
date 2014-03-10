// Other macro called in arg
#define EXPANDME1(a) void a(void);
#define EXPANDME2 foo
// void foo(void);
EXPANDME1(EXPANDME2)

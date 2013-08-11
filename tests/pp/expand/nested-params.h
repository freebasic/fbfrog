#define EXPANDME1(a) void a(void);
#define EXPANDME2 foo
EXPANDME1(EXPANDME2)

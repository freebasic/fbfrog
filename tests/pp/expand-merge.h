#define EXPANDME1(a) void foo##a(void);
EXPANDME1(bar)
EXPANDME1(baz)
#undef EXPANDME1

#define EXPANDME1(a) void foo ##a(void);
EXPANDME1(bar)
EXPANDME1(baz)
#undef EXPANDME1

#define EXPANDME1(a) void foo## a(void);
EXPANDME1(bar)
EXPANDME1(baz)
#undef EXPANDME1

#define EXPANDME1(a) void foo ## a(void);
EXPANDME1(bar)
EXPANDME1(baz)
#undef EXPANDME1

#define EXPANDME1(a) void foo      ##                  a(void);
EXPANDME1(bar)
EXPANDME1(baz)
#undef EXPANDME1

#define EXPANDME1(a,b) void a##b(void);
EXPANDME1(test, one)
#undef EXPANDME1

#define EXPANDME1 void a##b(void);
EXPANDME1
#undef EXPANDME1

#define EXPANDME1(b) void a##b##c(void);
EXPANDME1(x)
#undef EXPANDME1

#define EXPANDME1(a) void a##b##c(void);
EXPANDME1(x)
#undef EXPANDME1

#define EXPANDME1(c) void a##b##c(void);
EXPANDME1(x)
#undef EXPANDME1

#define EXPANDME1(a,c) void a##b##c(void);
EXPANDME1(x,y)
#undef EXPANDME1

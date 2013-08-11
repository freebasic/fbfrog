#define EXPANDME1(a) void foo##a(void);
EXPANDME1(bar)
EXPANDME1(baz)

#define EXPANDME2(a) void foo ##a(void);
EXPANDME2(bar)
EXPANDME2(baz)

#define EXPANDME3(a) void foo## a(void);
EXPANDME3(bar)
EXPANDME3(baz)

#define EXPANDME4(a) void foo ## a(void);
EXPANDME4(bar)
EXPANDME4(baz)

#define EXPANDME5(a) void foo      ##                  a(void);
EXPANDME5(bar)
EXPANDME5(baz)

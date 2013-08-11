#define EXPANDME1(a) a
EXPANDME1(void f(void));
EXPANDME1(int f(int, int));

#define EXPANDME2(a) void a
EXPANDME2(bar(void);)
EXPANDME2(baz(void);)

#define EXPANDME3(a) a(void);
EXPANDME3(void bar)
EXPANDME3(void baz)

#define EXPANDME4(a) void a(void);
EXPANDME4(bar)
EXPANDME4(baz)

#define EXPANDME5(a,b,c) a b c
EXPANDME5(void, f, (void));

#define EXPANDME6(a,b,c) c b a
EXPANDME6((void), f, void);

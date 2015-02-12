// @fbfrog -removedefine m

#define m(a) a
m(void f(void));
m(int f(int, int));
#undef m

#define m(a) void a
m(bar(void);)
m(baz(void);)
#undef m

#define m(a) a(void);
m(void bar)
m(void baz)
#undef m

#define m(a) void a(void);
m(bar)
m(baz)
#undef m

#define m(a,b,c) a b c
m(void, f, (void));
#undef m

#define m(a,b,c) c b a
m((void), f, void);
#undef m

#define m1(...) void f(__VA_ARGS__)
#define m2(args...) void f(args)
m1();
m2();
m1(int);
m2(int);
m1(int, int);
m2(int, int);
m1(float, double, int, int);
m2(float, double, int, int);
#undef m1
#undef m2

#define m1(...) void f(void (*p) __VA_ARGS__)
#define m2(args...) void f(void (*p) args)
m1(());
m2(());
m1((int));
m2((int));
m1((int, int));
m2((int, int));
#undef m1
#undef m2

#define m1(a, ...) void a(__VA_ARGS__)
#define m2(a, args...) void a(args)
m1(f);
m2(f);
m1(f, int);
m2(f, int);
#undef m1
#undef m2

#define m1(a, b, ...) a b(__VA_ARGS__)
#define m2(a, b, args...) a b(args)
m1(void, f);
m2(void, f);
m1(void, f, int);
m2(void, f, int);
#undef m1
#undef m2

#define m1(a, ...) void a(__VA_ARGS__)
m1(x, int);
m1(x);
m1(x, int, int);
m1(x, int);
m1(x);
#undef m1

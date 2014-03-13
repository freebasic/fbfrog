// @fbfrog -whitespace -nonamefixup -removedefine m

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

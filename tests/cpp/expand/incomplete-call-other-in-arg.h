// @fbfrog -removedefine m1

#define m1(x) void x(foo)(void)
#define m2(x) x

m1(m2);
void m2(foo)(void);
void foo(void);

// @fbfrog -removedefine m1 -removedefine m2

// Other macro called in arg
#define m1(a) void a(void);
#define m2 foo
m1(m2)

void foo(void);

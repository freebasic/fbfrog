#define m1(x) m2
#define m2(x) x

m1(0)(static int a;)
m2(static int b;)
static int c;

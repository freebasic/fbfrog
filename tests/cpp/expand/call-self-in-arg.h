// @fbfrog -removedefine m1

// Self called in arg (ok, unlike recursion)
#define m1(x) x
m1(m1(static int i;))
static int i;

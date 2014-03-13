// @fbfrog -removedefine m1 -removedefine m2

#define m1(x) static int i;
#define m2 (void)

// m1: incomplete macro call, not expanded
void m1 m2;
// void m1 (void);

// @fbfrog -removedefine m1

// Incomplete self call in arg (not expanded in arg because it's incomplete,
// not expanded in body later because it's a recursive call)
#define m1(x) x(void)
void m1(m1);
// void m1(void);

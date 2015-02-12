// @fbfrog -removedefine m2

// Incomplete recursive macro call, not expanded
#define m1(x) m1
#define m2 (void)
void m1(void) m2;
// void m1 (void);

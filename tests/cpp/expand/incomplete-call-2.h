// @fbfrog -removedefine m3

#define m1 m2
#define m2(x) x
#define m3 (void)

// m2: incomplete macro call, not expanded
void m1 m3;
// void m2 m3;
// void m2 (void);

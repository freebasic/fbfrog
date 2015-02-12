// @fbfrog -removedefine m1 -removedefine m2

// Other macro called in body
#define m1 void m2(void);
#define m2 foo
m1
void foo(void);

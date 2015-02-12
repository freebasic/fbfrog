// Indirect recursion with params
#define m1(x) m2(x)
#define m2(x) m1(x)
void m1(void); // -> m2(void) -> m1(void)
void m2(void); // -> m1(void) -> m2(void)

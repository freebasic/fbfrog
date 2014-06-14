#define m1 __VA_ARGS__
#define m2(x) x##__VA_ARGS__

void __VA_ARGS__(void);
void m1(void);
void m2(aaa)(void);

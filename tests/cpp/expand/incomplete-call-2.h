// Incomplete macro call, not expanded
#define EXPANDME1 EXPANDME2
#define EXPANDME2(x) x
#define EXPANDME3 (void)
void EXPANDME1 EXPANDME3;
// void EXPANDME2 EXPANDME3;
// void EXPANDME2 (void);

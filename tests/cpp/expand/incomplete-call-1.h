// Incomplete macro call, not expanded
#define EXPANDME1(x) static int i;
#define EXPANDME2 (void)
// void EXPANDME1 (void);
void EXPANDME1 EXPANDME2;

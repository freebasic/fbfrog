// Incomplete recursive macro call, still not expanded
#define EXPANDME1(x) EXPANDME1
#define EXPANDME2 (void)
// void EXPANDME1 (void);
void EXPANDME1(void) EXPANDME2;

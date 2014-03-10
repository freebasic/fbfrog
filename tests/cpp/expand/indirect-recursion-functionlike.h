// Indirect recursion with params
#define EXPANDME1(x) EXPANDME2(x)
#define EXPANDME2(x) EXPANDME1(x)
void EXPANDME1(void); // -> EXPANDME2(void) -> EXPANDME1(void)
void EXPANDME2(void); // -> EXPANDME1(void) -> EXPANDME2(void)

// Incomplete self call in arg
#define EXPANDME1(x) x(void)
// void EXPANDME1(void);
void EXPANDME1(EXPANDME1);

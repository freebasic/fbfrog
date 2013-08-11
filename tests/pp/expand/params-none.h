#define EXPANDME1 void f(void);
EXPANDME1

// The "(...)" following EXPANDME2 is not an argument list
#define EXPANDME2 foo
void EXPANDME2(void);

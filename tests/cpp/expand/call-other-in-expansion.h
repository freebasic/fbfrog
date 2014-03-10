// Other macro called in body
#define EXPANDME1 void EXPANDME2(void);
#define EXPANDME2 foo
// void foo(void);
EXPANDME1

#define EXPANDME1 void f(void);
EXPANDME1

// The "(...)" following EXPANDME2 is not an argument list
#define EXPANDME2 foo
void EXPANDME2(void);

// The "(...)" following the macro id isn't a parameter list, as it's separated
// by spaces
#define EXPANDME3 (void)
void f EXPANDME3;

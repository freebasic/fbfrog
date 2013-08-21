#define EXPANDME1() void f(void);
EXPANDME1()

// Missing "()"
EXPANDME1

// Too many arguments
EXPANDME1(a)
EXPANDME1(a,b)

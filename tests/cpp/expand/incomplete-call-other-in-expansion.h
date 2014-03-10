#define EXPANDME1(x) EXPANDME2
#define EXPANDME2(x) x
EXPANDME1(0)(static int a;)
EXPANDME2(static int b;)
static int c;

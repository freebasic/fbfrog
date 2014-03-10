// Self called in arg (ok, unlike recursion)
#define EXPANDME1(x) x
// static int i;
EXPANDME1(EXPANDME1(static int i;))

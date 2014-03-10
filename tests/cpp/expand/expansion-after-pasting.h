// Expansion after pasting
#define EXPANDME1(x) EXPAND##x
#define EXPANDME2 x2
#define EXPANDME3 x3
static int EXPANDME1(ME2); // -> EXPANDME2 -> x2
static int EXPANDME1(ME3); // -> EXPANDME2 -> x2

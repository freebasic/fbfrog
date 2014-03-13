// Expansion after pasting
#define m1(x) m##x
#define m2 x2
#define m3 x3
static int m1(2); // -> m2 -> x2
static int m1(3); // -> m3 -> x3

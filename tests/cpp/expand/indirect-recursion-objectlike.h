// Indirect recursion
#define m1 m2
#define m2 m1
static int m1; // -> m2 -> m1
static int m2; // -> m1 -> m2

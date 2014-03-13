// @fbfrog -removedefine m1
// Recursion after pasting
#define m1 m##1
static int m1; // -> m1

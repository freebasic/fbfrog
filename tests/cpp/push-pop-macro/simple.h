#define A 1
static int x = A;

#pragma push_macro("A")
#undef A
#define A 2
static int y = A;

#pragma pop_macro("A")
static int z = A;

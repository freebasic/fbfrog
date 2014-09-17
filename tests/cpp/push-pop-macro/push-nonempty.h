#define A 123
#pragma push_macro("A")
static int x = A;

#pragma pop_macro("A")
static int y = A;

#define A 123

#pragma push_macro("A")

// Temporarily redefined with different body
#undef A
#define A 456
static int x = A;

#pragma pop_macro("A")
static int y = A;

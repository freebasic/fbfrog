#define A 123

#pragma push_macro("A")

// Temporarily redefined with empty body
#undef A
#define A
A static int x;

#pragma pop_macro("A")
static int y = A;

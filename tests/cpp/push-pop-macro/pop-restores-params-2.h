#define A(x)

#pragma push_macro("A")

// Temporarily redefined without parameter
// (not even with different body, just without the parameter!)
#undef A
#define A
A static int x; // A without arguments should be expanded now

#pragma pop_macro("A")
static int y = A; // A without arguments shouldn't be expanded

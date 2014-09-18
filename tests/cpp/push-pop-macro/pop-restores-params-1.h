#define A

#pragma push_macro("A")

// Temporarily redefined with parameter
// (not even with different body, just the parameter!)
#undef A
#define A(x)
A(0) static int x = A; // A without arguments shouldn't be expanded anymore

#pragma pop_macro("A")
A static int y; // A without arguments should be expanded again

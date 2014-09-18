#define A(x)

#pragma push_macro("A")

// Temporarily redefined with different parameters
// (not even with different body, just different parameters!)
#undef A
#define A(x, y)
A(0, 0) static int x; // Using A should require 2 arguments now

#pragma pop_macro("A")
A(0) static int y; // Using A should require 1 argument now

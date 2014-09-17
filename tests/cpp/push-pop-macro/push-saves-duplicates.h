#define A 1

// Save A 1
#pragma push_macro("A")

#undef A
#define A 2

// Save A 2
#pragma push_macro("A")

// Save A 2 again
#pragma push_macro("A")
static int a_2 = A;

#undef A
#define A 3
static int b_3 = A;

// Restore to second A 2
#pragma pop_macro("A")
static int c_2 = A;

// Restore to first A 2
#pragma pop_macro("A")
static int d_2 = A;

// Restore to A 1
#pragma pop_macro("A")
static int e_1 = A;

#define A 1
static int a_1 = A;

#pragma push_macro("A")
#undef A
#define A 2
static int b_2 = A;

#pragma push_macro("A")
#undef A
#define A 3
static int c_3 = A;

#pragma pop_macro("A")
static int d_2 = A;

#pragma pop_macro("A")
static int e_1 = A;

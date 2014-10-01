_Pragma("pack(1)")
struct PackedStruct {
	char byte;
	int dword;
};

_Pragma("pack()")
struct NormalStruct {
	char byte;
	int dword;
};

#define A 1
static int a_1 = A;
_Pragma("push_macro(\"A\")")
static int b_1 = A;
#undef A
#define A 2
static int c_2 = A;
_Pragma("pop_macro(\"A\")")
static int d_1 = A;

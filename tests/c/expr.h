enum E {
	// UOPs
	A = !A,
	A = ~A,
	A = -A,
	A = +A,
	A = &A,
	A = *A,
	A = sizeof A,
	A = sizeof(A),

	// atoms
	A = (A),
	A = ((A)),
	A = (void)0,
	A = (char)0,
	A = (float)0,
	A = (double)0,
	A = (int)0,
	A = (signed int)0,
	A = (unsigned int)0,
	A = (const int)0,
	A = (short)0,
	A = (long)0,
	A = (enum E)0,
	A = (struct UDT)0,
	A = (union UDT)0,
	A = (__stdcall void (*)(void))0,
	A = (__cdecl void (*)(void))0,
	A = (__attribute__((stdcall)) void (*)(void))0,
	A = (void *)0,
	A = (int *)0,
	A = (struct UDT *)0,
	A = (union UDT *)0,
	A = 1,
	A = 01,
	A = 0x1,
	A = "a",
	A = L"a",
	A = 'a',
	A = L'a',
	A = A,
	A = f(A),
	A = f(1,2,3,4),
	A = f(),

	// infix
	A = a ? b : c,
	A = a || b,
	A = a && b,
	A = a | b,
	A = a ^ b,
	A = a & b,
	A = a == b,
	A = a != b,
	A = a < b,
	A = a <= b,
	A = a > b,
	A = a >= b,
	A = a << b,
	A = a >> b,
	A = a + b,
	A = a - b,
	A = a * b,
	A = a / b,
	A = a % b,
	A = a[b],
	A = a.b,
	A = a->b,
};

#define A(x) "a" "b"
#define A(x) "a" "b" "c" "d"
#define A(x) "a""b"
#define A(x) "a" #x "b"
#define A(x) "a"#x"b"#x"c"

#define A (void)0
#define A (char)0
#define A (float)0
#define A (double)0
#define A (int)0
#define A (signed int)0
#define A (unsigned int)0
#define A (const int)0
#define A (short)0
#define A (long)0
#define A (enum E)0
#define A (struct UDT)0
#define A (union UDT)0
#define A (__stdcall void (*)(void))0
#define A (__cdecl void (*)(void))0
#define A (__attribute__((stdcall)) void (*)(void))0
#define A (void *)0
#define A (int *)0
#define A (struct UDT *)0
#define A (union UDT *)0

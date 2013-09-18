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

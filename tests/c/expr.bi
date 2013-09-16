enum E
	A = sizeof( A )
	A = sizeof( A )
	A = f(A)
	A = f(1, 2, 3, 4)
	A = f()
end enum

#define A(x) "a" + "b"
#define A(x) (("a" + "b") + "c") + "d"
#define A(x) "a" + "b"
#define A(x) ("a" + #x) + "b"
#define A(x) ((("a" + #x) + "b") + #x) + "c"

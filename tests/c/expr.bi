enum E
	A = -(A = 0)
	A = not A
	A = -A
	A = A
	A = @A
	A = *A
	A = sizeof( A )
	A = sizeof( A )
	A = A
	A = A
	A = 1
	A = &o1
	A = &h1
	A = "a"
	A = wstr( "a" )
	A = asc( "a" )
	A = asc( wstr( "a" ) )
	A = A
	A = f(A)
	A = f(1, 2, 3, 4)
	A = f()
	A = iif( a, b, c )
	A = -(a orelse b)
	A = -(a andalso b)
	A = a or b
	A = a xor b
	A = a and b
	A = -(a = b)
	A = -(a <> b)
	A = -(a < b)
	A = -(a <= b)
	A = -(a > b)
	A = -(a >= b)
	A = a shl b
	A = a shr b
	A = a + b
	A = a - b
	A = a * b
	A = a / b
	A = a mod b
	A = a[b]
	A = a.b
	A = a->b
end enum

#define A(x) ("a" + "b")
#define A(x) ((("a" + "b") + "c") + "d")
#define A(x) ("a" + "b")
#define A(x) (("a" + #x) + "b")
#define A(x) (((("a" + #x) + "b") + #x) + "c")

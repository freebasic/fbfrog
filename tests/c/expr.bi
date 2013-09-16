enum E
	A = sizeof( A )
	A = sizeof( A )
end enum

#define A( x ) "a" + "b"
#define A( x ) (("a" + "b") + "c") + "d"
#define A( x ) "a" + "b"
#define A( x ) ("a" + #x) + "b"
#define A( x ) ((("a" + #x) + "b") + #x) + "c"

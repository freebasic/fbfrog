'' @fbfrog -whitespace -nonamefixup

type MYINT as long

enum E
	A = A
	A = A
	A = cast(any, 0)
	A = cast(zstring, 0)
	A = cast(single, 0)
	A = cast(double, 0)
	A = clng(0)
	A = clng(0)
	A = culng(0)
	A = cast(const long, 0)
	A = cshort(0)
	A = cast(clong, 0)
	A = cast(E, 0)
	A = cast(UDT, 0)
	A = cast(UDT, 0)
	A = cptr(sub stdcall(), 0)
	A = cptr(sub cdecl(), 0)
	A = cptr(sub stdcall(), 0)
	A = cptr(any ptr, 0)
	A = cptr(long ptr, 0)
	A = cptr(UDT ptr, 0)
	A = cptr(UDT ptr, 0)
	A = cast(wstring, 0)
	A = cuint(0)
	A = cint(0)
	A = cint(0)
	A = cbyte(0)
	A = cubyte(0)
	A = cshort(0)
	A = cushort(0)
	A = clng(0)
	A = culng(0)
	A = clngint(0)
	A = culngint(0)
	A = cbyte(0)
	A = cshort(0)
	A = clng(0)
	A = clngint(0)
	A = cast(MYINT, 0)
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
	A = -(A = 0)
	A = not A
	A = -A
	A = A
	A = @A
	A = *A
	A = sizeof( A )
	A = sizeof( A )
	A = sizeof(long)
	A = sizeof(long) * 2
	A = sizeof(long) shl 1
	A = sizeof(MYINT)
	A = sizeof(MYINT) * 2
	A = sizeof(MYINT) shl 1
	A = sizeof(sub stdcall())
	A = -(a = 0)
	A = not a
	A = -a
	A = a
	A = -(a = 0)
	A = not a
	A = -a
	A = a
	A = -(a = 0)
	A = not a
	A = -a
	A = a
	A = -(a <> 0)
	A = a
	A = a
	A = a
	A = -(a orelse b)
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
	A = -(a orelse b)
	A = -(a orelse b)
	A = -(a orelse b)
	A = -(a andalso b)
	A = -(a andalso b)
	A = -(a andalso b)
	A = a or b
	A = a or b
	A = a or b
	A = a xor b
	A = a xor b
	A = a xor b
	A = a and b
	A = a and b
	A = a and b
	A = -(a = b)
	A = -(a = b)
	A = -(a = b)
	A = -(a <> b)
	A = -(a <> b)
	A = -(a <> b)
	A = -(a < b)
	A = -(a < b)
	A = -(a < b)
	A = -(a <= b)
	A = -(a <= b)
	A = -(a <= b)
	A = -(a > b)
	A = -(a > b)
	A = -(a > b)
	A = -(a >= b)
	A = -(a >= b)
	A = -(a >= b)
	A = a shl b
	A = a shl b
	A = a shl b
	A = a shr b
	A = a shr b
	A = a shr b
	A = a + b
	A = a + b
	A = a + b
	A = a - b
	A = a - b
	A = a - b
	A = a * b
	A = a * b
	A = a * b
	A = a / b
	A = a / b
	A = a / b
	A = a mod b
	A = a mod b
	A = a mod b
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
	A = -((a orelse b) orelse c)
	A = -(a orelse (b orelse c))
	A = -((a orelse b) orelse c)
	A = -(a orelse (b andalso c))
	A = -(a orelse (b andalso c))
	A = -((a orelse b) andalso c)
	A = -((a andalso b) orelse c)
	A = -(a andalso (b orelse c))
	A = -((a andalso b) orelse c)
	A = -((a andalso b) andalso c)
	A = -(a andalso (b andalso c))
	A = -((a andalso b) andalso c)
	A = -(a andalso (b or c))
	A = -(a andalso (b or c))
	A = (-(a andalso b)) or c
	A = -((a or b) andalso c)
	A = a or (-(b andalso c))
	A = -((a or b) andalso c)
	A = (a or b) or c
	A = a or (b or c)
	A = (a or b) or c
	A = a or (b xor c)
	A = a or (b xor c)
	A = (a or b) xor c
	A = (a xor b) or c
	A = a xor (b or c)
	A = (a xor b) or c
	A = (a xor b) xor c
	A = a xor (b xor c)
	A = (a xor b) xor c
	A = a xor (b and c)
	A = a xor (b and c)
	A = (a xor b) and c
	A = (a and b) xor c
	A = a and (b xor c)
	A = (a and b) xor c
	A = (a and b) and c
	A = a and (b and c)
	A = (a and b) and c
	A = a and (-(b = c))
	A = a and (-(b = c))
	A = -((a and b) = c)
	A = (-(a = b)) and c
	A = -(a = (b and c))
	A = (-(a = b)) and c
	A = -((-(a = b)) = c)
	A = -(a = (-(b = c)))
	A = -((-(a = b)) = c)
	A = -((-(a = b)) <> c)
	A = -(a = (-(b <> c)))
	A = -((-(a = b)) <> c)
	A = -((-(a <> b)) = c)
	A = -(a <> (-(b = c)))
	A = -((-(a <> b)) = c)
	A = -((-(a <> b)) <> c)
	A = -(a <> (-(b <> c)))
	A = -((-(a <> b)) <> c)
	A = -(a <> (-(b < c)))
	A = -(a <> (-(b < c)))
	A = -((-(a <> b)) < c)
	A = -((-(a < b)) <> c)
	A = -(a < (-(b <> c)))
	A = -((-(a < b)) <> c)
	A = -((-(a < b)) < c)
	A = -(a < (-(b < c)))
	A = -((-(a < b)) < c)
	A = -((-(a < b)) <= c)
	A = -(a < (-(b <= c)))
	A = -((-(a < b)) <= c)
	A = -((-(a <= b)) < c)
	A = -(a <= (-(b < c)))
	A = -((-(a <= b)) < c)
	A = -((-(a <= b)) <= c)
	A = -(a <= (-(b <= c)))
	A = -((-(a <= b)) <= c)
	A = -((-(a <= b)) > c)
	A = -(a <= (-(b > c)))
	A = -((-(a <= b)) > c)
	A = -((-(a > b)) <= c)
	A = -(a > (-(b <= c)))
	A = -((-(a > b)) <= c)
	A = -((-(a > b)) > c)
	A = -(a > (-(b > c)))
	A = -((-(a > b)) > c)
	A = -((-(a > b)) >= c)
	A = -(a > (-(b >= c)))
	A = -((-(a > b)) >= c)
	A = -((-(a >= b)) > c)
	A = -(a >= (-(b > c)))
	A = -((-(a >= b)) > c)
	A = -((-(a >= b)) >= c)
	A = -(a >= (-(b >= c)))
	A = -((-(a >= b)) >= c)
	A = -(a >= (b shl c))
	A = -(a >= (b shl c))
	A = (-(a >= b)) shl c
	A = -((a shl b) >= c)
	A = a shl (-(b >= c))
	A = -((a shl b) >= 1)
	A = (a shl b) shl c
	A = a shl (b shl c)
	A = (a shl b) shl c
	A = (a shl b) shr c
	A = a shl (b shr c)
	A = (a shl b) shr c
	A = (a shr b) shl c
	A = a shr (b shl c)
	A = (a shr b) shl c
	A = (a shr b) shr c
	A = a shr (b shr c)
	A = (a shr b) shr c
	A = a shr (b + c)
	A = a shr (b + c)
	A = (a shr b) + c
	A = (a + b) shr c
	A = a + (b shr c)
	A = (a + b) shr c
	A = (a + b) + c
	A = a + (b + c)
	A = (a + b) + c
	A = (a + b) - c
	A = a + (b - c)
	A = (a + b) - c
	A = (a - b) + c
	A = a - (b + c)
	A = (a - b) + c
	A = (a - b) - c
	A = a - (b - c)
	A = (a - b) - 1
	A = a - (b * c)
	A = a - (b * c)
	A = (a - b) * c
	A = (a * b) - c
	A = a * (b - c)
	A = (a * b) - c
	A = (a * b) * c
	A = a * (b * c)
	A = (a * b) * c
	A = (a * b) / c
	A = a * (b / c)
	A = (a * b) / c
	A = (a / b) * c
	A = a / (b * c)
	A = (a / b) * c
	A = (a / b) / c
	A = a / (b / c)
	A = (a / b) / c
	A = (a / b) mod c
	A = a / (b mod c)
	A = (a / b) mod c
	A = (a mod b) / c
	A = a mod (b / c)
	A = (a mod b) / c
	A = (a mod b) mod c
	A = a mod (b mod c)
	A = (a mod b) mod c
	A = -((a andalso (b + c)) andalso (d = e))
	A = iif( a, b, c )
	A = iif( a, b, iif( c, d, e ) )
	A = iif( a, b, iif( c, d, e ) )
	A = iif( iif( a, b, c ), d, e )
	A = (1, 2, 3)
	A = ()
	A = (1)
	A = (1)
	A = (-(a = 0))
	A = f(-(a = 0))
end enum

#define A(x) ("a" + "b")
#define A(x) ((("a" + "b") + "c") + "d")
#define A(x) ("a" + "b")
#define A(x) (("a" + #x) + "b")
#define A(x) (((("a" + #x) + "b") + #x) + "c")

#define A cast(any, 0)
#define A cast(zstring, 0)
#define A cast(single, 0)
#define A cast(double, 0)
#define A clng(0)
#define A clng(0)
#define A culng(0)
#define A cast(const long, 0)
#define A cshort(0)
#define A cast(clong, 0)
#define A cast(E, 0)
#define A cast(UDT, 0)
#define A cast(UDT, 0)
#define A cptr(sub stdcall(), 0)
#define A cptr(sub cdecl(), 0)
#define A cptr(sub stdcall(), 0)
#define A cptr(any ptr, 0)
#define A cptr(long ptr, 0)
#define A cptr(UDT ptr, 0)
#define A cptr(UDT ptr, 0)
#define A cast(wstring, 0)
#define A cuint(0)
#define A cint(0)
#define A cint(0)
#define A cbyte(0)
#define A cubyte(0)
#define A cshort(0)
#define A cushort(0)
#define A clng(0)
#define A culng(0)
#define A clngint(0)
#define A culngint(0)
#define A cbyte(0)
#define A cshort(0)
#define A clng(0)
#define A clngint(0)
#define A cast(MYINT, 0)

#macro A
	scope
		a(0)
		b(1)
	end scope
#endmacro
#define A (1, 2, 3)
#define A ()
#define A (1)
#define A (1)

#define A (-(a = 0), -(a = 0))
#macro A
	scope
		f(-(a = 0))
	end scope
#endmacro

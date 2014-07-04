#pragma once

#include once "crt/long.bi"

type UDT as UDT_

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
	A = wstr("a")
	A = asc("a")
	A = asc(wstr("a"))
	A = A
	A = f(A)
	A = f(1, 2, 3, 4)
	A = f()
	A = -(A = 0)
	A = not A
	A = -A
	A = +A
	A = @A
	A = *A
	A = sizeof(A)
	A = sizeof(A)
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
	A = +a
	A = -(a = 0)
	A = not a
	A = -a
	A = +a
	A = -(a = 0)
	A = not a
	A = -a
	A = +a
	A = -((a = 0) = 0)
	A = not (not a)
	A = -(-a)
	A = +(+a)
	A = -(a orelse (-(-b)))
	A = -(a orelse (-(-b)))
	A = -(a andalso (-(-b)))
	A = a or (-(-b))
	A = a xor (-(-b))
	A = a and (-(-b))
	A = -(a = (-(-b)))
	A = -(a <> (-(-b)))
	A = -(a < (-(-b)))
	A = -(a <= (-(-b)))
	A = -(a > (-(-b)))
	A = -(a >= (-(-b)))
	A = a shl (-(-b))
	A = a shr (-(-b))
	A = a + (-(-b))
	A = a - (-(-b))
	A = a * (-(-b))
	A = a / (-(-b))
	A = a mod (-(-b))
	A = -((-(-a)) orelse b)
	A = -(-(-(a orelse b)))
	A = -((-(-a)) orelse b)
	A = -((-(-a)) andalso b)
	A = -(-(-(a andalso b)))
	A = -((-(-a)) andalso b)
	A = (-(-a)) or b
	A = -(-(a or b))
	A = (-(-a)) or b
	A = (-(-a)) xor b
	A = -(-(a xor b))
	A = (-(-a)) xor b
	A = (-(-a)) and b
	A = -(-(a and b))
	A = (-(-a)) and b
	A = -((-(-a)) = b)
	A = -(-(-(a = b)))
	A = -((-(-a)) = b)
	A = -((-(-a)) <> b)
	A = -(-(-(a <> b)))
	A = -((-(-a)) <> b)
	A = -((-(-a)) < b)
	A = -(-(-(a < b)))
	A = -((-(-a)) < b)
	A = -((-(-a)) <= b)
	A = -(-(-(a <= b)))
	A = -((-(-a)) <= b)
	A = -((-(-a)) > b)
	A = -(-(-(a > b)))
	A = -((-(-a)) > b)
	A = -((-(-a)) >= b)
	A = -(-(-(a >= b)))
	A = -((-(-a)) >= b)
	A = (-(-a)) shl b
	A = -(-(a shl b))
	A = (-(-a)) shl b
	A = (-(-a)) shr b
	A = -(-(a shr b))
	A = (-(-a)) shr b
	A = (-(-a)) + b
	A = -(-(a + b))
	A = (-(-a)) + b
	A = (-(-a)) - b
	A = -(-(a - b))
	A = (-(-a)) - b
	A = (-(-a)) * b
	A = -(-(a * b))
	A = (-(-a)) * b
	A = (-(-a)) / b
	A = -(-(a / b))
	A = (-(-a)) / b
	A = (-(-a)) mod b
	A = -(-(a mod b))
	A = (-(-a)) mod b
	A = iif(a, b, c)
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
	A = iif(a, b, c)
	A = iif(a, b, iif(c, d, e))
	A = iif(a, b, iif(c, d, e))
	A = iif(iif(a, b, c), d, e)
	A = (1, 2, 3)
	A = ()
	A = (1)
	A = (1)
	A = (-(a = 0))
	A = f(-(a = 0))
end enum

#define A01(x) ("a" + "b")
#define A02(x) ((("a" + "b") + "c") + "d")
#define A03(x) ("a" + "b")
#define A04(x) (("a" + #x) + "b")
#define A05(x) (((("a" + #x) + "b") + #x) + "c")

const A06 = cast(any, 0)
const A07 = cast(zstring, 0)
const A08 = cast(single, 0)
const A09 = cast(double, 0)
const A10 = clng(0)
const A11 = clng(0)
const A12 = culng(0)
const A13 = cast(const long, 0)
const A14 = cshort(0)
const A15 = cast(clong, 0)
const A16 = cast(E, 0)
const A17 = cast(UDT, 0)
const A18 = cast(UDT, 0)
const A19 = cptr(sub stdcall(), 0)
const A20 = cptr(sub cdecl(), 0)
const A21 = cptr(sub stdcall(), 0)
const A22 = cptr(any ptr, 0)
const A23 = cptr(long ptr, 0)
const A24 = cptr(UDT ptr, 0)
const A25 = cptr(UDT ptr, 0)
const A26 = cast(wstring, 0)
const A27 = cuint(0)
const A28 = cint(0)
const A29 = cint(0)
const A30 = cbyte(0)
const A31 = cubyte(0)
const A32 = cshort(0)
const A33 = cushort(0)
const A34 = clng(0)
const A35 = culng(0)
const A36 = clngint(0)
const A37 = culngint(0)
const A38 = cbyte(0)
const A39 = cshort(0)
const A40 = clng(0)
const A41 = clngint(0)
const A42 = cast(MYINT, 0)

#macro A43
	scope
		a(0)
		b(1)
	end scope
#endmacro
#define A44 (1, 2, 3)
#define A45 ()
#define A46 (1)
#define A47 (1)

#define A48 (-(a = 0), -(a = 0))
#macro A49
	scope
		f(-(a = 0))
	end scope
#endmacro

#define A50 defined(FOO)
#define A51(a) defined(a)

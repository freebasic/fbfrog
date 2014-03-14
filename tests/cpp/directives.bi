'' @ignore

#define INCLUDED

'' @ignore

#define INCLUDED

'' @ignore

#define INCLUDED

'' @ignore

#define INCLUDED

'' @ignore

#define INCLUDED

'' @ignore

#define INCLUDED

'' @ignore

#define INCLUDED

'' @ignore

#define INCLUDED
const A01 = 0
const A02 = 1
const A03 = 11
const A04 = &o1
const A05 = &o123
const A06 = &h0
const A07 = &h0
const A08 = &h1
const A09 = &hFF
const A10 = 1
const A11 = 1
const A12 = 1
const A13 = 0.1
const A14 = 0
const A15 = 1.123
const A16 = 10
const A17 = 10
const A18 = 1
const A19 = 1
const A20 = 1
const A21 = 1
const A22 = 1
const B01 = 2
const B02 = 1
#define A
#define A
#define A foo
#define A
#define m(a)
#define m(a)
#define m(a, b)
#define m(a, b)
#define m(a, b)
#define m(foo, abcdefg, something, bar, buzzzz)
#define m(a) foo
#define m(a) foo
#define m(a) a##foo
#define m(a) foo##a
#define m(a) foo##a##foo
#define m(a, b) a##b
#define m(a, b) a##b##foo
#define m(a, b) a##foo##b
#define m(a, b) a##foo##b##foo
#define m(a, b) foo##a##b
#define m(a, b) foo##a##b##foo
#define m(a, b) foo##a##foo##b
#define m(a, b) foo##a##foo##b##foo
#define m(a) #a
#define no_parameters_here a
dim shared separator1 as long

#macro A
	scope
		f(123)
	end scope
#endmacro
#macro A(x)
	scope
		f(x)
	end scope
#endmacro
#macro A(x)
	scope
		f1(x + 1)
		f2(x + 2)
		f3(x + 3)
	end scope
#endmacro

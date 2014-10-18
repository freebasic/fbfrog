#pragma once

#include once "crt/long.bi"

'' @ignore

dim shared included as long

'' @ignore

dim shared included as long

'' @ignore

dim shared included as long

'' @ignore

dim shared included as long

'' @ignore

dim shared included as long

'' @ignore

dim shared included as long

'' @ignore

dim shared included as long

'' @ignore

dim shared included as long

#define A01 0
#define A02 1
#define A03 11
#define A04 &o1
#define A05 &o123
#define A06 &h
#define A07 &h0
#define A08 &h1
#define A09 &hFF
#define A10 1.0
#define A11 1.0f
#define A12 1.0
#define A13 0.1
#define A14 0.0
#define A15 1.123
#define A16 1e+1
#define A17 1.0e+1
#define A18 1
#define A19 cast(clong, 1)
#define A20 cast(culong, 1)
#define A21 1ll
#define A22 1ull

#define B01 (1 + 1)
#define B02 1

#define C1  '' no space following

#define C2  '' trailing space
#define C3 foo
#define C4  '' foo

#define m01(a)
#define m02(a)
#define m03(a, b)
#define m04(a, b)
#define m05(a, b)
#define m06(foo, abcdefg, something, bar, buzzzz)
#define m07(a) foo
#define m08(a) foo

#define m09(a) a##foo
#define m10(a) foo##a
#define m11(a) foo##a##foo

#define m12(a, b) a##b
#define m13(a, b) a##b##foo
#define m14(a, b) a##foo##b
#define m15(a, b) a##foo##b##foo
#define m16(a, b) foo##a##b
#define m17(a, b) foo##a##b##foo
#define m18(a, b) foo##a##foo##b
#define m19(a, b) foo##a##foo##b##foo

#define m20(a) #a

#define m21(__VA_ARGS__...) f(__VA_ARGS__)
#define m22(a, __VA_ARGS__...) a(__VA_ARGS__)
#define m23(a, __VA_ARGS__...) f(a, __VA_ARGS__)
#define m24(a, b, __VA_ARGS__...) f(a, b, __VA_ARGS__)
#define m25(a, b, args...) f(a, b, args)

#define no_parameters_here a

#macro D1
	scope
		f(123)
	end scope
#endmacro
#macro D2(x)
	scope
		f(x)
	end scope
#endmacro
#macro D3(x)
	scope
		f1((x) + 1)
		f2((x) + 2)
		f3((x) + 3)
	end scope
#endmacro

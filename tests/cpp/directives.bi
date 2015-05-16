#pragma once

#include once "crt/long.bi"

dim shared included as long
dim shared included as long
dim shared included as long
dim shared included as long

const A01 = 0
const A02 = 1
const A03 = 11
const A04 = &o1
const A05 = &o123
const A06 = &h
const A07 = &h0
const A08 = &h1
const A09 = &hFF
const A10 = 1.0
const A11 = 1.0f
const A12 = 1.0
const A13 = 0.1
const A14 = 0.0
const A15 = 1.123
const A16 = 1e+1
const A17 = 1.0e+1
const A18 = 1u
const A19 = cast(clong, 1)
const A20 = cast(culong, 1)
const A21 = 1ll
const A22 = 1ull
const B01 = 1 + 1
const B02 = 1
#define C1
#define C2
#define C3 foo
#define C4
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
#define stringify(s) #s
#define D1 scope : f(123) : end scope
#define D2(x) scope : f(x) : end scope
#macro D3(x)
	scope
		f1((x) + 1)
		f2((x) + 2)
		f3((x) + 3)
	end scope
#endmacro

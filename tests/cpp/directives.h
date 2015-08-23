#include "directives-1.h"
	#include "directives-1.h"
#	include "directives-1.h"
 # include "directives-1.h"

// "empty" PP directive
#
# /* another "empty" with just a comment */
# // ditto

#define A01 0
#define A02 1
#define A03 11
#define A04 01
#define A05 0123
#define A06 0x
#define A07 0x0
#define A08 0x1
#define A09 0xFF
#define A10 1.0
#define A11 1.0f
#define A12 1.0d
#define A13 0.1
#define A14 0.0
#define A15 1.123
#define A16 1e+1
#define A17 1.0e+1
#define A18 1u
#define A19 1l
#define A20 1ul
#define A21 1ll
#define A22 1ull

#define B01 1 + 1
#define B02 ((1))

// no space following
#define C1
// trailing space
#define C2 
#define C3 foo
#define C4 /*foo*/

#define m01(a)
#define m02(a) 
#define m03(a,b)
#define m04(a, b)
#define m05( a , b )
#define m06(foo, abcdefg, something, bar, buzzzz)
#define m07(a) foo
#define m08(a)foo

#define m09(a)      a##foo
#define m10(a) foo##a
#define m11(a) foo##a##foo

#define m12(a, b)        a    ##     b
#define m13(a, b)        a    ##     b ## foo
#define m14(a, b)        a ## foo ## b
#define m15(a, b)        a ## foo ## b ## foo
#define m16(a, b) foo ## a    ##     b
#define m17(a, b) foo ## a    ##     b ## foo
#define m18(a, b) foo ## a ## foo ## b
#define m19(a, b) foo ## a ## foo ## b ## foo

#define m20(a) #a

#define m21(...) f(__VA_ARGS__)
#define m22(a, ...) a(__VA_ARGS__)
#define m23(a, ...) f(a, __VA_ARGS__)
#define m24(a, b, ...) f(a, b, __VA_ARGS__)
#define m25(a, b, args...) f(a, b, args)

#define no_parameters_here (a)

#define stringify(s) #s

#pragma message("test")
#pragma message("a" "b" "c" stringify(this))
#pragma GCC system_header
#pragma GCC push_options
#pragma GCC pop_options
#pragma GCC reset_options
#pragma GCC optimize("O0")
#pragma GCC target("foo")
#pragma GCC system_header
#pragma pack(1)
#pragma pack()

#define D1 { f( 123 ); }
#define D2(x) { f( x ); }
#define D3(x) { f1( (x) + 1 ); f2( (x) + 2 ); f3( (x) + 3 ); }

#warning "Example warning"

#pragma warning(disable : 123)

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wfoo"
#pragma clang diagnostic pop

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfoo"
#pragma GCC diagnostic pop

#pragma GCC visibility push(default)
#pragma GCC visibility pop
#pragma GCC visibility push(hidden)
#pragma GCC visibility pop

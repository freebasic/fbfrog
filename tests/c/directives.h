#include "1"
#include <2>
	#include "3"
	#include <4>
#	include "5"
#	include <6>
 # include "7"
 # include <8>

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

#define A
#define A 
#define A foo
#define A /*foo*/

#define m(a)
#define m(a) 
#define m(a,b)
#define m(a, b)
#define m( a , b )
#define m(foo, abcdefg, something, bar, buzzzz)
#define m(a) foo
#define m(a)foo

#define m(a)      a##foo
#define m(a) foo##a
#define m(a) foo##a##foo

#define m(a, b)        a    ##     b
#define m(a, b)        a    ##     b ## foo
#define m(a, b)        a ## foo ## b
#define m(a, b)        a ## foo ## b ## foo
#define m(a, b) foo ## a    ##     b
#define m(a, b) foo ## a    ##     b ## foo
#define m(a, b) foo ## a ## foo ## b
#define m(a, b) foo ## a ## foo ## b ## foo

#define m(a) #a

#define no_parameters_here (a)

#pragma message("test")
static int separator1;

#define A { f( 123 ); }
#define A(x) { f( x ); }
#define A(x) { f1( (x) + 1 ); f2( (x) + 2 ); f3( (x) + 3 ); }

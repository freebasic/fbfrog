#include "1"
#include "2"
#include "3"
#include "4"
#include "5"
#include "6"
#include "7"
#include "8"

#define A01 0
#define A02 1
#define A03 11
#define A04 &o1
#define A05 &o123
#define A06 &h0
#define A07 &h0
#define A08 &h1
#define A09 &hFF
#define A10 1
#define A11 1
#define A12 1
#define A13 0.1
#define A14 0
#define A15 1.123
#define A16 10
#define A17 10
#define A18 1
#define A19 1
#define A20 1
#define A21 1
#define A22 1

#define B01 2
#define B02 1

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

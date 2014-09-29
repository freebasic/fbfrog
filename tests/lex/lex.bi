#pragma once

#include once "crt/long.bi"

extern "C"

'' @fbfrog -whitespace -nonamefixup

#define A01 0
#define A02 1
#define A03 11
#define A04 &o0
#define A05 &o1
#define A06 &o123
#define A07 &h0
#define A08 &h0
#define A09 &h1
#define A10 &hFF
#define A11 1d
#define A12 1f
#define A13 1d
#define A14 0.1
#define A15 0d
#define A16 1.123
#define A17 10d
#define A18 10d
#define A19 1
#define A20 cast(clong, 1)
#define A21 cast(culong, 1)
#define A22 1ll
#define A23 1ull
#define A24 9d
#define A25 90d
#define A26 90d
#define A27 0.9
#define A28 90d
#define A29 90d
#define A30 0.9
#define A31 &h88

#define B1 "foo"
#define B2 wstr("foo")
#define B3 !"foo\n"
#define B4 """foo"""
#define B5 asc("a")
#define B6 asc(wstr("a"))

#define C01 """"
#define C02 "'"
#define C03 "'"
#define C04 "?"
#define C05 !"\\"
#define C06 !"\a"
#define C07 !"\b"
#define C08 !"\f"
#define C09 !"\n"
#define C10 !"\r"
#define C11 !"\t"
#define C12 !"\v"
#define C13 asc("'")
#define C14 asc("""")
#define C15 asc("""")
#define C16 asc("?")
#define C17 asc(!"\\")
#define C18 asc(!"\a")
#define C19 asc(!"\b")
#define C20 asc(!"\f")
#define C21 asc(!"\n")
#define C22 asc(!"\r")
#define C23 asc(!"\t")
#define C24 asc(!"\v")

#define D1 !"\0"
#define D2 !"\0"
#define D3 !"\0"
#define D4 !"\0000"
#define D5 !"\1"
#define D6 !"\1"
#define D7 !"\0001"

#define E1 !"\0a"
#define E2 !"\0a"
#define E3 !"\0a"
#define E4 !"\0g"
#define E5 !"\0g"
#define E6 !"\0g"

#define F1 !"\1"
#define F2 !"\t"
#define F3 "I"
#define F4 "I1"

#define G1 !"\0010"
#define G2 !"\0270"
#define G3 !"\2550"

#define H1 "a@b"
#define H2 "1@2"
#define H3 !"\0"
#define H4 !"\0"
#define H5 !"\255"
#define H6 !"\170"
#define H7 !"a\0g"

'' Even stray form feed characters

declare sub f()  '' Some escaped newlines 

'' single-line \
''comment \
''across \
''multiple \
''lines
'' escaped
''
''\
''newlines
''inside\
''multi-line\
''comment 

#define Z1 "abcdef"  '' escaped newline in string literal
#define Z2 "abcdef"
#define Z3 ""
#define Z4 ""

end extern

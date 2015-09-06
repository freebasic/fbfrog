#define A01 0
#define A02 1
#define A03 11
#define A04 00
#define A05 01
#define A06 0123
#define A07 0x
#define A08 0x0
#define A09 0x1
#define A10 0xFF
#define A11 1.0
#define A12 1.0f
#define A13 1.0d
#define A14 0.1
#define A15 0.0
#define A16 1.123
#define A17 1e+1
#define A18 1.0e+1
#define A19 1u
#define A20 1l
#define A21 1ul
#define A22 1ll
#define A23 1ull
#define A24 09.0
#define A25 09e1
#define A26 09e+1
#define A27 09e-1
#define A28 09E1
#define A29 09E+1
#define A30 09E-1
#define A31 0x88
#define A32 0XFF
#define A33 1lu
#define A34 1llu
#define A35 1i8
#define A36 1i16
#define A37 1i32
#define A38 1i64
#define A39 1ui8
#define A40 1ui16
#define A41 1ui32
#define A42 1ui64
#define A43 0b
#define A44 0b0
#define A45 0b1
#define A46 0b10100110
#define A47 0B0

#define B1 "foo"
#define B2 L"foo"
#define B3 "foo\n"
#define B4 "\"foo\""
#define B5 'a'
#define B6 L'a'

#define C01 "\""
#define C02 "\'"
#define C03 "'"
#define C04 "\?"
#define C05 "\\"
#define C06 "\a"
#define C07 "\b"
#define C08 "\f"
#define C09 "\n"
#define C10 "\r"
#define C11 "\t"
#define C12 "\v"
#define C13 '\''
#define C14 '\"'
#define C15 '"'
#define C16 '\?'
#define C17 '\\'
#define C18 '\a'
#define C19 '\b'
#define C20 '\f'
#define C21 '\n'
#define C22 '\r'
#define C23 '\t'
#define C24 '\v'

#define D1 "\0"
#define D2 "\00"
#define D3 "\000"
#define D4 "\0000"
#define D5 "\01"
#define D6 "\001"
#define D7 "\0001"

#define E1 "\0a"
#define E2 "\00a"
#define E3 "\000a"
#define E4 "\0g"
#define E5 "\00g"
#define E6 "\000g"

#define F1 "\1"
#define F2 "\11"
#define F3 "\111"
#define F4 "\1111"

#define G1 "\0010"
#define G2 "\0330"
#define G3 "\3770"

#define H1 "a\100b"
#define H2 "1\1002"
#define H3 "\x0"
#define H4 "\x0000000000000000000"
#define H5 "\xFF"
#define H6 "\x00000000000AA"
#define H7 "a\x00000000g"

/* foo */

// foo

/**
 * Comments like this
 */

/* /' */

/* '/'/' */

/*'*/

/*/*/

/*/'/*/

// Even stray form feed characters


/* Some escaped newlines */
\
\
void f(void); \
\
\

// single-line \
comment \
across \
multiple \
lines

/* escaped

\
newlines
inside\
multi-line\
comment */

// escaped newline in string literal
#define Z1 "abc\
def"
#define Z2 "abc\ 
def"
#define Z3 "\
"
#define Z4 "\ 
"

// behindspace must be set correctly where it matters...

// same as M1(x) -- function-like macro, escaped EOL not treated as space
#define M1\
(x) + 1

// same as M2 (x) -- object-like macro, space shouldn't be ignored
#define M2 \
(x) + 1

// object-like macro, comment treated as space
#define M3/**/(x) + 1

// object-like macro, comment treated as space
#define M4/*
*/(x) + 1

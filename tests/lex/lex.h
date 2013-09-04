#define A 0
#define A 1
#define A 11
#define A 01
#define A 0123
#define A 0x
#define A 0x0
#define A 0x1
#define A 0xFF
#define A 1.0
#define A 1.0f
#define A 1.0d
#define A 0.1
#define A 0.0
#define A 1.123
#define A 1e+1
#define A 1.0e+1
#define A 1u
#define A 1l
#define A 1ul
#define A 1ll
#define A 1ull

#define B "foo"
#define B L"foo"
#define B "foo\n"
#define B "\"foo\""
#define B 'a'
#define B L'a'

#define C "\""
#define C "\'"
#define C "'"
#define C "\?"
#define C "\\"
#define C "\a"
#define C "\b"
#define C "\f"
#define C "\n"
#define C "\r"
#define C "\t"
#define C "\v"

#define C '\''
#define C '\"'
#define C '"'
#define C '\?'
#define C '\\'
#define C '\a'
#define C '\b'
#define C '\f'
#define C '\n'
#define C '\r'
#define C '\t'
#define C '\v'

#define D "\0"
#define D "\00"
#define D "\000"
#define D "\0000"
#define D "\01"
#define D "\001"
#define D "\0001"

#define E "\0a"
#define E "\00a"
#define E "\000a"
#define E "\0g"
#define E "\00g"
#define E "\000g"

#define F "\1"
#define F "\11"
#define F "\111"
#define F "\1111"

#define G "\0010"
#define G "\0330"
#define G "\3770"

#define H "a\100b"
#define H "1\1002"
#define H "\x0"
#define H "\x0000000000000000000"
#define H "\xFF"
#define H "\x00000000000AA"
#define H "a\x00000000g"

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

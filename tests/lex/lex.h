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
#define D "\1"
#define D "\11"
#define D "\111"
#define D "\01"
#define D "\001"
#define D "a\100b"
#define D "1\1002"
#define D "\xFF"
#define D "\x0"
#define D "\x0000000000000000000"
#define D "\x00000000000AA"
#define D "a\x00000000g"

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

#define ADDROF(a) (&a)

#define A !x
#define A x != x
#define A x % x
#define A x %= x
#define A x & x
#define A &x /* ambigious -- AND vs. @, depends on where the define is used */
#define A x &= x
#define A x && x
#define A x++
#define A x--
#define A x << x
#define A x <<= x
#define A x = x
#define A x == x
#define A x >> x
#define A x >>= x
#define A x ? x : x
#define A x ^ x
#define A x ^= x
#define A x | x
#define A x |= x
#define A x || x
#define A ~x

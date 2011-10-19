#define ADDROF(a) ( /' TODO: best guess translation of & '/ @a)

#define A /' TODO: bitwise NOT, different precedence '/ not x
#define A x <> x
#define A x mod x
#define A x mod= x
#define A x /' TODO: best guess translation of & '/ and x
#define A /' TODO: best guess translation of & '/ and x /' ambigious -- AND vs. @, depends on where the define is used '/
#define A x and= x
#define A x andalso x
#define A x /' TODO: not supported in FB '/ ++
#define A x /' TODO: not supported in FB '/ --
#define A x shl x
#define A x shl= x
#define A x = x
#define A x = x
#define A x shr x
#define A x shr= x
#define A x /' TODO: iif() '/ ? x : x
#define A x xor x
#define A x xor= x
#define A x or x
#define A x or= x
#define A x orelse x
#define A /' TODO: bitwise NOT, different precedence '/ not x

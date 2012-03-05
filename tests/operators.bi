#define ADDROF(a) ( /' TODO: check whether & meant @ or AND '/ @a)

#define A /' TODO: add parentheses around NOT (different precedence) '/ not x
#define A x <> x
#define A x mod x
#define A x mod= x
#define A x /' TODO: check whether & meant @ or AND '/ and x
#define A /' TODO: check whether & meant @ or AND '/ and x /' ambigious -- AND vs. @, depends on where the define is used '/
#define A x and= x
#define A x andalso x
#define A x /' TODO: translate ++/-- '/ ++
#define A x /' TODO: translate ++/-- '/ --
#define A x shl x
#define A x shl= x
#define A x = x
#define A x = x
#define A x shr x
#define A x shr= x
#define A x /' TODO: turn a?b:c into iif(a,b,c) '/ ? x : x
#define A x xor x
#define A x xor= x
#define A x or x
#define A x or= x
#define A x orelse x
#define A /' TODO: add parentheses around NOT (different precedence) '/ not x

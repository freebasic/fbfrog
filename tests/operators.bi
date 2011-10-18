#define ADDROF(a) ( /'TODO: was &'/ @a)

#define A /'TODO: unary NOT'/ not x
#define A x <> x
#define A x mod x
#define A x mod= x
#define A x and x
#define A and x /' ambigious -- AND vs. @, depends on where the define is used '/
#define A x and= x
#define A x andalso x
#define A x /'TODO'/ ++
#define A x /'TODO'/ --
#define A x shl x
#define A x shl= x
#define A x = x
#define A x = x
#define A x shr x
#define A x shr= x
#define A x /'TODO: iif()'/ ? x : x
#define A x xor x
#define A x xor= x
#define A x or x
#define A x or= x
#define A x orelse x

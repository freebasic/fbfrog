// This is turned into a TODO
#define A(x) x+

// And in this #define body we actually reference A which we failed to parse
#define B(x) A(x)

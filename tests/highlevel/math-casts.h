enum E {
	ENUMCONST1 = 0,
};

// A culng() cast needs to be added around this BOP, to ensure the result is
// truncated to 32bit in FB 64bit
#define A58 (0u - 100u)
#define A59 (0u - 100)
#define A60 (0 - 100)
#define A61 (0 - 100u)

// Similar situation here - but the cast is already there, and we shouldn't add
// another one.
#define A62 ((unsigned int)(0u - 100u))

// Expression type depending on the type of an existing symbol
#define A70 (A58 - 1u)
#define A71 (A58 - 1)
#define A72 ((unsigned int) A58 - 1u)
#define A73 ((unsigned int) A58 - 1)
#define A74 (ENUMCONST1 - 1u)
#define A75 (ENUMCONST1 - 1)
#define A76 ((unsigned int)ENUMCONST1 - 1u)
#define A77 ((unsigned int)ENUMCONST1 - 1)
#define A78 (undefined - 1u)
#define A79 (undefined - 1)
#define A80 ((unsigned int)undefined - 1u)
#define A81 ((unsigned int)undefined - 1)
#define A82 ((int)undefined - 1u)
#define A83 ((int)undefined - 1)
#ifdef _WIN32
	#define A84 undefined
#else
	#define A84 (-1)
#endif
#define A85 A84

// Same but with forward references
#define B01 B00 // simple alias
#define B00 (0u - 100u)

#define B11 (B10 - 1) // "complex" math expression
#define B10 (0u - 100u)

#define B22 (B21 - 1) // extra indirection level
#define B21 (B20 - 1)
#define B20 (0u - 100u)

#define B31 (B30 - 1)
#define B32 (B31 - 1) // isn't a fwdref itself, but refers to one
#define B30 (0u - 100u)

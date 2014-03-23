// B shouldn't be typedeffed to A here; since A is a pointer to the struct it
// shouldn't be seen as alias for it. B should be used as the alias.
typedef struct { int i; } *A1, B1;
typedef struct { int i; } **A2, B2;
typedef struct { int i; } (*A3)(void), B3;

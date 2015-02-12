// @fbfrog -removedefine m

#define m void f(void);
m
#undef m

// The "(...)" following m2 is not an argument list
#define m foo
void m(void);
#undef m

// The "(...)" following the macro id isn't a parameter list, as it's separated
// by spaces
#define m (void)
void f m;
#undef m

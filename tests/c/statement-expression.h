// @fbfrog -v
// GCC's "({...})" statement-expressions can't be translated to FB
#define A1 ({ int i = foo(0); i; })

#define A2 { int a[({ f(); })]; }

// @fbfrog -filterout '*b.h'

// Even though b.h will be filtered out, fbfrog should still collect typedefs
// from it.

#define X1 (A)0
#define X2 (B)0

typedef int A;
#include "b.h"

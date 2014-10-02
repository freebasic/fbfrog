// These #defines use typedefs which aren't declared yet. fbfrog should still be
// able to detect it though because the typedefs are declared later.
#define X1 (A)0
#define X2 (B)0

typedef int A;
#include "b.h"

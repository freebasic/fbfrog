// These #defines use types which aren't declared yet. fbfrog should still be
// able to detect it though because they are declared later.
#define X1 (A)0
#define X2 (B)0
#define X3 (struct C)0

typedef int A;
#include "b.h"

struct C {
	int i;
};

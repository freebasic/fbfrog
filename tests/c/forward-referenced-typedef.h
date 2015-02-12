// Illegal C, but still the situation could happen in fbfrog's AST after
// highlevel transformations, and FB supports it...

extern T *p;
typedef int T;

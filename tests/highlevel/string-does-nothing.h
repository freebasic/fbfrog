// @fbfrog -string x

// Even though this is affected by -string, it shouldn't be turned into a zstring,
// because that would be invalid FB
extern signed char x;

typedef signed char x; // here it's ok though

extern signed char *x; // for comparison

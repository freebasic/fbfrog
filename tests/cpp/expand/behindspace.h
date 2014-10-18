// @fbfrog -removedefine mergetest

#define stringify1(s) #s
#define stringify2(s) stringify1(s)

#define mergetest(a, b) prefix a##b
static char *test1 = stringify1(mergetest(x, y));  // "mergetest(x, y)"
static char *test2 = stringify1(mergetest( x, y)); // "mergetest( x, y)"
static char *test3 = stringify2(mergetest(x, y));  // "prefix xy"
static char *test4 = stringify2(mergetest( x, y)); // "prefix xy"

#define TEST_MERGE x##y
static char *test5 = stringify2(TEST_MERGE);        // "xy"
static char *test6 = stringify2(prefix TEST_MERGE); // "prefix xy"
static char *test7 = stringify2(=TEST_MERGE);       // "=xy"
static char *test8 = stringify2(= TEST_MERGE);      // "= xy"

// Macro whose first token isn't behind space
#define m10()x
static char *test10 = stringify2(=m10());	// "=x"
static char *test11 = stringify2(= m10());	// "= x"

// Macro whose first token is a param, and it's behind space
#define m20(a) a
static char *test20 = stringify2(=m20(0));	// "=0"
static char *test21 = stringify2(= m20(0));	// "= 0"
static char *test22 = stringify2(=m20(=));	// "=="
static char *test23 = stringify2(= m20(=));	// "= ="

// Macro whose first token is a param, and it isn't behind space
#define m30(a)a
static char *test30 = stringify2(=m30(0));	// "=0"
static char *test31 = stringify2(= m30(0));	// "= 0"
static char *test32 = stringify2(=m30(=));	// "=="
static char *test33 = stringify2(= m30(=));	// "= ="

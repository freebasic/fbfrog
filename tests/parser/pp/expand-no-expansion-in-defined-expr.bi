'' "defined EXPANDME1" shouldn't be expanded to "defined 123", because no
'' expansion should be done for "defined id" expressions. "defined <number>" is

'' good for the test case because it's an invalid expression.
#define EXPANDME1 123

extern yes as long
extern separator1 as long

extern separator2 as long

extern yes as long
extern separator3 as long

extern yes as long
extern separator4 as long

extern separator5 as long

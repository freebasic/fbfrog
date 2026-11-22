#pragma once

extern "C"

' @fbfrog -removedefine B_is_defined
' "defined m1" shouldn't be expanded to "defined 123", because no
' expansion should be done for "defined id" expressions. "defined <number>" is
' good for the test case because it would trigger a syntax error.
const m1 = 123
extern yes as long
extern separator1 as long
extern separator2 as long
extern yes as long
extern separator3 as long
extern yes as long
extern separator4 as long
extern separator5 as long
' Here, the defined operator appears as the result of macro expansion. We
' should still not expand its operand, like gcc.
const B = 1 / 0
declare sub f()
extern separator6 as long
#undef B
' defined operator as macro call arg
#define some(x) x
const B = 1 / 0
declare sub f()
extern separator7 as long
#undef some
#undef B

end extern

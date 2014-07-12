#pragma once

extern "C"

extern     a as long
dim shared a as long
extern     b as long
dim shared b as long

'' TODO: unrecognized construct:
'' c
'' ---------------------------------------------------------------------------
'' expected identifier for the symbol declared in this declaration but found '[eof]'
''     c
''     ^
'' tests/c/id-basetype-missing-id-and-semicolon-in-macro.h(2): construct found here
''     #define m int a; int b; c
''                             ^

end extern

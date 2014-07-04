#pragma once

extern "C"

extern     f as long
dim shared f as long

#define m(a, b) a##b

'' TODO: unrecognized construct:
'' void;
'' ---------------------------------------------------------------------------
'' expected identifier for the symbol declared in this declaration but found ';'
''    void ;
''         ^
'' tests/c/decl-basetype-pasted-missing-id.h(4): construct found here
''    2: 
''    3: #define m(a,b) a##b
''    4: m(vo,id);
''               ^
''    5: 

end extern

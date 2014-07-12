#pragma once

extern "C"

extern     f as long
dim shared f as long

#define m(a, b) a##b

'' TODO: unrecognized construct:
'' void;
'' ---------------------------------------------------------------------------
'' expected identifier for the symbol declared in this declaration but found ';'
''     void ;
''          ^
'' tests/c/decl-basetype-pasted-missing-id.h(4): construct found here
''     m(vo,id);
''             ^

end extern

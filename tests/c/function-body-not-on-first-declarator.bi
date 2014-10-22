#pragma once

extern "C"

'' TODO: unrecognized construct:
'' void *a, b(void) { ; };
'' ---------------------------------------------------------------------------
'' tests/c/function-body-not-on-first-declarator.h(1): expected ';' to finish this declaration but found '{'
''     void *a, b(void) { ; };
''                      ^
'' context as seen by fbfrog:
''     void * a , b ( void ) { ; } ;
''                           ^

end extern

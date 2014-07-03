#pragma once

'' TODO: unrecognized construct:
'' void *a, b(void) { ; };
'' ---------------------------------------------------------------------------
'' tests/c/function-body-not-on-first-declarator.h(1): expected ';' to finish this declaration but found '{'
''    1: void *a, b(void) { ; };
''                        ^
''    2: 
'' context as seen by fbfrog:
''    void * a , b ( void ) { ; } ;
''                          ^

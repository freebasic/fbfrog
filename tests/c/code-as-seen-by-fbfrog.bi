#pragma once

'' TODO: unrecognized construct:
'' void f(int i;
'' ---------------------------------------------------------------------------
'' tests/c/code-as-seen-by-fbfrog.h(6): expected ')' to close parameter list in function declaration but found ';'
''    4: /* Some comment in the
''    5: middle of things.*/
''    6: int i; /* invalid code */
''            ^
''    7: 
'' context as seen by fbfrog:
''    void f ( int i ;
''                   ^

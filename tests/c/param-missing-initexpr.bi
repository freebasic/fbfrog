#pragma once

'' TODO: unrecognized construct:
'' void f(int i =);
'' ---------------------------------------------------------------------------
'' tests/c/param-missing-initexpr.h(1): expected an atomic expression
''    1: void f(int i =);
''                     ^
''    2: 
'' context as seen by fbfrog:
''    void f ( int i = ) ;
''                     ^

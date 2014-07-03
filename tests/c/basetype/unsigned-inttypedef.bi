#pragma once

type myint as long

'' TODO: unrecognized construct:
'' static unsigned myint i;
'' ---------------------------------------------------------------------------
'' tests/c/basetype/unsigned-inttypedef.h(2): expected ';' to finish this declaration but found 'i'
''    1: typedef int myint;
''    2: static unsigned myint i;
''                             ^
''    3: 
'' context as seen by fbfrog:
''    static unsigned myint i ;
''                          ^

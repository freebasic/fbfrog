#pragma once

type myint as long

'' TODO: unrecognized construct:
'' static short myint i;
'' ---------------------------------------------------------------------------
'' tests/c/basetype/short-inttypedef.h(2): expected ';' to finish this declaration but found 'i'
''    1: typedef int myint;
''    2: static short myint i;
''                          ^
''    3: 
'' context as seen by fbfrog:
''    static short myint i ;
''                       ^

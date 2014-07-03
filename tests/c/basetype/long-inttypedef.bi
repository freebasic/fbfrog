#pragma once

type myint as long

'' TODO: unrecognized construct:
'' static long myint i;
'' ---------------------------------------------------------------------------
'' tests/c/basetype/long-inttypedef.h(2): expected ';' to finish this declaration but found 'i'
''    1: typedef int myint;
''    2: static long myint i;
''                         ^
''    3: 
'' context as seen by fbfrog:
''    static long myint i ;
''                      ^

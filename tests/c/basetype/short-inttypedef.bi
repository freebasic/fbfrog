#pragma once

type myint as long

'' TODO: unrecognized construct:
'' static short myint i;
'' ---------------------------------------------------------------------------
'' tests/c/basetype/short-inttypedef.h(2): expected ';' to finish this declaration but found 'i'
''     static short myint i;
''                        ^
'' context as seen by fbfrog:
''     static short myint i ;
''                        ^

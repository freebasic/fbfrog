#pragma once

type myint as long

'' TODO: unrecognized construct:
'' static long myint i;
'' ---------------------------------------------------------------------------
'' tests/c/basetype/long-inttypedef.h(2): expected ';' to finish this declaration but found 'i'
''     static long myint i;
''                       ^
'' context as seen by fbfrog:
''     static long myint i ;
''                       ^

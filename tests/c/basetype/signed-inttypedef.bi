#pragma once

type myint as long

'' TODO: unrecognized construct:
'' static signed myint i;
'' ---------------------------------------------------------------------------
'' tests/c/basetype/signed-inttypedef.h(2): expected ';' to finish this declaration but found 'i'
''     static signed myint i;
''                         ^
'' context as seen by fbfrog:
''     static signed myint i ;
''                         ^

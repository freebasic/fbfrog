#pragma once

extern "C"

sub a()
end sub

'' TODO: unrecognized construct:
'' , *b;
'' ---------------------------------------------------------------------------
'' tests/c/function-body-not-on-last-declarator.h(1): expected a data type starting a declaration but found ','
''    1: void a(void) { ; }, *b;
''                         ^
''    2: 
'' context as seen by fbfrog:
''    void a ( void ) { ; } , * b ;
''                          ^

end extern

#pragma once

'' TODO: unrecognized construct:
'' static int i1 = 09;
'' ---------------------------------------------------------------------------
'' tests/lex/numlit-invalid-digit.h(1): invalid digit in octal number literal
''     static int i1 = 09;
''                     ^~
'' context as seen by fbfrog:
''     static int i1 = 09 ;
''                     ^~

'' TODO: unrecognized construct:
'' static int i2 = 09f;
'' ---------------------------------------------------------------------------
'' tests/lex/numlit-invalid-digit.h(2): invalid digit in octal number literal
''     static int i2 = 09f;
''                     ^~~
'' context as seen by fbfrog:
''     static int i2 = 09f ;
''                     ^~~

'' TODO: unrecognized construct:
'' static int i3 = 078;
'' ---------------------------------------------------------------------------
'' tests/lex/numlit-invalid-digit.h(3): invalid digit in octal number literal
''     static int i3 = 078;
''                     ^~~
'' context as seen by fbfrog:
''     static int i3 = 078 ;
''                     ^~~

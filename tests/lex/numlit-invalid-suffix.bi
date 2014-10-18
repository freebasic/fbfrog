#pragma once

'' TODO: unrecognized construct:
'' static int i1 = 1a;
'' ---------------------------------------------------------------------------
'' tests/lex/numlit-invalid-suffix.h(1): invalid suffix on number literal: 'a'
''     static int i1 = 1a;
''                     ^~
'' context as seen by fbfrog:
''     static int i1 = 1a ;
''                     ^~

'' TODO: unrecognized construct:
'' static int i2 = 1z;
'' ---------------------------------------------------------------------------
'' tests/lex/numlit-invalid-suffix.h(2): invalid suffix on number literal: 'z'
''     static int i2 = 1z;
''                     ^~
'' context as seen by fbfrog:
''     static int i2 = 1z ;
''                     ^~

'' TODO: unrecognized construct:
'' static int i3 = 1uu;
'' ---------------------------------------------------------------------------
'' tests/lex/numlit-invalid-suffix.h(3): invalid suffix on number literal: 'u'
''     static int i3 = 1uu;
''                     ^~~
'' context as seen by fbfrog:
''     static int i3 = 1uu ;
''                     ^~~

'' TODO: unrecognized construct:
'' static int i4 = 1ullu;
'' ---------------------------------------------------------------------------
'' tests/lex/numlit-invalid-suffix.h(4): invalid suffix on number literal: 'u'
''     static int i4 = 1ullu;
''                     ^~~~~
'' context as seen by fbfrog:
''     static int i4 = 1ullu ;
''                     ^~~~~

'' TODO: unrecognized construct:
'' static int i5 = 1lll;
'' ---------------------------------------------------------------------------
'' tests/lex/numlit-invalid-suffix.h(5): invalid suffix on number literal: 'l'
''     static int i5 = 1lll;
''                     ^~~~
'' context as seen by fbfrog:
''     static int i5 = 1lll ;
''                     ^~~~

'' TODO: unrecognized construct:
'' static int i6 = 1e+1e+2;
'' ---------------------------------------------------------------------------
'' tests/lex/numlit-invalid-suffix.h(6): invalid suffix on number literal: 'e+2'
''     static int i6 = 1e+1e+2;
''                     ^~~~~~~
'' context as seen by fbfrog:
''     static int i6 = 1e+1e+2 ;
''                     ^~~~~~~

'' TODO: unrecognized construct:
'' static int i7 = 1uf;
'' ---------------------------------------------------------------------------
'' tests/lex/numlit-invalid-suffix.h(7): invalid suffix on number literal: 'f'
''     static int i7 = 1uf;
''                     ^~~
'' context as seen by fbfrog:
''     static int i7 = 1uf ;
''                     ^~~

'' TODO: unrecognized construct:
'' static int i8 = 1i31;
'' ---------------------------------------------------------------------------
'' tests/lex/numlit-invalid-suffix.h(8): invalid suffix on number literal: 'i31'
''     static int i8 = 1i31;
''                     ^~~~
'' context as seen by fbfrog:
''     static int i8 = 1i31 ;
''                     ^~~~

'' TODO: unrecognized construct:
'' static int i9 = 1i64u;
'' ---------------------------------------------------------------------------
'' tests/lex/numlit-invalid-suffix.h(9): invalid suffix on number literal: 'u'
''     static int i9 = 1i64u;
''                     ^~~~~
'' context as seen by fbfrog:
''     static int i9 = 1i64u ;
''                     ^~~~~

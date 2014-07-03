#pragma once

#define m(param) param

'' TODO: unrecognized construct:
'' typedef struct a ## b;
'' ---------------------------------------------------------------------------
'' expected identifier for the symbol declared in this declaration
''    typedef struct a ## b ;
''                     ^~
'' tests/cpp/expand/merge/from-macro-arg.h(6): construct found here
''    4: //    typedef struct ab;
''    5: #define m(param) param
''    6: typedef struct m(a ## b);
''       ^~~~~~~
''    7: 
'' tests/cpp/expand/merge/from-macro-arg.h(6): token found here
''    4: //    typedef struct ab;
''    5: #define m(param) param
''    6: typedef struct m(a ## b);
''                          ^~
''    7: 

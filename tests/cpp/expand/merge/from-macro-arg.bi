#pragma once

#define m(param) param

'' TODO: unrecognized construct:
'' typedef struct a ## b;
'' ---------------------------------------------------------------------------
'' expected identifier for the symbol declared in this declaration but found '##'
''     typedef struct a ## b ;
''                      ^~
'' tests/cpp/expand/merge/from-macro-arg.h(6): construct found here
''     typedef struct m(a ## b);
''     ^~~~~~~
'' tests/cpp/expand/merge/from-macro-arg.h(6): token found here
''     typedef struct m(a ## b);
''                        ^~

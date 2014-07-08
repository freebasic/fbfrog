#pragma once

extern "C"

'' TODO: unrecognized construct:
'' #define m void f(void
'' ---------------------------------------------------------------------------
'' tests/c/macro-body-unknown-construct.h(1): expected expression but found 'void'
''    1: #define m void f(void
''                 ^~~~
''    2: 
''    3: void ok(void);
'' context as seen by fbfrog:
''    # define m void f ( void 
''               ^~~~

declare sub ok()

'' TODO: unrecognized construct:
'' #define A void f(
'' ---------------------------------------------------------------------------
'' tests/c/macro-body-unknown-construct.h(5): expected expression but found 'void'
''    3: void ok(void);
''    4: 
''    5: #define A void f(
''                 ^~~~
''    6: #define B() void f(
''    7: #define C(a) void f(
'' context as seen by fbfrog:
''    # define A void f ( 
''               ^~~~

'' TODO: unrecognized construct:
'' #define B() void f(
'' ---------------------------------------------------------------------------
'' tests/c/macro-body-unknown-construct.h(6): expected expression but found 'void'
''    4: 
''    5: #define A void f(
''    6: #define B() void f(
''                   ^~~~
''    7: #define C(a) void f(
''    8: #define D(a, b, c) void f(
'' context as seen by fbfrog:
''    # define B ( ) void f ( 
''                   ^~~~

'' TODO: unrecognized construct:
'' #define C(a) void f(
'' ---------------------------------------------------------------------------
'' tests/c/macro-body-unknown-construct.h(7): expected expression but found 'void'
''    5: #define A void f(
''    6: #define B() void f(
''    7: #define C(a) void f(
''                    ^~~~
''    8: #define D(a, b, c) void f(
''    9: 
'' context as seen by fbfrog:
''    # define C ( a ) void f ( 
''                     ^~~~

'' TODO: unrecognized construct:
'' #define D(a, b, c) void f(
'' ---------------------------------------------------------------------------
'' tests/c/macro-body-unknown-construct.h(8): expected expression but found 'void'
''    6: #define B() void f(
''    7: #define C(a) void f(
''    8: #define D(a, b, c) void f(
''                          ^~~~
''    9: 
'' context as seen by fbfrog:
''    # define D ( a , b , c ) void f ( 
''                             ^~~~

end extern

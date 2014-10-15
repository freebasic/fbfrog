#pragma once

extern "C"

'' TODO: unrecognized construct:
'' #define m void f(void
'' ---------------------------------------------------------------------------
'' tests/c/macro-body-unknown-construct.h(1): failed to parse full #define body
''     #define m void f(void
''                    ^
'' context as seen by fbfrog:
''     # define m void f ( void 
''                     ^

declare sub ok()

'' TODO: unrecognized construct:
'' #define A void f(
'' ---------------------------------------------------------------------------
'' tests/c/macro-body-unknown-construct.h(5): failed to parse full #define body
''     #define A void f(
''                    ^
'' context as seen by fbfrog:
''     # define A void f ( 
''                     ^

'' TODO: unrecognized construct:
'' #define B() void f(
'' ---------------------------------------------------------------------------
'' tests/c/macro-body-unknown-construct.h(6): failed to parse full #define body
''     #define B() void f(
''                      ^
'' context as seen by fbfrog:
''     # define B ( ) void f ( 
''                         ^

'' TODO: unrecognized construct:
'' #define C(a) void f(
'' ---------------------------------------------------------------------------
'' tests/c/macro-body-unknown-construct.h(7): failed to parse full #define body
''     #define C(a) void f(
''                       ^
'' context as seen by fbfrog:
''     # define C ( a ) void f ( 
''                           ^

'' TODO: unrecognized construct:
'' #define D(a, b, c) void f(
'' ---------------------------------------------------------------------------
'' tests/c/macro-body-unknown-construct.h(8): failed to parse full #define body
''     #define D(a, b, c) void f(
''                             ^
'' context as seen by fbfrog:
''     # define D ( a , b , c ) void f ( 
''                                   ^

end extern

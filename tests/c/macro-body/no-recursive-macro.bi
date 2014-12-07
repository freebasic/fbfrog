#pragma once

'' The following symbols have been renamed:
''     #define A => A_
''     #define B => B_

extern "C"

declare sub A(byval x as long)

#define A_(x) A(x)
#define b 1
#define B_ 2
#define C B_

end extern

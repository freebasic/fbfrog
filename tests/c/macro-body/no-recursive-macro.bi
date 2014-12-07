#pragma once

'' The following symbols have been renamed:
''     #define A => A_

extern "C"

declare sub A(byval x as long)

#define A_(x) A(x)

end extern

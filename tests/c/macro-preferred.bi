#pragma once

'' The following symbols have been renamed:
''     #define A => A_

extern "C"

declare sub A(byval i as long)

#define A_(i) A(i + 1)

dim shared i as long = A_(0 + 1)

end extern

#pragma once

extern "C"

declare sub A(byval x as long)

#define A(x) A(x)
#define b 1
#define B 2
#define C B

end extern

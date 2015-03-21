#pragma once

extern "C"

declare sub A(byval i as long)
#define A(i) A(i + 1)
dim shared i as long = A(0 + 1)

end extern

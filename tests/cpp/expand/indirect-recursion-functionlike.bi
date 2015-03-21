#pragma once

extern "C"

#define m1(x) m2(x)
#define m2(x) m1(x)
declare sub m1()
declare sub m2()

end extern

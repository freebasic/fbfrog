#pragma once

extern "C"

' Indirect recursion with params
#define m1(x) m2(x)
#define m2(x) m1(x)
declare sub m1() ' -> m2(void) -> m1(void)
declare sub m2() ' -> m1(void) -> m2(void)

end extern

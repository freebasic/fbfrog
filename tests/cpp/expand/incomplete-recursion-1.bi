#pragma once

extern "C"

' Incomplete recursive macro call, not expanded
#define m1(x) m1
extern     m1 as any
dim shared m1 as any
' void m1(void);

end extern

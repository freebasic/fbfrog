#pragma once

extern "C"

' @fbfrog -removedefine m2
' Incomplete recursive macro call, not expanded
#define m1(x) m1
declare sub m1()
' void m1 (void);

end extern

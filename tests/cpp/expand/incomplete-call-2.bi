#pragma once

extern "C"

' @fbfrog -removedefine m3
#define m1 m2
#define m2(x) x
' m2: incomplete macro call, not expanded
declare sub m2()
' void m2 m3;
' void m2 (void);

end extern

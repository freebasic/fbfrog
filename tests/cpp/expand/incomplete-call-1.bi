#pragma once

extern "C"

' @fbfrog -removedefine m1 -removedefine m2
' m1: incomplete macro call, not expanded
declare sub m1()
' void m1 (void);

end extern

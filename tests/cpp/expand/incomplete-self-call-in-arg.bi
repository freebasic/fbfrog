#pragma once

extern "C"

' @fbfrog -removedefine m1
' Incomplete self call in arg (not expanded in arg because it's incomplete,
' not expanded in body later because it's a recursive call)
declare sub m1()
' void m1(void);

end extern

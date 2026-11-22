#pragma once

extern "C"

' @fbfrog -removedefine m
declare sub f()
' The "(...)" following m2 is not an argument list
declare sub foo()
' The "(...)" following the macro id isn't a parameter list, as it's separated
' by spaces
declare sub f()

end extern

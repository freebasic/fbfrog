#pragma once

extern "C"

' @fbfrog -string x
' Even though this is affected by -string, it shouldn't be turned into a zstring,
' because that would be invalid FB
extern x as byte
type x as zstring ' here it's ok though
extern x as zstring ptr ' for comparison

end extern

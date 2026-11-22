#pragma once

extern "C"

' @fbfrog -removedefine m1 -removedefine m2
' Other macro called in arg, arg sometimes used with # and ##
declare sub f1(byval pm2 as zstring ptr = "m2")
declare sub f2(byval m2p as zstring ptr = "m2")
declare sub f3(byval pm2x as zstring ptr = "m2")
declare sub f1(byval pm2 as zstring ptr = "m2")
declare sub f2(byval m2p as zstring ptr = "m2")
declare sub f3(byval pm2x as zstring ptr = "m2")

end extern

#pragma once

extern "C"

declare function f11(byval i as long) as function() as long
declare function f12(byval i as long) as function() as long
declare function f13(byval i as long) as function() as long
declare function x1N(byval i as long) as function() as long
declare function f21() as function(byval as single, byval as single) as single
declare function f22() as function(byval as single, byval as single) as single
declare function f23() as function(byval as single, byval as single) as single
declare function x2N() as function(byval as single, byval as single) as single
declare function f31 stdcall() as sub()
declare function x3N stdcall() as sub()
declare function f41() as sub stdcall()
declare function x4N() as sub stdcall()

end extern

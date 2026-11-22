#pragma once

extern "C"

' vararg functions are always cdecl; the __stdcall attribute should be ignored here
declare sub f(byval n as long, ...)

end extern

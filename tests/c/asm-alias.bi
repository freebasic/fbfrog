#pragma once

'' The following symbols have been renamed:
''     variable x1 => x2

extern "C"

' @fbfrog -rename x1 x2
extern a alias "b" as long
extern a alias "b" as long
extern a alias "b" as long
declare sub f1 alias "f2"()
extern x2 alias "x1" as long

end extern

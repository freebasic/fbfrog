#pragma once

'' The following symbols have been renamed:
''     variable b => a
''     variable b => a
''     variable b => a
''     procedure f2 => f1
''     variable x1 => x2

extern "C"

extern a alias "b" as long
extern a alias "b" as long
extern a alias "b" as long
declare sub f1 alias "f2"()
extern x2 alias "x1" as long

end extern

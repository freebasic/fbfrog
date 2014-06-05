#pragma once

extern "C"

'' @fbfrog -whitespace -nonamefixup
'' "defined m1" shouldn't be expanded to "defined 123", because no
'' expansion should be done for "defined id" expressions. "defined <number>" is

const m1 = 123

extern yes as long
extern separator1 as long

extern separator2 as long

extern yes as long
extern separator3 as long

extern yes as long
extern separator4 as long

extern separator5 as long

end extern

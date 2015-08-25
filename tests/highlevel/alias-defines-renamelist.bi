#pragma once

'' The following symbols have been renamed:
''     procedure F1 => F2
''     variable EV1 => EV2
''     variable FOO => BAR

extern "C"

const C1 = 123
const C2 = C1
type T1 as long
type T2 as T1
declare sub F1()
declare sub F2 alias "F1"()
extern EV1 as long
extern EV2 alias "EV1" as long
extern BAR alias "FOO" as long

end extern

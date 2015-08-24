#pragma once

'' The following symbols have been renamed:
''     struct A1 => A2
''     typedef B1 => B2
''     constant C1 => C2
''     #define D1 => D2
''     procedure E1 => E2

extern "C"

type A2
	i as long
end type

type B2 as long
const C2 = 123
#define D2(x) x
declare function E2 alias "E1"(byval as long) as long
type Ref1 as A2
type Ref2 as B2
const Ref3 = C2
extern Ref4 as A2
extern Ref5 as B2
#define Ref6 D2(1)
#define Ref7 E2(1)

end extern

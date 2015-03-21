#pragma once

'' The following symbols have been renamed:
''     typedef A1 => B1
''     typedef A2 => B2
''     typedef byt => byte
''     typedef T1 => T2
''     struct A3 => B3

extern "C"

type a as long
type A as long
type a1 as long
type B1 as long
type a2 as long
type B2 as long

extern x1 as a
extern x2 as A
extern x3 as a1
extern x4 as B1
extern x5 as a2
extern x6 as B2
type byte as byte
type T2 as long
declare sub T2()

type B3
	i as long
end type

end extern

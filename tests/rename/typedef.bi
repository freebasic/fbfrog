#pragma once

'' The following symbols have been renamed:
''     typedef A1 => B1
''     typedef A2 => B2
''     typedef byt => byte
''     typedef T1 => T2
''     struct A3 => B3

extern "C"

' @fbfrog -renametypedef A1 B1 -renametypedef A2 B2 -renametypedef byt byte -renametypedef T1 T2 -renametypedef A3 B3
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
' Rename to FB keyword
type byte as byte
' Rename to conflict with other symbol
type T2 as long
declare sub T2()
' The A3 typedef will be removed, and the UDT will take on its name.
' This process should carry over the renaming information, so that the UDT
' appears in renamelists instead of the typedef.

type B3
	i as long
end type

end extern

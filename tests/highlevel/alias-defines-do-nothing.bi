#pragma once

extern "C"

' Sometimes we don't know which declaration an alias define refers to.
' In such cases it must not be turned into a declaration.

type A
	i as long
end type

declare sub A()
#define A_ A

type B
	i as long
end type

#define B_ B
declare sub B()
#define C_ C

type C
	i as long
end type

declare sub C()
dim shared SV1 as long
#define SV2 SV1

end extern

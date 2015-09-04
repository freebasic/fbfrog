#pragma once

extern "C"

extern s1 as zstring * 10
extern i1(0 to 15 - 1) as long

type UDT1
	i as long
	s2 as zstring * 20
end type

type UDT2
	i as long
	i2(0 to 25 - 1) as long
end type

declare sub f1(byval s3 as zstring ptr)
declare sub f2(byval i3 as long ptr)
extern s4 as const zstring * len("foo")
extern array10x20(0 to 10 - 1, 0 to 19) as long

end extern

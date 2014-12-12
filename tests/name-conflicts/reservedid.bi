#pragma once

'' The following symbols have been renamed:
''     #define A => A_
''     procedure f1 => f1_
''     inside struct UDT:
''         field foo => foo_

extern "C"

#define A_ 123

declare sub f1_ alias "f1"()

type UDT
	foo_ as long
end type

end extern

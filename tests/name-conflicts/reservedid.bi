#pragma once

'' The following symbols have been renamed:
''     inside struct UDT:
''         field foo => foo_
''     #define A => A_
''     procedure f1 => f1_

extern "C"

#define A_ 123

declare sub f1_ alias "f1"()

type UDT
	foo_ as long
end type

end extern

#pragma once

extern "C"

'' The following symbols have been renamed:
''     #define INT_ alias "INT"
''     constant AND_ alias "AND"
''     constant OR_ alias "OR"
''     constant XOR_ alias "XOR"
''     struct DOUBLE_ alias "DOUBLE"

#define INT_(x) x

enum
	AND_ = 0
	OR_ = AND_
	XOR_ = 1 + OR_
end enum

extern     array1(0 to 0) as long
dim shared array1(0 to 0) as long

#define A1 INT_(1)

type DOUBLE_
	a as DOUBLE_ ptr
	b as DOUBLE_ ptr
end type

declare sub f1(byval as DOUBLE_)
declare sub f2(byval as DOUBLE_)

end extern

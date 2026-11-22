#pragma once

extern "C"

/' Block comment '/
' Line comment
const FOO = 1 ' trailing define
extern     x as long
dim shared x as long /' trailing block '/

type S
	a as long ' field a
	b as long /' field b '/
end type

enum E
	A ' A value
	B /' B value '/
end enum

end extern

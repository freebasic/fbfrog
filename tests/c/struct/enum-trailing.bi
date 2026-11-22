#pragma once

' Trailing comments on the final (comma-less) enumerator

enum E1
	A1
	B1 = 2 ' trailing on last
end enum

enum E2
	C1
	D1 = 4 /' trailing block comment on last '/
end enum

enum E3
	E3A
	E3B = 7
end enum

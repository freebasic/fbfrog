#pragma once

type UDT2
	a as long
end type

type UDT1
	' gcc: warning: declaration does not declare anything
end type

' sizeof(UDT1) = 0, UDT2 does not become a field

type UDT4
	a as long
end type

type UDT3
	' The "const" has no effect
	' gcc: warning: declaration does not declare anything
end type

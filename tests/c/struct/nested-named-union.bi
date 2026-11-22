#pragma once

union UDT2
	a as long
end union

type UDT1
	' gcc: warning: declaration does not declare anything
end type

' sizeof(UDT1) = 0, UDT2 does not become a field

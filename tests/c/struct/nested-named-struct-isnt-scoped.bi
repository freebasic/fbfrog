#pragma once

type UDT2
	a as long
end type

type UDT1
end type

' UDT2 is visible here, compiles fine with gcc
dim shared x as UDT2

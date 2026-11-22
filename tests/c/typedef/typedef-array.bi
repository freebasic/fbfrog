#pragma once

extern "C"

dim shared array1(0 to 9) as long
dim shared array2(0 to 9) as long
dim shared expectedarray1(0 to 9) as long

type UDT1
	arrayfield1(0 to 9) as long
	expectedarrayfield1(0 to 9) as long
end type

union UDT2
	arrayfield1(0 to 9) as long
	expectedarrayfield1(0 to 9) as long

	type
		arrayfield2(0 to 9) as long
		expectedarrayfield2(0 to 9) as long
		arrayfield3(0 to 9) as long
		expectedarrayfield3(0 to 9) as long
	end type
end union

' param becoming an array param due to the typedef being solved out
declare sub f1(byval param as long ptr)
declare sub expectedf2(byval param as long ptr)
' Typedef's dimension must be merged with existing dimension
dim shared array3(0 to 19, 0 to 9) as long
dim shared expectedarray3(0 to 19, 0 to 9) as long
' 2 blocks with 5 elements each
' 1 block of TABLE2x5
dim shared table1x2x5(0 to 0, 0 to 1, 0 to 4) as ubyte
' 2 blocks of TABLE2x5
dim shared table2x2x5(0 to 1, 0 to 1, 0 to 4) as ubyte
' 10 blocks, each with 20 blocks of TABLE2x5
dim shared table10x20x2x5(0 to 9, 0 to 19, 0 to 1, 0 to 4) as ubyte
extern constarray(0 to 9) as const long
declare sub f2(byval constarrayparam as const long ptr)
' pointer to array
declare sub f3(byval p as long ptr)
declare sub f4(byval p as const long ptr)

end extern

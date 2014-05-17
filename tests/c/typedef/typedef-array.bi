extern "C"

dim shared array1(0 to 10 - 1) as long
dim shared array2(0 to 10 - 1) as long
dim shared expectedarray1(0 to 10 - 1) as long

type UDT1
	arrayfield1(0 to 10 - 1) as long
	expectedarrayfield1(0 to 10 - 1) as long
end type

union UDT2
	arrayfield1(0 to 10 - 1) as long
	expectedarrayfield1(0 to 10 - 1) as long

	type
		arrayfield2(0 to 10 - 1) as long
		expectedarrayfield2(0 to 10 - 1) as long
		arrayfield3(0 to 10 - 1) as long
		expectedarrayfield3(0 to 10 - 1) as long
	end type
end union

declare sub f1(byval param as long ptr)
declare sub expectedf2(byval param as long ptr)

dim shared array3(0 to 20 - 1, 0 to 10 - 1) as long
dim shared expectedarray3(0 to 20 - 1, 0 to 10 - 1) as long
dim shared table1x2x5(0 to 1 - 1, 0 to 2 - 1, 0 to 5 - 1) as ubyte
dim shared table2x2x5(0 to 2 - 1, 0 to 2 - 1, 0 to 5 - 1) as ubyte
dim shared table10x20x2x5(0 to 10 - 1, 0 to 20 - 1, 0 to 2 - 1, 0 to 5 - 1) as ubyte

end extern

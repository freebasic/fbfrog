#pragma once

extern "C"

extern array(0 to 9) as long
extern arrayOfPtrs(0 to 9) as long ptr
extern pArray(0 to 9) as long ptr
extern ppArray(0 to 9) as long ptr ptr
extern pArrayOfPtrs(0 to 9) as long ptr ptr
extern ppArrayOfPtrs(0 to 9) as long ptr ptr ptr

type UDT
	bitfield : 1 as long
	bitfieldOfPtr : 1 as long ptr
	pBitfield : 1 as long ptr
	ppBitfield : 1 as long ptr ptr
end type

end extern

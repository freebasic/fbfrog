#pragma once

extern "C"

extern array(0 to 9) as long
extern arrayOfPtrs(0 to 9) as long ptr
extern pArray as long ptr
extern ppArray as long ptr ptr
extern pArrayOfPtrs as long ptr ptr
extern ppArrayOfPtrs as long ptr ptr ptr

type UDT
	bitfield : 1 as long
	bitfieldOfPtr : 1 as long ptr
	'' TODO: int (*pBitfield) : 1;
	'' TODO: int (**ppBitfield) : 1;
end type

end extern

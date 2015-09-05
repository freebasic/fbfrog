extern int array[10];
extern int *arrayOfPtrs[10];
extern int (*pArray)[10];
extern int (**ppArray)[10];
extern int *(*pArrayOfPtrs)[10];
extern int *(**ppArrayOfPtrs)[10];

struct UDT {
	int bitfield : 1;
	int *bitfieldOfPtr : 1;
	int (*pBitfield) : 1;
	int (**ppBitfield) : 1;
};

typedef int T[10];

static T array1;
static T array2;

struct UDT1 {
	T arrayfield1;
};

union UDT2 {
	T arrayfield1;
	struct {
		T arrayfield2;
		T arrayfield3;
	};
};

// param becoming an array param due to the typedef being solved out
void f1(T param);

// Typedef's dimension must be merged with existing dimension
static T array3[20];

// 2 blocks with 5 elements each
typedef unsigned char TABLE2x5[2][5];

// 1 block of TABLE2x5
static TABLE2x5 table1x2x5[1];

// 2 blocks of TABLE2x5
static TABLE2x5 table2x2x5[2];

// 10 blocks, each with 20 blocks of TABLE2x5
static TABLE2x5 table10x20x2x5[10][20];

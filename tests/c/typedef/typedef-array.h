typedef int T[10];

static T array1;
static T array2;
static int expectedarray1[10];

struct UDT1 {
	T arrayfield1;
	int expectedarrayfield1[10];
};

union UDT2 {
	T arrayfield1;
	int expectedarrayfield1[10];
	struct {
		T arrayfield2;
		int expectedarrayfield2[10];
		T arrayfield3;
		int expectedarrayfield3[10];
	};
};

// param becoming an array param due to the typedef being solved out
void f1(T param);
void expectedf2(int param[10]);

// Typedef's dimension must be merged with existing dimension
static T array3[20];
static int expectedarray3[20][10];

// 2 blocks with 5 elements each
typedef unsigned char TABLE2x5[2][5];

// 1 block of TABLE2x5
static TABLE2x5 table1x2x5[1];

// 2 blocks of TABLE2x5
static TABLE2x5 table2x2x5[2];

// 10 blocks, each with 20 blocks of TABLE2x5
static TABLE2x5 table10x20x2x5[10][20];

extern const T constarray;
void f2(const T constarrayparam);

// pointer to array
void f3(T *p);
void f4(T const *p);

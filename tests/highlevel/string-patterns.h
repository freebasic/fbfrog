// @fbfrog -string 'string*' -nostring 'byteArray*' -string UDT1.name -string UDT*.my*Path -string f1.param1

extern signed char string1[10];
extern signed char string2[10];
extern signed char string3[10];
extern char byteArray1[10];
extern char byteArray2[10];
extern char byteArray3[10];

struct UDT1 {
	signed char name[10]; // should be matched
	signed char forComparison[10];
	signed char myPath[10]; // should be matched
};

struct UDT2 {
	signed char name[10]; // shouldn't be matched
	signed char myFullPath[10]; // should be matched
};

extern signed char myPartialPath[10]; // shouldn't be matched

void f1(unsigned char *param1 /* should be matched */);
void f2(unsigned char *param1 /* shouldn't be matched */);

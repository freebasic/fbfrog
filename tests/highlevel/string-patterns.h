// @fbfrog -string 'string*' -nostring 'byteArray*' -string UDT1.name -string UDT*.my*Path -string f1.param1 -string f3.0 -string f5.1 -string PFDoodle.1 -string procPtrParam.1 -string f7*

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

void f3(unsigned char *); // should be matched
void f4(unsigned char *); // shouldn't be matched

void f5(unsigned char *, unsigned char *, unsigned char *); // matching 2nd parameter only

typedef void (*PFDoodle)(unsigned char *, unsigned char *); // match 2nd parameter of procptr subtype of a typedef
void f6(int, void (*procPtrParam)(unsigned char *, unsigned char *), int); // same, but as param instead of typedef

void f7(unsigned char *param /* shouldn't be matched */);

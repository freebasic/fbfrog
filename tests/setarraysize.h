// @fbfrog -setarraysize s1 10 -setarraysize s2 20 -setarraysize s3 30 -setarraysize i1 15 -setarraysize i2 25 -setarraysize i3 35 -setarraysize s4 'len("foo")' -setarraysize array10x20 10
extern char s1[];
extern int i1[];

struct UDT1 {
	int i;
	char s2[];
};

struct UDT2 {
	int i;
	int i2[];
};

void f1(char s3[]);
void f2(int i3[]);

extern const char s4[];

extern int array10x20[][20];

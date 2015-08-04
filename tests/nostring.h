// @fbfrog -nostring field2 -nostring var2 -nostring p2 -nostring CHAR

extern char var1[100];
extern char var2[100];

struct UDT {
	char field1[100];
	char field2[100];
};

extern char *p1;
extern char *p2;

typedef char CHAR;
extern CHAR *p3;
void f1(CHAR i, CHAR *p);

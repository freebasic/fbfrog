// @fbfrog -string s1 -string p1 -string p4 -string p5 -string s3

typedef unsigned char myubyte;
extern myubyte s1[10];
extern myubyte s2[10];

void f1(signed char *p1, unsigned char *p2);
void f2(signed char *p3, unsigned char *p4);
void f3(myubyte *p5);

extern signed char s3[10];

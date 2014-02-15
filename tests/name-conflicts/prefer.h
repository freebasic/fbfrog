// parameter < #define
#define I1 1
void fi1(int i1);
void fi2(int i2);
#define I2 1

// #define < proc
#define I3 1
void i3(void);
void i4(void);
#define I4 1

// constant < proc
enum { I5 };
void i5(void);
void i6(void);
enum { I6 };

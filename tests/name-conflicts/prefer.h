// parameter < #define
#define I1() 1
void fi1(int i1);
void fi2(int i2);
#define I2() 1

// #define < proc
#define I3() 1
void i3(void);
void i4(void);
#define I4() 1

// enumconst < proc
enum { I5 };
void i5(void);
void i6(void);
enum { I6 };

// constant < proc
#define I7 1
void i7(void);
void i8(void);
#define I8 1

typedef int T1(int i);
T1 f11;
T1 f12;
T1 f13;
int x1N(int i);

// Calling convention must be preserved
typedef __attribute__((stdcall)) void T2(void);
T2 f2;
__attribute__((stdcall)) void x2(void);

// Multiple parameters
typedef void T3(int a, short b, float c, double);
T3 f3;
void x3(int a, short b, float c, double);

// Typedefs should be processed recursively
typedef T1 T4;
T4 f4;
int x4(int i);

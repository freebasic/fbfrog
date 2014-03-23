typedef int (*T1(int i))(void);
T1 f11;
T1 f12;
T1 f13;
int (*x1N(int i))(void);

typedef float (*T2(void))(float, float);
T2 f21;
T2 f22;
T2 f23;
float (*x2N(void))(float, float);

typedef __attribute__((stdcall)) void (*T3(void))(void);
T3 f31;
__attribute__((stdcall)) void (*x3N(void))(void);

typedef void (__attribute__((stdcall)) *T4(void))(void);
T4 f41;
void (__attribute__((stdcall)) *x4N(void))(void);

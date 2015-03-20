// @fbfrog -renametypedef A1 B1 -renametypedef A2 B2 -renametypedef byt byte -renametypedef T1 T2 -renametypedef A3 B3

typedef int a;
typedef int A;
typedef int a1;
typedef int A1;
typedef int a2;
typedef int A2;

extern a x1;
extern A x2;
extern a1 x3;
extern A1 x4;
extern a2 x5;
extern A2 x6;

// Rename to FB keyword
typedef signed char byt;

// Rename to conflict with other symbol
typedef int T1;
void T2(void);

// The A3 typedef will be removed, and the UDT will take on its name.
// This process should carry over the renaming information, so that the UDT
// appears in renamelists instead of the typedef.
typedef struct { int i; } A3;

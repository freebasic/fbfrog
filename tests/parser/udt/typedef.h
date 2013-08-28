typedef UDT A;
typedef struct UDT A;
typedef int A;
typedef int *A;
typedef int A, B, C;
typedef void (*A)(void);
typedef UDT (*A)(UDT);
typedef UDT **A, B, (*C)(int);

typedef A A;
static int separator1;

typedef struct A A;
static int separator2;

signed a;
unsigned a;

int a;
signed int a;
unsigned int a;

long a;
signed long a;
unsigned long a;

long int a;
signed long int a;
unsigned long int a;

long long a;
signed long long a;
unsigned long long a;

long long int a;
signed long long int a;
unsigned long long int a;

float a;
double a;

__int8 a;
__int32 a;
__int64 a;

 int8_t a;
uint8_t a;
 int32_t a;
uint32_t a;
 int64_t a;
uint64_t a;

A a;
enum A a;
union A a;
struct A a;

// Pointers

int *a;
int ****a;
int *a, **b;

// Split up based on different ptrcount
int a, *b, **c;
int **a, *b, c;
int a, *b, c;
int *a, b, *c;
int *a, *b, c;
int a, *b, *c;

// CONSTness
const int a;
int const a;
const int const a;
int * const a;
int const * const a;
const int const * const * * * const a;

// Split up based on different ptr const mask
int *const a, *b, ***const*c;

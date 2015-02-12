static   signed a;
static unsigned a;

static          char a;
static   signed char a;
static unsigned char a;

static          short a;
static   signed short a;
static unsigned short a;

static          short int a;
static   signed short int a;
static unsigned short int a;

static          int a;
static   signed int a;
static unsigned int a;

static          long a;
static   signed long a;
static unsigned long a;

static          long int a;
static   signed long int a;
static unsigned long int a;

static          long long a;
static   signed long long a;
static unsigned long long a;

static          long long int a;
static   signed long long int a;
static unsigned long long int a;

static float a;
static double a;
static long double a;
static double long a;

static __int8 a;
static __int16 a;
static __int32 a;
static __int64 a;

static  int8_t a;
static uint8_t a;
static  int16_t a;
static uint16_t a;
static  int32_t a;
static uint32_t a;
static  int64_t a;
static uint64_t a;

static size_t a;
static ssize_t a;
static ptrdiff_t a;
static intptr_t a;
static uintptr_t a;

static wchar_t a;
static wchar_t *a;

static _Bool a;

static A a;
static enum A a;
static union A a;
static struct A a;

static char *a;
static const char *a;
static char const unsigned *a;
static int ****a;
static int *a, **b, c, ****d;
static int short signed a;
static long const int long unsigned a;
static int *const a, *b, ***const*c;
static const int a;
static UDT *x;

static       int       *       *       a; // int ptr ptr
static       int       *       * const a; // int ptr const ptr
static       int       * const *       a; // int const ptr ptr
static       int       * const * const a; // int const ptr const ptr
static       int const *       *       a; // const int ptr ptr
static       int const *       * const a; // const int ptr const ptr
static       int const * const *       a; // const int const ptr ptr
static       int const * const * const a; // const int const ptr const ptr
static const int       *       *       a; // const int ptr ptr
static const int       *       * const a; // const int ptr const ptr
static const int       * const *       a; // const int const ptr ptr
static const int       * const * const a; // const int const ptr const ptr
static const int const *       *       a; // const int ptr ptr
static const int const *       * const a; // const int ptr const ptr
static const int const * const *       a; // const int const ptr ptr
static const int const * const * const a; // const int const ptr const ptr

static int * restrict p;
static int * __restrict p;
static int * __restrict__ p;
static int * * restrict p;
static int * restrict * p;
static int * restrict * restrict p;
static int * const restrict * const p;
static int * restrict const * const p;

static jmp_buf x;
static jmp_buf *px;
static const jmp_buf cx;
static const jmp_buf *pcx;
void f(jmp_buf x);
void f(jmp_buf *px);
void f(const jmp_buf cx);
void f(const jmp_buf *pcx);

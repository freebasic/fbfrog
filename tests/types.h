static   signed a;
static unsigned a;

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

static __int8 a;
static __int32 a;
static __int64 a;

static  int8_t a;
static uint8_t a;
static  int32_t a;
static uint32_t a;
static  int64_t a;
static uint64_t a;

static A a;
static enum A a;
static union A a;
static struct A a;

// Pointers
static int *a;
static int ****a;
static int *a, **b;

static int   a,   b,   c;
static int   a,   b,  *c;
static int   a,   b, **c;
static int   a,  *b,   c;
static int   a,  *b,  *c;
static int   a,  *b, **c;
static int   a, **b,   c;
static int   a, **b,  *c;
static int   a, **b, **c;
static int  *a,   b,   c;
static int  *a,   b,  *c;
static int  *a,   b, **c;
static int  *a,  *b,   c;
static int  *a,  *b,  *c;
static int  *a,  *b, **c;
static int  *a, **b,   c;
static int  *a, **b,  *c;
static int  *a, **b, **c;
static int **a,   b,   c;
static int **a,   b,  *c;
static int **a,   b, **c;
static int **a,  *b,   c;
static int **a,  *b,  *c;
static int **a,  *b, **c;
static int **a, **b,   c;
static int **a, **b,  *c;
static int **a, **b, **c;

static int *const a, *b, ***const*c;

// CONSTness

static       int               a;
static       int const         a;
static const int               a;
static const int const         a;

static       int       *       a;
static       int       * const a;
static       int const *       a;
static       int const * const a;
static const int       *       a;
static const int       * const a;
static const int const *       a;
static const int const * const a;

static       int       *       *       a;
static       int       *       * const a;
static       int       * const *       a;
static       int       * const * const a;
static       int const *       *       a;
static       int const *       * const a;
static       int const * const *       a;
static       int const * const * const a;
static const int       *       *       a;
static const int       *       * const a;
static const int       * const *       a;
static const int       * const * const a;
static const int const *       *       a;
static const int const *       * const a;
static const int const * const *       a;
static const int const * const * const a;

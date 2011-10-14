int a;

int *p;

int a, b;

int *p, ***p, x;

/* sub ptr */
void (*p)();

/* function pointer, with anonymous function pointer param */
double ***(*p)(int ***(*)(char***));

int a, b, c;
int *a, b, c;
int **a, b, c;

int a, *b, c;
int *a, *b, c;
int **a,  *b, c;

int a, **b, c;
int *a, **b, c;
int **a, **b, c;

int a, b, *c;
int *a, b, *c;
int **a, b, *c;

int a, *b, *c;
int *a, *b, *c;
int **a, *b, *c;

int a, **b, *c;
int *a, **b, *c;
int **a, **b, *c;

int a, b, **c;
int *a, b, **c;
int **a, b, **c;

int a, *b, **c;
int *a, *b, **c;
int **a, *b, **c;

int a, **b, **c;
int *a, **b, **c;
int **a, **b, **c;

static int a;

extern int a;

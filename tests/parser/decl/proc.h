static int __storage_spec_on_procs;
static void f(void);
extern void f(void);

static int __nested_id;
void (f)(void);
void ((f))(void);
void (((f)))(void);

static int __nested_declarator;
void (f(void));
void ((f(void)));
short *(f(void));
short *(*f(void));
short (*(*f(void)));

static int __result_types;
void f(void);
int f(void);
int *f(void);
UDT f(void);
UDT **f(void);

static int __result_procptr;
void (*f(void))(void);
int (*f(float, float))(double, double);
void (*(*f(short int a))(short int b))(short int c);

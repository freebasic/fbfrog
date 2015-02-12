static int __simple;
void f(int a);
void f(int a, int b);
void f(int a, int b, int c);
void f(int *a, int ***b);

static int __anonymous;
void f(int);
void f(int *, int, int ***, int);

static int __initializers;
void f(int i = 123);
void f(int a, int b = 123);
void f(int a = 123, int b);
void f(int a, int b = 123, int c);
void f(int a = 1, int b = 2, int c = 3);
void f(void (*p)(int) = 0);
void f(void (*p)(int i = 123) = 0);

static int __arrays;
void f(int i[5]);
void f(int a, int b[20]);
void f(int a[20], int b);
void f(int a, int b[20], int c);
void f(int a[1], int b[2], int c[3]);
void f(int a[]);

static int __nested_id;
void f(int (a));
void f(int ((a)), int (b));
void f(int *(a), int *(*((*b))), int (*c));

static int __anonymous_nested_id;
void f(int *(*((*))), int (*));

static int __no_params;
void f();
void f(void);

static int __procptr_params;
void f(void (*a)(void));
void f(void (* )(void));
void f(void (*a)(void (*b)(void)));
void f(void (* )(void (* )(void)));

static int __vararg;
void f(int a, ...);

static int __functions;
void f( void () );
void f( void (()) );
void f( void ((())) );
void f( void (void) );
void f( void ((void)) );
void f( void (((void))) );
void f( void f(void) );

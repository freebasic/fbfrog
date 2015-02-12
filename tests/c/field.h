struct UDT {
	signed int i;
	unsigned long long int j;
	unsigned k;
	double a,b,c;

	int **a, *a, a;
	int a, **a, **a;
	int *a, a, a, **a, **a, a;

	struct T ****y;
	struct T *a, ****a, a;

	int *(*p)(int*);
	int f(int, int);
	void proc(void);

	int a;
#if 1
	int b;
#endif
	int c;

	int a : 1;
	int a : 3;
	int a : 27;
	int a : 1 + 5 * 4;
	int a : 1, b : 1;

	int a;
	int **a;
	int a, b;
	int  *a, b, *c, ***d;
	int a[20];
	int a[2][3];
	void (*p[40])(void);
	void (*p[2][3])(void);

	void f(void);
	int f(void);
	void f();
	UDT **f(void);
	void f(int *a, int ***b);

	void (*a)(void);
	int (*a)(int);
	int (*a)(int a), (*b)(int a), c;
	int a, (*b)(int a), c, (*d)(int a);
	int **(*a)(int);

	void f(void (*a)(void));
	void f(void (*)(void));

	void (*a)(void (*a)(void));
	int ***(*p)(int ***(*)(int ***));
};

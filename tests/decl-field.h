struct T {
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

	int a;
#if 1
	int b;
#endif
	int c;
};

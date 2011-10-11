struct T {
	int *(*p)(int*);
};

void f(void (*foo)());

/* sub ptr */
void (*fp01)();

/* function pointer, with function pointer param */
double *(*fp02)(int ***(*)(char**));

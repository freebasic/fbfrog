void f(int (a));
void f(int ((a)), int (b));
void f(int *(a), int *(*((*b))), int (*c));

void f(int ());
void f(int (()), int ());
void f(int *(), int *(*((*))), int (*));

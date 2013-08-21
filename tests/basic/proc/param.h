void f(int a);
void f(int a, int b);
void f(int a, int b, int c);
void f(int *a, int ***b);

void f(int);
void f(int *, int, int ***, int);

void f(int i = 123);
void f(int a, int b = 123);
void f(int a = 123, int b);
void f(int a, int b = 123, int c);
void f(int a = 1, int b = 2, int c = 3);

void f(int i[5]);
void f(int a, int b[20]);
void f(int a[20], int b);
void f(int a, int b[20], int c);
void f(int a[1], int b[2], int c[3]);

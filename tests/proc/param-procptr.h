void f(void (*a)(void));
void f(void (* )(void));

void f(void (*a)(void (*b)(void)));
void f(void (* )(void (* )(void)));

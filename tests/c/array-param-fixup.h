// @fbfrog -whitespace -nonamefixup

void f(int i[]);
extern void (*p)(int i[]);
void f(void (*p)(int i[]));

struct UDT {
	void f(int i[]);
	void (*p)(int i[]);
};

#define A (void (*)(int i[]))0
